# s6_cress_ICC_compare_bag_or_seedling_level_ANOVA.r
#
# Pipeline step 6: quantify pseudoreplication. For each analysis group
# (ALL / AS / JZ) and each response variable, compute the intraclass
# correlation (ICC) of seedlings within bags, then run two ANOVAs side
# by side -- one on bag means (correct) and one on raw seedlings (wrong)
# -- so the inflation of significance from ignoring within-bag clustering
# is visible in the printed p-values.
#
# Why: the bag is the experimental unit (potency was applied per bag,
# ~16 seeds per bag). s5 already runs the bag-level ANOVA; this script
# documents *how bad* the seed-level shortcut would have been by showing
# ICC, design effect (DEFF), effective N, and the p-value ratio between
# the two analyses.
#
# Design: one-way random-effects ICC via ICC::ICCbare with bag_id as the
# cluster. ANOVAs are Type-III SS with contr.sum so the interaction is
# coded consistently with s5. Three analysis groups: ALL (1-10),
# AS (1-5), JZ (6-10).
#
# Inputs : either the s3 v1v2 combined file (raw) or an s4 skewness-
#          corrected file, depending on USE_SKEWNESS_CORRECTED. Either way
#          the file carries in_v1_analysis / in_v2_analysis flags written
#          by s3 (preserved through s4); rows are filtered by DATASET_VER
#          using those flags, exactly like s5. "v2" is the hybrid stream
#          (v2_remeasured for ASPS 1-5, v1_original for ASPS 6-10) and is
#          the default analysis stream.
# Outputs: a single PNG of seedling-level distribution histograms
#          (4 vars x AS/JZ panels) into outputs/<run_dir>/. ICC and ANOVA
#          comparison results are printed to the console only -- this
#          script is diagnostic, not a results-producing step.


library(readxl)
library(car)        # Anova(type="III")
library(here)
library(dplyr)
library(tidyr)
library(ICC)
library(ggplot2)
library(gridExtra)


#===== CONFIG ===============================================================

SCRIPT_TAG     <- "s6"
DATASET_VER    <- "v2"     # "v1" | "v2" | "v1v2"  -- matches s5 semantics
USE_SKEWNESS_CORRECTED <- TRUE  # TRUE -> read latest s4 output for DATASET_VER
RUN_DATE       <- format(Sys.Date(), "%Y%m%d")

# Response variables analysed. When USE_SKEWNESS_CORRECTED is TRUE we swap
# the matching T<var>_cut* truncated column over the raw column below, so
# the rest of the script (ICC, ANOVAs, histograms) operates on the same
# truncated values s5 uses for the skew-corrected ANOVA.
response_vars <- c("sprout_length", "root_length",
                   "seedling_length", "root_sprout_ratio")

# Inputs.
INPUT_S3_BASENAME <- "cress_length_ASPS_1-10_alldata_decoded_v1v2.xlsx"
INPUT_S3_PARENT   <- "cress_combine_files"  # holds <date>_cress_combined/
S4_OUTPUTS_ROOT   <- "outputs"               # holds <date>_s4_skewness_<ver>/


#===== DERIVED PATHS (don't edit) ===========================================

SCRIPT_PURPOSE <- paste0("icc_",
                         if (USE_SKEWNESS_CORRECTED) "skewcorr" else "raw")

out_dir <- file.path(
  "outputs",
  paste(RUN_DATE, SCRIPT_TAG, SCRIPT_PURPOSE, DATASET_VER, sep = "_")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_path <- function(suffix, ext) {
  file.path(
    out_dir,
    paste0(RUN_DATE, "_", SCRIPT_TAG, "_", DATASET_VER, "_", suffix, ".", ext)
  )
}


#===== RESOLVE INPUT ========================================================

# Two cases, picked by USE_SKEWNESS_CORRECTED:
#   FALSE -> raw s3 file: most-recent <date>_cress_combined/ under
#            cress_combine_files/.
#   TRUE  -> skewness-corrected: most-recent <date>_s4_skewness_<DATASET_VER>/
#            under outputs/, picking its *_skewness_corr.xlsx.
# Both helpers are copied verbatim from s5 so the two scripts stay in sync.
resolve_s3 <- function() {
  combined_root <- here(INPUT_S3_PARENT)
  candidates <- sort(
    list.dirs(combined_root, full.names = FALSE, recursive = FALSE),
    decreasing = TRUE
  )
  candidates <- candidates[grepl("_cress_combined$", candidates)]
  for (d in candidates) {
    p <- file.path(combined_root, d, INPUT_S3_BASENAME)
    if (file.exists(p)) return(p)
  }
  stop("No ", INPUT_S3_BASENAME, " found under ", combined_root,
       "/*_cress_combined/. Run s3_cress_combine_files_v2.r first.")
}

resolve_s4 <- function(version) {
  outputs_root <- here(S4_OUTPUTS_ROOT)
  if (!dir.exists(outputs_root)) {
    stop("USE_SKEWNESS_CORRECTED=TRUE but ", outputs_root,
         " does not exist. Run s4 first.")
  }
  pattern <- paste0("^[0-9]{8}_s4_skewness_", version, "$")
  candidates <- sort(
    list.dirs(outputs_root, full.names = FALSE, recursive = FALSE),
    decreasing = TRUE
  )
  candidates <- candidates[grepl(pattern, candidates)]
  for (d in candidates) {
    files <- list.files(file.path(outputs_root, d),
                        pattern = "_skewness_corr\\.xlsx$",
                        full.names = TRUE)
    if (length(files) > 0) return(files[1])
  }
  stop("No s4 skewness_corr.xlsx found for DATASET_VER=", version,
       ". Run s4 with that DATASET_VER first.")
}

input_path <- if (USE_SKEWNESS_CORRECTED) {
  resolve_s4(DATASET_VER)
} else {
  resolve_s3()
}


#===== READ DATA ============================================================

cat("\n", strrep("=", 80), "\n", sep = "")
cat("READING DATA\n")
cat(strrep("=", 80), "\n", sep = "")
cat("Input         : ", input_path, "\n", sep = "")
cat("Dataset ver   : ", DATASET_VER, "\n", sep = "")
cat("Skewness corr : ", USE_SKEWNESS_CORRECTED, "\n", sep = "")
cat("Output folder : ", out_dir, "\n\n", sep = "")

df_raw <- read_excel(input_path, sheet = "Sheet 1")

# Filter by dataset version using the membership flags written by s3
# (and preserved through s4). "v2" is the hybrid stream (v2_remeasured for
# ASPS 1-5, v1_original for ASPS 6-10); "v1" is the original measurements
# only; "v1v2" keeps every row so ASPS 1-5 appear twice -- comparison
# view, not an analysis input.
df_raw <- switch(DATASET_VER,
  "v1"   = df_raw[df_raw$in_v1_analysis, ],
  "v2"   = df_raw[df_raw$in_v2_analysis, ],
  "v1v2" = df_raw,
  stop("DATASET_VER must be 'v1', 'v2', or 'v1v2'; got: ", DATASET_VER)
)
cat("Total seed-level rows after filter: ", nrow(df_raw), "\n", sep = "")


#===== SWAP TO TRUNCATED COLUMNS WHEN USING SKEWNESS-CORRECTED INPUT ========

# s4 appends T<var>_cut<value> truncated columns rather than overwriting
# the originals. Copy the truncated column back over the original name for
# every response_var that has a truncated counterpart so the rest of the
# script (ICC, ANOVA, histograms) operates on the same values s5 used.
# Variables without a truncated column (e.g. root_sprout_ratio) flow
# through unchanged. Mirrors the equivalent block in s5.
if (USE_SKEWNESS_CORRECTED) {
  for (var in response_vars) {
    trunc_cols <- grep(paste0("^T", var, "_cut"),
                       colnames(df_raw), value = TRUE)
    if (length(trunc_cols) >= 1) {
      df_raw[[var]] <- df_raw[[trunc_cols[1]]]
      cat("  swapped ", var, " <- ", trunc_cols[1], "\n", sep = "")
    }
  }
}


#===== PARSE EXPERIMENT AND POTENCY =========================================

# exp_no is e.g. "3_A": <experiment_number>_<potency_code>. Split that
# out, tag rows AS / JZ by experimenter for the by-operator analysis,
# and build a unique bag_id used as the ICC cluster variable.

df_seedlings <- df_raw %>%
  separate(exp_no, into = c("experiment_number", "potency_code"), 
           sep = "_", remove = FALSE) %>%
  mutate(
    experiment_number = as.integer(experiment_number),
    bag = as.integer(bag),
    experimenter = ifelse(experiment_number <= 5, "AS", "JZ"),
    bag_id = paste(experiment_number, potency_code, bag, sep = "_")
  )

cat("Parsed structure:\n")
cat("  Experiments:", paste(sort(unique(df_seedlings$experiment_number)), collapse = ", "), "\n")
cat("  Total seedlings:", nrow(df_seedlings), "\n")
cat("  Total bags:", length(unique(df_seedlings$bag_id)), "\n")


#===== CALCULATE BAG-LEVEL MEANS ============================================

# Average per bag so the bag-level ANOVA below treats the bag as the
# experimental unit (correct), while the seedling-level ANOVA reuses
# df_seedlings (incorrect, by design -- that's the comparison we want).
df_bags <- df_seedlings %>%
  group_by(experiment_number, experimenter, potency_code, potency, bag, bag_id, exp_no, label) %>%
  summarise(
    n_seeds = n(),
    sprout_length = mean(sprout_length, na.rm = TRUE),
    root_length = mean(root_length, na.rm = TRUE),
    seedling_length = mean(seedling_length, na.rm = TRUE),
    root_sprout_ratio = mean(root_sprout_ratio, na.rm = TRUE),
    .groups = "drop"
  )

cat("  Bag-level dataset:", nrow(df_bags), "bags\n")
cat("  Mean seeds per bag:", round(mean(df_bags$n_seeds), 1), "\n")


#===== FACTORISE FOR ANOVA ==================================================

df_seedlings$experiment_number <- as.factor(df_seedlings$experiment_number)
df_seedlings$potency <- as.factor(df_seedlings$potency)
df_seedlings$experimenter <- as.factor(df_seedlings$experimenter)
df_seedlings$bag_id <- as.factor(df_seedlings$bag_id)

df_bags$experiment_number <- as.factor(df_bags$experiment_number)
df_bags$potency <- as.factor(df_bags$potency)
df_bags$experimenter <- as.factor(df_bags$experimenter)


#===== ANALYSIS GROUPS ======================================================

# ALL splits the data by experimenter to expose any operator-level
# difference (AS ran ASPS 1-5, JZ ran ASPS 6-10). Each group carries both
# its bag-level and seedling-level dataset so the loops below can hit them
# without re-filtering.
analysis_groups <- list(
  ALL = list(
    name = "ALL DATA (ASPS 1-10)",
    data_seedlings = df_seedlings,
    data_bags = df_bags
  ),
  AS = list(
    name = "AS ONLY (ASPS 1-5)",
    data_seedlings = df_seedlings %>% filter(experimenter == "AS"),
    data_bags = df_bags %>% filter(experimenter == "AS")
  ),
  JZ = list(
    name = "JZ ONLY (ASPS 6-10)",
    data_seedlings = df_seedlings %>% filter(experimenter == "JZ"),
    data_bags = df_bags %>% filter(experimenter == "JZ")
  )
)


#===== INTRACLASS CORRELATION COEFFICIENT (ICC) =============================

# ICC = between-bag variance / total variance, computed via ICC::ICCbare
# under a one-way random-effects model with bag_id as the cluster. The
# design effect DEFF = 1 + (avg cluster size - 1) * ICC tells us how much
# the standard errors are inflated when we (wrongly) treat seedlings as
# independent; effective N = actual N / DEFF gives the equivalent
# independent-sample size. DEFF > 2 is a strong pseudoreplication signal.
cat("\n\n")
cat(strrep("#", 80), "\n")
cat("INTRACLASS CORRELATION COEFFICIENT (ICC) ANALYSIS\n")
cat(strrep("#", 80), "\n")
cat("\n")
cat("ICC measures the proportion of total variance due to clustering within bags.\n")
cat("ICC = 0: Seedlings are completely independent (no clustering effect)\n")
cat("ICC = 1: Seedlings within a bag are identical (complete clustering)\n")
cat("\n")
cat("Design Effect (DEFF) = 1 + (avg cluster size - 1) × ICC\n")
cat("DEFF shows how much clustering inflates standard errors.\n")
cat("DEFF > 2 indicates serious pseudoreplication issues.\n")
cat("\n")
cat(strrep("#", 80), "\n")

for (group_key in names(analysis_groups)) {
  
  group <- analysis_groups[[group_key]]
  df_subset <- group$data_seedlings
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat(group$name, "\n")
  cat(strrep("=", 80), "\n")
  
  cat(sprintf("\nN seedlings: %d\n", nrow(df_subset)))
  cat(sprintf("N bags: %d\n", length(unique(df_subset$bag_id))))
  
  avg_cluster_size <- nrow(df_subset) / length(unique(df_subset$bag_id))
  cat(sprintf("Average seedlings per bag: %.1f\n", avg_cluster_size))
  
  for (var in response_vars) {
    
    cat("\n")
    cat("--- ", toupper(var), " ---\n", sep = "")
    
    # Prepare data for ICC calculation
    # ICC package needs: y = response, x = cluster ID
    icc_data <- df_subset %>%
      select(bag_id, !!sym(var)) %>%
      filter(!is.na(!!sym(var))) %>%
      mutate(
        bag_id_char = as.character(bag_id),
        response_value = as.numeric(!!sym(var))
      )
    
    # Debug: Check data structure
    cat(sprintf("  Data check: %d observations, %d unique bags\n", 
                nrow(icc_data), length(unique(icc_data$bag_id_char))))
    cat(sprintf("  Response range: %.3f to %.3f\n", 
                min(icc_data$response_value, na.rm = TRUE),
                max(icc_data$response_value, na.rm = TRUE)))
    
    # Calculate ICC using one-way random effects model
    tryCatch({
      icc_result <- ICCbare(x = bag_id_char, y = response_value, data = icc_data)
      
      cat(sprintf("  ICC = %.4f\n", icc_result))
      
      # Calculate design effect
      deff <- 1 + (avg_cluster_size - 1) * icc_result
      cat(sprintf("  Design Effect (DEFF) = %.2f\n", deff))
      
      # Interpretation
      cat("  Interpretation: ")
      if (icc_result < 0.05) {
        cat("Very weak clustering - seedlings nearly independent\n")
      } else if (icc_result < 0.15) {
        cat("Weak clustering - moderate within-bag correlation\n")
      } else if (icc_result < 0.30) {
        cat("Moderate clustering - substantial within-bag correlation\n")
      } else {
        cat("Strong clustering - seedlings within bags are very similar\n")
      }
      
      # Effective sample size
      effective_n <- nrow(icc_data) / deff
      cat(sprintf("  Effective sample size: %.0f (vs actual %d seedlings)\n", 
                  effective_n, nrow(icc_data)))
      cat(sprintf("  Information loss: %.1f%%\n", 
                  (1 - effective_n / nrow(icc_data)) * 100))
      
    }, error = function(e) {
      cat(sprintf("  ERROR calculating ICC: %s\n", e$message))
    })
  }
}


#===== ANOVA COMPARISON: BAG-LEVEL vs SEEDLING-LEVEL ========================

# Same Type-III ANOVA on the same formula run twice -- once on bag means
# (correct), once on raw seedlings (pseudoreplicated). The seed-level
# p-values should be smaller because the inflated N pretends we have more
# independent information than we do; the printed ratio quantifies that
# inflation. contr.sum is set globally so the interaction term is testable
# independently of cell coding (matches s5).
cat("\n\n")
cat(strrep("#", 80), "\n")
cat("ANOVA COMPARISON: BAG-LEVEL vs SEEDLING-LEVEL\n")
cat(strrep("#", 80), "\n")
cat("\n")
cat("This comparison shows how treating non-independent observations as independent\n")
cat("affects statistical inference (Type I error inflation).\n")
cat("\n")
cat("EXPECTED PATTERN:\n")
cat("- Seedling-level analysis: Smaller p-values (inflated significance)\n")
cat("- Bag-level analysis: Larger p-values (correct inference)\n")
cat(strrep("#", 80), "\n")

options(contrasts = c("contr.sum", "contr.poly"))

for (group_key in names(analysis_groups)) {
  
  group <- analysis_groups[[group_key]]
  df_seedlings_subset <- group$data_seedlings
  df_bags_subset <- group$data_bags
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat(group$name, "\n")
  cat(strrep("=", 80), "\n")
  
  for (var in response_vars) {
    
    cat("\n")
    cat("--- ", toupper(var), " ---\n", sep = "")
    cat("\n")
    
    # Bag-level ANOVA
    cat("BAG-LEVEL ANALYSIS (Correct - bags as experimental units):\n")
    cat(sprintf("  N = %d bags\n", nrow(df_bags_subset)))
    
    formula_str <- paste(var, "~ potency * experiment_number")
    model_bags <- aov(as.formula(formula_str), data = df_bags_subset)
    anova_bags <- Anova(model_bags, type = "III")
    
    p_pot_bags <- anova_bags["potency", "Pr(>F)"]
    p_exp_bags <- anova_bags["experiment_number", "Pr(>F)"]
    p_int_bags <- anova_bags["potency:experiment_number", "Pr(>F)"]
    
    cat(sprintf("  Potency effect:       p = %.6f  %s\n", 
                p_pot_bags, 
                ifelse(p_pot_bags < 0.001, "***",
                       ifelse(p_pot_bags < 0.01, "**",
                              ifelse(p_pot_bags < 0.05, "*",
                                     ifelse(p_pot_bags < 0.10, ".", "ns"))))))
    cat(sprintf("  Experiment effect:    p = %.6f  %s\n", 
                p_exp_bags,
                ifelse(p_exp_bags < 0.001, "***",
                       ifelse(p_exp_bags < 0.01, "**",
                              ifelse(p_exp_bags < 0.05, "*",
                                     ifelse(p_exp_bags < 0.10, ".", "ns"))))))
    cat(sprintf("  Interaction:          p = %.6f  %s\n", 
                p_int_bags,
                ifelse(p_int_bags < 0.001, "***",
                       ifelse(p_int_bags < 0.01, "**",
                              ifelse(p_int_bags < 0.05, "*",
                                     ifelse(p_int_bags < 0.10, ".", "ns"))))))
    
    cat("\n")
    
    # Seedling-level ANOVA
    cat("SEEDLING-LEVEL ANALYSIS (PSEUDOREPLICATION - treats seedlings as independent):\n")
    cat(sprintf("  N = %d seedlings\n", nrow(df_seedlings_subset)))
    cat("  WARNING: This analysis ignores within-bag clustering!\n")
    
    model_seedlings <- aov(as.formula(formula_str), data = df_seedlings_subset)
    anova_seedlings <- Anova(model_seedlings, type = "III")
    
    p_pot_seedlings <- anova_seedlings["potency", "Pr(>F)"]
    p_exp_seedlings <- anova_seedlings["experiment_number", "Pr(>F)"]
    p_int_seedlings <- anova_seedlings["potency:experiment_number", "Pr(>F)"]
    
    cat(sprintf("  Potency effect:       p = %.6f  %s\n", 
                p_pot_seedlings,
                ifelse(p_pot_seedlings < 0.001, "***",
                       ifelse(p_pot_seedlings < 0.01, "**",
                              ifelse(p_pot_seedlings < 0.05, "*",
                                     ifelse(p_pot_seedlings < 0.10, ".", "ns"))))))
    cat(sprintf("  Experiment effect:    p = %.6f  %s\n", 
                p_exp_seedlings,
                ifelse(p_exp_seedlings < 0.001, "***",
                       ifelse(p_exp_seedlings < 0.01, "**",
                              ifelse(p_exp_seedlings < 0.05, "*",
                                     ifelse(p_exp_seedlings < 0.10, ".", "ns"))))))
    cat(sprintf("  Interaction:          p = %.6f  %s\n", 
                p_int_seedlings,
                ifelse(p_int_seedlings < 0.001, "***",
                       ifelse(p_int_seedlings < 0.01, "**",
                              ifelse(p_int_seedlings < 0.05, "*",
                                     ifelse(p_int_seedlings < 0.10, ".", "ns"))))))
    
    cat("\n")
    cat("COMPARISON (Ratio of p-values: Bag / Seedling):\n")
    cat(sprintf("  Potency:       %.2f× (seedling p-value is %.0f%% smaller)\n",
                p_pot_bags / p_pot_seedlings,
                (1 - p_pot_seedlings / p_pot_bags) * 100))
    cat(sprintf("  Experiment:    %.2f× (seedling p-value is %.0f%% smaller)\n",
                p_exp_bags / p_exp_seedlings,
                (1 - p_exp_seedlings / p_exp_bags) * 100))
    cat(sprintf("  Interaction:   %.2f× (seedling p-value is %.0f%% smaller)\n",
                p_int_bags / p_int_seedlings,
                (1 - p_int_seedlings / p_int_bags) * 100))
    
    # Flag terms that flip from non-significant (bag-level, correct) to
    # significant (seedling-level, pseudoreplicated). These are the
    # textbook pseudoreplication false positives that motivate s5
    # running on bag means.
    if (p_pot_seedlings < 0.05 && p_pot_bags >= 0.05) {
      cat("\n  ALERT: Potency effect significant at seedling-level but NOT at bag-level.\n")
      cat("         Likely false positive due to pseudoreplication.\n")
    }
    if (p_int_seedlings < 0.05 && p_int_bags >= 0.05) {
      cat("\n  ALERT: Interaction significant at seedling-level but NOT at bag-level.\n")
      cat("         Likely false positive due to pseudoreplication.\n")
    }
  }
}


#===== DISTRIBUTION HISTOGRAMS ==============================================

# 4 response variables x 2 experimenters = 8 panels arranged in a 4x2
# grid. Each panel shows the seedling-level distribution with a normal
# overlay (red) and the mean (dashed) so deviations from normality and
# operator-level shifts are visible at a glance. Saved as a single PNG
# alongside the run's outputs/.
cat("\n\n")
cat(strrep("#", 80), "\n")
cat("CREATING DISTRIBUTION HISTOGRAMS\n")
cat(strrep("#", 80), "\n\n")

histogram_plots <- list()
plot_counter <- 1

for (var in response_vars) {
  
  # AS histogram (left panel)
  df_as_seedlings <- df_seedlings %>% filter(experimenter == "AS")
  
  as_mean <- mean(df_as_seedlings[[var]], na.rm = TRUE)
  as_sd <- sd(df_as_seedlings[[var]], na.rm = TRUE)
  as_n <- sum(!is.na(df_as_seedlings[[var]]))
  
  p_as <- ggplot(df_as_seedlings, aes(x = !!sym(var))) +
    geom_histogram(aes(y = after_stat(density)), 
                   bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
    stat_function(fun = dnorm, 
                  args = list(mean = as_mean, sd = as_sd),
                  color = "red", linewidth = 1) +
    geom_vline(xintercept = as_mean, color = "darkblue", 
               linetype = "dashed", linewidth = 1) +
    annotate("text", x = Inf, y = Inf, 
             label = sprintf("AS\nN = %d\nMean = %.2f\nSD = %.2f", 
                             as_n, as_mean, as_sd),
             hjust = 1.1, vjust = 1.1, size = 3) +
    labs(title = paste(var, "- AS (Exp 1-5)"),
         x = var, y = "Density") +
    theme_bw() +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  histogram_plots[[plot_counter]] <- p_as
  plot_counter <- plot_counter + 1
  
  # JZ histogram (right panel)
  df_jz_seedlings <- df_seedlings %>% filter(experimenter == "JZ")
  
  jz_mean <- mean(df_jz_seedlings[[var]], na.rm = TRUE)
  jz_sd <- sd(df_jz_seedlings[[var]], na.rm = TRUE)
  jz_n <- sum(!is.na(df_jz_seedlings[[var]]))
  
  p_jz <- ggplot(df_jz_seedlings, aes(x = !!sym(var))) +
    geom_histogram(aes(y = after_stat(density)), 
                   bins = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
    stat_function(fun = dnorm, 
                  args = list(mean = jz_mean, sd = jz_sd),
                  color = "red", linewidth = 1) +
    geom_vline(xintercept = jz_mean, color = "darkgreen", 
               linetype = "dashed", linewidth = 1) +
    annotate("text", x = Inf, y = Inf, 
             label = sprintf("JZ\nN = %d\nMean = %.2f\nSD = %.2f", 
                             jz_n, jz_mean, jz_sd),
             hjust = 1.1, vjust = 1.1, size = 3) +
    labs(title = paste(var, "- JZ (Exp 6-10)"),
         x = var, y = "Density") +
    theme_bw() +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  histogram_plots[[plot_counter]] <- p_jz
  plot_counter <- plot_counter + 1
}

# Arrange all 8 plots in 4 rows × 2 columns
combined_histograms <- gridExtra::grid.arrange(
  grobs = histogram_plots,
  ncol = 2,
  nrow = 4,
  top = grid::textGrob(
    "Distribution of Seedling Measurements: AS vs JZ\nRed line = Normal distribution overlay, Dashed line = Mean",
    gp = grid::gpar(fontsize = 12, fontface = "bold")
  )
)

# Save plot into the run folder built up top in DERIVED PATHS, using the
# shared out_path() helper so the filename pattern matches s5 exactly:
# <date>_<script>_<dataset>_<suffix>.<ext>.
output_histogram <- out_path("seedling_distributions", "png")
ggsave(
  filename = output_histogram,
  plot = combined_histograms,
  width = 28,
  height = 35,
  dpi = 300,
  units = "cm"
)

cat(sprintf("Distribution histograms saved as: %s\n", output_histogram))
cat("\n")
