# s5_cress_descriptive_anova.r
#
# Pipeline step 5: descriptive stats + Type-III ANOVA + emmeans post-hoc
# + normalized-by-Lactose boxplots, on bag-level means of the cress data.
#
# Why bag-level: the bag is the experimental unit (potency was applied per
# bag, ~16 seeds per bag). Treating individual seeds as independent inflates
# Type-I error via pseudoreplication; s6 (ICC) quantifies that. Here we
# average per bag first and run the ANOVA on those means.
#
# Design: potency (6 levels) * experiment_number, Type-III SS with
# contr.sum so the interaction term is testable independently of cell
# coding. Three analysis groups: ALL (all experiments), AS (1-5),
# JZ (6-10) -- separates the two experimenters in case of operator effect.
#
# Inputs : either the s3 v1v2 combined file (raw) or an s4 skewness-
#          corrected file, depending on USE_SKEWNESS_CORRECTED. Either way
#          the file carries in_v1_analysis / in_v2_analysis flags written
#          by s3, and we filter rows here by DATASET_VER.
# Outputs: ANOVA xlsx (3 sheets: ALL/AS/JZ), post-hoc emmeans xlsx,
#          24 normalized boxplots (4 vars x 3 groups x {by_exp, by_potency}).


library(readxl)
library(car)        # Anova(type="III")
library(here)
library(dplyr)
library(tidyr)
library(openxlsx)
library(emmeans)
library(ggplot2)
library(gridExtra)
library(patchwork)  # 2016/2022 grid assembly for the texture-style line plot
library(plotrix)    # std.error() = sd / sqrt(n) for SE error bars


#===== CONFIG ===============================================================

SCRIPT_TAG     <- "s5"
DATASET_VER    <- "v2"   # "v1" | "v2" | "v1v2"
USE_SKEWNESS_CORRECTED <- TRUE   # TRUE -> read latest s4 output for DATASET_VER
RUN_DATE       <- format(Sys.Date(), "%Y%m%d")

# Response variables analysed. When USE_SKEWNESS_CORRECTED is TRUE we look
# for the matching T<var>_cut* column in the s4 file and use it instead of
# the raw column (the bag-level mean is then computed on the truncated data).
response_vars <- c("sprout_length", "root_length",
                   "seedling_length", "root_sprout_ratio")

# Inputs.
INPUT_S3_BASENAME <- "cress_length_ASPS_1-10_alldata_decoded_v1v2.xlsx"
INPUT_S3_PARENT   <- "cress_combine_files"  # holds <date>_cress_combined/
S4_OUTPUTS_ROOT   <- "outputs"


#===== DERIVED PATHS (don't edit) ===========================================

SCRIPT_PURPOSE <- paste0("anova_",
                         if (USE_SKEWNESS_CORRECTED) "skewcorr" else "raw")

out_dir <- file.path(
  S4_OUTPUTS_ROOT,
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

# Case 1: raw s3 file. Most-recent <date>_cress_combined/ under
# cress_combine_files/.
# Case 2: skewness-corrected. Walk outputs/ for the most recent
# <date>_s4_skewness_<DATASET_VER>/ and read its skewness_corr.xlsx.
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

cat("\n", strrep("=", 80), "\n", sep = "")
cat("READING DATA\n")
cat(strrep("=", 80), "\n", sep = "")
cat("Input         : ", input_path, "\n", sep = "")
cat("Dataset ver   : ", DATASET_VER, "\n", sep = "")
cat("Skewness corr : ", USE_SKEWNESS_CORRECTED, "\n", sep = "")
cat("Output folder : ", out_dir, "\n\n", sep = "")

df_raw <- read_excel(input_path, sheet = "Sheet 1")

# Filter by dataset version using the membership flags written by s3
# (preserved through s4). "v1v2" keeps every row -- biological samples
# in ASPS 1-5 then appear twice (v1 + v2), so it's a comparison view,
# not a normal analysis input.
df_raw <- switch(DATASET_VER,
  "v1"   = df_raw[df_raw$in_v1_analysis, ],
  "v2"   = df_raw[df_raw$in_v2_analysis, ],
  "v1v2" = df_raw,
  stop("DATASET_VER must be 'v1', 'v2', or 'v1v2'; got: ", DATASET_VER)
)
cat("Total seed-level rows after filter: ", nrow(df_raw), "\n", sep = "")


#===== SWAP TO TRUNCATED COLUMNS WHEN USING SKEWNESS-CORRECTED INPUT ========

# s2 appends T<var>_cut<value> columns rather than overwriting the originals.
# To keep downstream code uniform we copy the truncated column back over the
# original name *for the response_vars that have a truncated counterpart*.
# Variables without a truncated column (e.g. root_sprout_ratio) flow through
# unchanged. The console prints which columns were swapped so this isn't
# silent.
if (USE_SKEWNESS_CORRECTED) {
  for (var in response_vars) {
    trunc_cols <- grep(paste0("^T", var, "_cut"), colnames(df_raw), value = TRUE)
    if (length(trunc_cols) >= 1) {
      df_raw[[var]] <- df_raw[[trunc_cols[1]]]
      cat("  swapped ", var, " <- ", trunc_cols[1], "\n", sep = "")
    }
  }
}


#===== PARSE EXPERIMENT AND POTENCY =========================================

# exp_no in the data is e.g. "3_A": <experiment_number>_<potency_code>.
# We split that out and tag rows AS / JZ by experimenter so the analysis
# can be split by operator.
df_parsed <- df_raw %>%
  separate(exp_no, into = c("experiment_number", "potency_code"),
           sep = "_", remove = FALSE) %>%
  mutate(
    experiment_number = as.integer(experiment_number),
    bag = as.integer(bag),
    experimenter = ifelse(experiment_number <= 5, "AS", "JZ")
  )


#===== CALCULATE BAG-LEVEL MEANS ============================================

# The bag is the experimental unit (potency applied per bag, ~16 seeds in
# each). Averaging here before the ANOVA avoids pseudoreplication; s4
# quantifies how badly the seed-level analysis would inflate Type-I error.
# root_sprout_ratio is averaged per bag from the per-seed ratios already in
# the input, matching the original script.
df_bags <- df_parsed %>%
  group_by(experiment_number, experimenter, potency_code, potency, bag,
           exp_no, label) %>%
  summarise(
    n_seeds           = n(),
    sprout_length     = mean(sprout_length,     na.rm = TRUE),
    root_length       = mean(root_length,       na.rm = TRUE),
    seedling_length   = mean(seedling_length,   na.rm = TRUE),
    root_sprout_ratio = mean(root_sprout_ratio, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nBag-level rows: ", nrow(df_bags),
    "  (mean ", round(mean(df_bags$n_seeds), 1),
    " seeds/bag, range ", min(df_bags$n_seeds),
    "-", max(df_bags$n_seeds), ")\n", sep = "")


#===== BAG INVENTORY ========================================================

# Full enumeration of every bag that survives into df_bags, with the seed
# count per bag. Two views are printed:
#   (1) per-experiment summary  -- quick check that all 10 experiments are
#       present and that AS (1-5) / JZ (6-10) bag counts look right.
#   (2) full per-bag table      -- every bag in the run, no truncation, so
#       odd bags (very low n_seeds, missing potencies) are easy to spot.
# Uses df_bags before factorisation so experiment_number / potency_code /
# bag still sort naturally as numbers/characters.

cat("\n", strrep("#", 80), "\n", sep = "")
cat("BAG INVENTORY (dataset feeding this run)\n")
cat(strrep("#", 80), "\n", sep = "")

# (1) Per-experiment summary: how many bags and seeds per experiment, and
# the spread of seeds-per-bag within that experiment.
bag_inventory_by_exp <- df_bags %>%
  group_by(experimenter, experiment_number) %>%
  summarise(
    n_bags        = n(),
    n_seeds_total = sum(n_seeds),
    mean_seeds    = round(mean(n_seeds), 1),
    min_seeds     = min(n_seeds),
    max_seeds     = max(n_seeds),
    .groups       = "drop"
  ) %>%
  arrange(experimenter, experiment_number)

cat("\nPer-experiment summary:\n")
print.data.frame(bag_inventory_by_exp, row.names = FALSE)

# (2) Bag x experiment matrix of seed counts. One row per (potency_code,
# bag), one column per experiment, cells = n_seeds. Empty cells mark bags
# that don't exist for that experiment, so missing potencies and odd low-
# count bags are spotted at a glance instead of scrolling a long per-bag
# list. Rows are sorted by potency order (as it appears in df_bags) and
# then numeric bag suffix, so bags of the same potency stay adjacent and
# "Lactose" doesn't get alphabetised out of place.
potency_order <- unique(df_bags$potency_code)
bag_matrix <- df_bags %>%
  mutate(bag_id = paste0(potency_code, bag)) %>%
  select(experiment_number, bag_id, potency_code, bag, n_seeds) %>%
  pivot_wider(names_from = experiment_number, values_from = n_seeds) %>%
  arrange(match(potency_code, potency_order), bag) %>%
  select(-potency_code, -bag)  # keep bag_id as the row label

cat("\nBag x experiment matrix of seed counts (",
    nrow(bag_matrix), " bag slots x ",
    ncol(bag_matrix) - 1, " experiments):\n", sep = "")
print.data.frame(bag_matrix, row.names = FALSE, na.print = "")

# (3) Totals split by experimenter -- one-line sanity check that AS + JZ
# add up to the bag-level rows reported above.
bag_totals <- df_bags %>%
  group_by(experimenter) %>%
  summarise(
    n_bags        = n(),
    n_seeds_total = sum(n_seeds),
    .groups       = "drop"
  )

cat("\nTotals by experimenter:\n")
print.data.frame(bag_totals, row.names = FALSE)
cat("Grand total: ", nrow(df_bags), " bags, ",
    sum(df_bags$n_seeds), " seeds\n", sep = "")


# Factorise grouping vars so Anova(type="III") gets the right contrasts.
df_bags$experiment_number <- as.factor(df_bags$experiment_number)
df_bags$potency           <- as.factor(df_bags$potency)
df_bags$potency_code      <- as.factor(df_bags$potency_code)
df_bags$experimenter      <- as.factor(df_bags$experimenter)


#===== ANALYSIS GROUPS ======================================================

# Three parallel cuts: all data, AS-only (exp 1-5), JZ-only (exp 6-10).
# Splitting by experimenter lets us see operator-specific effects without
# polluting the combined model.
analysis_groups <- list(
  ALL = list(name = "ALL DATA (ASPS 1-10)",
             data = df_bags, sheet_name = "ALL"),
  AS  = list(name = "AS ONLY (ASPS 1-5)",
             data = df_bags %>% filter(experimenter == "AS"),
             sheet_name = "AS"),
  JZ  = list(name = "JZ ONLY (ASPS 6-10)",
             data = df_bags %>% filter(experimenter == "JZ"),
             sheet_name = "JZ")
)


#===== HELPERS ==============================================================

# 5-line star helper -- replaces the four-deep ifelse chains in the
# original. Returns the conventional p-value annotation.
sig_stars <- function(p) {
  if (is.na(p))     return("")
  if (p < 0.001)    return("***")
  if (p < 0.01)     return("**")
  if (p < 0.05)     return("*")
  if (p < 0.10)     return(".")
  return("ns")
}


#===== DESCRIPTIVE STATISTICS ===============================================

cat("\n", strrep("#", 80), "\n", sep = "")
cat("DESCRIPTIVE STATISTICS\n")
cat(strrep("#", 80), "\n", sep = "")

for (group_key in names(analysis_groups)) {
  group <- analysis_groups[[group_key]]
  df_subset <- group$data
  cat("\n", strrep("=", 80), "\n", group$name, "\n",
      strrep("=", 80), "\n", sep = "")
  cat("N bags: ", nrow(df_subset), "\n", sep = "")

  for (var in response_vars) {
    cat("\n--- ", toupper(var), " ---\n", sep = "")

    overall <- df_subset %>% summarise(
      Mean = mean(!!sym(var), na.rm = TRUE),
      SD   = sd(!!sym(var),   na.rm = TRUE),
      Min  = min(!!sym(var),  na.rm = TRUE),
      Max  = max(!!sym(var),  na.rm = TRUE)
    )
    cat(sprintf("  Mean +/- SD: %.3f +/- %.3f   range %.3f - %.3f\n",
                overall$Mean, overall$SD, overall$Min, overall$Max))

    by_potency <- df_subset %>%
      group_by(potency) %>%
      summarise(Mean = mean(!!sym(var), na.rm = TRUE),
                SD   = sd(!!sym(var),   na.rm = TRUE),
                N    = n(), .groups = "drop") %>%
      arrange(potency)
    cat(sprintf("  %-12s %10s %10s %6s\n", "Potency", "Mean", "SD", "N"))
    for (i in seq_len(nrow(by_potency))) {
      cat(sprintf("  %-12s %10.3f %10.3f %6d\n",
                  by_potency$potency[i], by_potency$Mean[i],
                  by_potency$SD[i], by_potency$N[i]))
    }
  }
}


#===== ANOVA (Type III SS) ==================================================

cat("\n\n", strrep("#", 80), "\n", sep = "")
cat("ANOVA (Type-III SS, bag-level means, contr.sum)\n")
cat(strrep("#", 80), "\n", sep = "")

# contr.sum required so Type-III SS for main effects is interpretable in
# the presence of the interaction term.
options(contrasts = c("contr.sum", "contr.poly"))

anova_results_list <- list()

for (group_key in names(analysis_groups)) {
  group <- analysis_groups[[group_key]]
  df_subset <- group$data
  cat("\n", strrep("=", 80), "\n", group$name, "\n",
      strrep("=", 80), "\n", sep = "")

  group_results <- list()
  for (var in response_vars) {
    cat("\n--- ", toupper(var), " ---\n", sep = "")
    model <- aov(as.formula(paste(var, "~ potency * experiment_number")),
                 data = df_subset)
    a <- Anova(model, type = "III")
    print(a, digits = 6)

    p_pot <- a["potency",                       "Pr(>F)"]
    p_exp <- a["experiment_number",             "Pr(>F)"]
    p_int <- a["potency:experiment_number",     "Pr(>F)"]
    cat(sprintf("\n  Potency:     p = %.6f  %s\n", p_pot, sig_stars(p_pot)))
    cat(sprintf("  Experiment:  p = %.6f  %s\n",   p_exp, sig_stars(p_exp)))
    cat(sprintf("  Interaction: p = %.6f  %s\n",   p_int, sig_stars(p_int)))

    group_results[[var]] <- data.frame(
      Experiment  = p_exp,
      Potency     = p_pot,
      Interaction = p_int
    )
  }
  anova_results_list[[group_key]] <- group_results
}


#===== EXPORT ANOVA SUMMARY (xlsx, colour-coded) ============================

# Cell-fill cues match the printed-report convention used previously:
# red < 0.01, orange < 0.05, lilac < 0.10. Header row bolded for scannability.
wb_anova    <- createWorkbook()
style_red    <- createStyle(fgFill = "#FFB3BA")
style_orange <- createStyle(fgFill = "#FFDFBA")
style_lilac  <- createStyle(fgFill = "#E0BBE4")
style_header <- createStyle(textDecoration = "bold", fgFill = "#D3D3D3")

# Walk one p-value cell and apply the colour appropriate to its threshold.
fill_p <- function(wb, sheet, row, col, p) {
  if (is.na(p)) return(invisible())
  s <- if (p < 0.01) style_red
       else if (p < 0.05) style_orange
       else if (p < 0.10) style_lilac
       else NULL
  if (!is.null(s)) addStyle(wb, sheet, s, rows = row, cols = col, stack = TRUE)
}

for (group_key in names(analysis_groups)) {
  group     <- analysis_groups[[group_key]]
  sheet     <- group$sheet_name
  results   <- anova_results_list[[group_key]]
  results_df <- do.call(rbind, results)
  results_df <- data.frame(Parameter = rownames(results_df),
                           results_df, stringsAsFactors = FALSE)

  addWorksheet(wb_anova, sheet)
  writeData(wb_anova, sheet, paste0("ANOVA Summary: ", group$name),
            startRow = 1, startCol = 1)
  writeData(wb_anova, sheet, results_df, startRow = 3, rowNames = FALSE)
  addStyle(wb_anova, sheet, style_header,
           rows = 3, cols = 1:4, gridExpand = TRUE)

  for (r in seq_len(nrow(results_df))) {
    for (c in 2:4) {
      fill_p(wb_anova, sheet, r + 3, c, results_df[r, c])
    }
  }
  setColWidths(wb_anova, sheet, cols = 1:4, widths = c(25, 15, 15, 15))

  legend_row <- nrow(results_df) + 5
  writeData(wb_anova, sheet, "Color Legend:",         startRow = legend_row,     startCol = 1)
  writeData(wb_anova, sheet, "Light lilac = p<0.10",  startRow = legend_row + 1, startCol = 1)
  writeData(wb_anova, sheet, "Light orange = p<0.05", startRow = legend_row + 2, startCol = 1)
  writeData(wb_anova, sheet, "Light red = p<0.01",    startRow = legend_row + 3, startCol = 1)
  addStyle(wb_anova, sheet, style_lilac,  rows = legend_row + 1, cols = 1)
  addStyle(wb_anova, sheet, style_orange, rows = legend_row + 2, cols = 1)
  addStyle(wb_anova, sheet, style_red,    rows = legend_row + 3, cols = 1)
}

out_anova <- out_path("anova_summary_ALL_AS_JZ", "xlsx")
saveWorkbook(wb_anova, out_anova, overwrite = TRUE)
cat("\nWrote ANOVA summary: ", out_anova, "\n", sep = "")


#===== POST-HOC: emmeans pairwise comparisons ==============================

# For each (group, response_var) build two lower-triangular matrices of
# pairwise potency p-values:
#   1. potency main effect: marginal pairs across experiments
#      (adjust="none" -- raw pairwise p, matches original behaviour).
#   2. interaction contrast: tests whether the (pot1 - pot2) contrast
#      itself differs across experiments (joint test of pairs of contrasts).
# When there's only one experiment in the subset the interaction column
# is "NA" by construction.
compute_emmeans_contrasts <- function(data_subset, variable) {

  n_experiments <- length(unique(data_subset$experiment_number))
  model <- aov(as.formula(paste(variable, "~ potency * experiment_number")),
               data = data_subset)

  remedies   <- levels(data_subset$potency)
  n_remedies <- length(remedies)

  potency_matrix     <- matrix(NA, n_remedies, n_remedies,
                               dimnames = list(remedies, remedies))
  interaction_matrix <- matrix(NA, n_remedies, n_remedies,
                               dimnames = list(remedies, remedies))

  # Main-effect pairwise potency contrasts (collapsed across experiment).
  emm <- emmeans(model, ~ potency)
  pairs_summary <- summary(pairs(emm, adjust = "none"))

  for (i in seq_len(nrow(pairs_summary))) {
    parts   <- trimws(strsplit(as.character(pairs_summary$contrast[i]),
                               " - ")[[1]])
    idx1    <- which(remedies == parts[1])
    idx2    <- which(remedies == parts[2])
    p_val   <- pairs_summary$p.value[i]
    # Always store in lower triangle (row > col) for consistent layout.
    if (idx1 > idx2) potency_matrix[idx1, idx2] <- p_val
    else             potency_matrix[idx2, idx1] <- p_val
  }

  if (n_experiments > 1) {
    tryCatch({
      emm_by_exp           <- emmeans(model, ~ potency | experiment_number)
      pairs_by_exp         <- pairs(emm_by_exp)
      pairs_by_exp_summary <- summary(pairs_by_exp)

      for (i in 1:(n_remedies - 1)) {
        for (j in (i + 1):n_remedies) {
          comparison_name <- paste(remedies[i], "-", remedies[j])
          rows <- grep(paste0("^", comparison_name, "$"),
                       pairs_by_exp_summary$contrast)
          if (length(rows) >= 2) {
            tryCatch({
              specific  <- pairs_by_exp[rows]
              joint     <- test(pairs(specific), joint = TRUE)
              interaction_matrix[j, i] <- joint$p.value
            }, error = function(e) {
              interaction_matrix[j, i] <- NA
            })
          }
        }
      }
    }, error = function(e) {
      interaction_matrix[lower.tri(interaction_matrix)] <- NA
    })
  }

  list(potency = potency_matrix, interaction = interaction_matrix)
}

cat("\n\n", strrep("#", 80), "\n", sep = "")
cat("POST-HOC: emmeans pairwise comparisons\n")
cat(strrep("#", 80), "\n", sep = "")

wb_posthoc  <- createWorkbook()
style_4dec  <- createStyle(numFmt = "0.0000")
style_bold  <- createStyle(textDecoration = "bold")

for (group_key in names(analysis_groups)) {
  group     <- analysis_groups[[group_key]]
  sheet     <- group$sheet_name
  df_subset <- group$data
  cat("\n", strrep("=", 80), "\n", group$name, "\n",
      strrep("=", 80), "\n", sep = "")

  addWorksheet(wb_posthoc, sheet)
  current_row <- 1
  writeData(wb_posthoc, sheet, paste0("Post Hoc Tests: ", group$name),
            startRow = current_row, startCol = 1)
  addStyle(wb_posthoc, sheet, style_bold, rows = current_row, cols = 1)
  current_row <- current_row + 2

  n_remedies <- length(levels(df_subset$potency))

  for (var in response_vars) {
    cat("  ", var, "\n", sep = "")
    res     <- compute_emmeans_contrasts(df_subset, var)
    pmat    <- res$potency
    imat    <- res$interaction
    remedies <- rownames(pmat)
    n_remedies <- length(remedies)

    # Header row 1: variable name + spanning potency labels.
    writeData(wb_posthoc, sheet, var, startRow = current_row, startCol = 1)
    for (j in seq_len(n_remedies)) {
      writeData(wb_posthoc, sheet, remedies[j],
                startRow = current_row, startCol = 1 + (j - 1) * 2 + 1)
      mergeCells(wb_posthoc, sheet,
                 rows = current_row,
                 cols = (1 + (j - 1) * 2 + 1):(1 + (j - 1) * 2 + 2))
    }
    addStyle(wb_posthoc, sheet, style_header,
             rows = current_row, cols = 1:(2 * n_remedies + 1),
             gridExpand = TRUE)
    current_row <- current_row + 1

    # Header row 2: potency / interaction sub-labels per column pair.
    for (j in seq_len(n_remedies)) {
      writeData(wb_posthoc, sheet, "potency",
                startRow = current_row, startCol = 1 + (j - 1) * 2 + 1)
      writeData(wb_posthoc, sheet, "interaction",
                startRow = current_row, startCol = 1 + (j - 1) * 2 + 2)
    }
    addStyle(wb_posthoc, sheet, style_header,
             rows = current_row, cols = 1:(2 * n_remedies + 1),
             gridExpand = TRUE)
    current_row <- current_row + 1

    # Body: one row per row-potency, lower triangle of pairwise p-values.
    for (i in seq_len(n_remedies)) {
      writeData(wb_posthoc, sheet, remedies[i],
                startRow = current_row, startCol = 1)
      for (j in seq_len(n_remedies)) {
        if (j >= i) next
        col_pot <- 1 + (j - 1) * 2 + 1
        col_int <- col_pot + 1

        p_pot <- pmat[i, j]
        p_int <- imat[i, j]

        if (is.numeric(p_pot) && !is.na(p_pot)) {
          writeData(wb_posthoc, sheet, p_pot,
                    startRow = current_row, startCol = col_pot)
          addStyle(wb_posthoc, sheet, style_4dec,
                   rows = current_row, cols = col_pot)
          fill_p(wb_posthoc, sheet, current_row, col_pot, p_pot)
        }
        if (is.numeric(p_int) && !is.na(p_int)) {
          writeData(wb_posthoc, sheet, p_int,
                    startRow = current_row, startCol = col_int)
          addStyle(wb_posthoc, sheet, style_4dec,
                   rows = current_row, cols = col_int)
          fill_p(wb_posthoc, sheet, current_row, col_int, p_int)
        }
      }
      current_row <- current_row + 1
    }
    current_row <- current_row + 2
  }

  setColWidths(wb_posthoc, sheet,
               cols = 1:(2 * n_remedies + 1),
               widths = c(15, rep(10, 2 * n_remedies)))

  legend_row <- current_row + 1
  writeData(wb_posthoc, sheet, "Color Legend:",         startRow = legend_row,     startCol = 1)
  writeData(wb_posthoc, sheet, "Light lilac = p<0.10",  startRow = legend_row + 1, startCol = 1)
  writeData(wb_posthoc, sheet, "Light orange = p<0.05", startRow = legend_row + 2, startCol = 1)
  writeData(wb_posthoc, sheet, "Light red = p<0.01",    startRow = legend_row + 3, startCol = 1)
  addStyle(wb_posthoc, sheet, style_lilac,  rows = legend_row + 1, cols = 1)
  addStyle(wb_posthoc, sheet, style_orange, rows = legend_row + 2, cols = 1)
  addStyle(wb_posthoc, sheet, style_red,    rows = legend_row + 3, cols = 1)
}

out_posthoc <- out_path("posthoc_emmeans_ALL_AS_JZ", "xlsx")
saveWorkbook(wb_posthoc, out_posthoc, overwrite = TRUE)
cat("\nWrote post-hoc: ", out_posthoc, "\n", sep = "")


#===== PLOTS: normalized boxplots (by experiment, by potency) ===============

# Each plot shows per-bag values normalized to the Lactose grand mean of the
# subset (so 1.0 = the negative-control level for that subset). Two layouts:
#   by_exp     - one panel per experiment, x-axis = potency
#   by_potency - one panel per potency,    x-axis = experiment
# Saved at 300dpi cm so they're print-ready.

# Robust legend extractor. cowplot::get_legend() is fragile across ggplot2
# major versions: ggplot2 3.5.0 renamed the legend gtable cell from
# "guide-box" to "guide-box-bottom"/"-right"/..., and ggplot2 4.0 rewrote
# the guide system again. When cowplot's lookup misses, it returns a half-
# initialised grob with a NULL viewport path -- which then crashes
# grid.arrange() later on with
#   Error in UseMethod("depth"): no applicable method for 'depth'
#                                applied to an object of class "NULL"
# To stay version-independent we build the gtable ourselves and grab the
# first non-empty grob whose layout name starts with "guide-box".
extract_legend <- function(plot) {
  g <- ggplot2::ggplotGrob(plot)
  guide_idx <- which(grepl("^guide-box", g$layout$name))
  for (i in guide_idx) {
    grob <- g$grobs[[i]]
    if (!inherits(grob, "zeroGrob")) return(grob)
  }
  grid::nullGrob()  # fallback: shouldn't trigger unless the plot has no legend
}

# Build a shared bottom legend for a faceted ggplot grid that uses
# theme(legend.position="none") on the panels themselves.
build_panel_legend <- function(df_normalized, fill_var, fill_palette,
                               fill_title) {
  p <- ggplot(df_normalized,
              aes(x = .data[[fill_var]], y = normalized_value,
                  fill = .data[[fill_var]])) +
    geom_boxplot() +
    scale_fill_brewer(palette = fill_palette, name = fill_title) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 10, face = "bold"),
          legend.text  = element_text(size = 9))
  extract_legend(p)
}

# Single panel for one experiment OR one potency.
build_panel <- function(panel_data, x_var, fill_var, fill_palette, title) {
  ggplot(panel_data,
         aes(x = .data[[x_var]], y = normalized_value,
             fill = .data[[fill_var]])) +
    geom_boxplot(outlier.size = 1.5, width = 0.7) +
    geom_hline(yintercept = 1.0, linetype = "dashed",
               color = "red", linewidth = 0.7) +
    scale_fill_brewer(palette = fill_palette) +
    labs(title = title, x = NULL, y = "Normalized value") +
    theme_bw() +
    theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 9),
          legend.position = "none",
          panel.grid.major.x = element_blank())
}

cat("\n\n", strrep("#", 80), "\n", sep = "")
cat("PLOTS: normalized boxplots\n")
cat(strrep("#", 80), "\n", sep = "")

for (group_key in names(analysis_groups)) {
  group       <- analysis_groups[[group_key]]
  df_subset   <- group$data
  group_label <- group$sheet_name
  cat("\n", group$name, "\n", sep = "")

  for (var in response_vars) {
    # Normalize to Lactose grand mean within this subset.
    lactose_mean <- df_subset %>%
      filter(potency == "Lactose") %>%
      pull(!!sym(var)) %>%
      mean(na.rm = TRUE)

    df_normalized <- df_subset %>%
      mutate(normalized_value = !!sym(var) / lactose_mean)

    #--- by experiment: one panel per exp, x = potency
    experiments <- sort(unique(df_normalized$experiment_number))
    panels <- lapply(experiments, function(exp) {
      build_panel(df_normalized %>% filter(experiment_number == exp),
                  x_var = "potency", fill_var = "potency",
                  fill_palette = "Dark2",
                  title = paste0("Exp ", exp))
    })
    legend <- build_panel_legend(df_normalized, "potency", "Dark2", "Potency")

    n_exp        <- length(experiments)
    plot_width   <- max(35, n_exp * 7)
    combined <- gridExtra::grid.arrange(
      grobs  = panels,
      ncol   = n_exp, nrow = 1,
      top    = grid::textGrob(
        paste0(var, " (", group_label, ") -- normalized to Lactose mean = ",
               sprintf("%.3f", lactose_mean)),
        gp = grid::gpar(fontsize = 14, fontface = "bold")),
      bottom = legend
    )
    ggsave(filename = out_path(paste0("normalized_by_exp_", group_label,
                                      "_", var), "png"),
           plot = combined,
           width = plot_width, height = 12, dpi = 300, units = "cm")

    #--- by potency: one panel per potency, x = experiment
    potencies <- sort(unique(df_normalized$potency))
    panels <- lapply(potencies, function(pot) {
      build_panel(df_normalized %>% filter(potency == pot),
                  x_var = "experiment_number",
                  fill_var = "experiment_number",
                  fill_palette = "Set2",
                  title = pot)
    })
    legend <- build_panel_legend(df_normalized, "experiment_number",
                                 "Set2", "Experiment")
    combined <- gridExtra::grid.arrange(
      grobs  = panels,
      ncol   = length(potencies), nrow = 1,
      top    = grid::textGrob(
        paste0(var, " by Potency (", group_label,
               ") -- normalized to Lactose mean = ",
               sprintf("%.3f", lactose_mean)),
        gp = grid::gpar(fontsize = 14, fontface = "bold")),
      bottom = legend
    )
    ggsave(filename = out_path(paste0("normalized_by_potency_", group_label,
                                      "_", var), "png"),
           plot = combined,
           width = 42, height = 12, dpi = 300, units = "cm")

    cat("  ", var, "\n", sep = "")
  }
}


#===== PLOTS: mean +/- SE line plots (2016 vs 2022, texture style) ==========

# Companion figure to the boxplots above. Layout mirrors the ASPS texture/
# fractal output: rows = response variables, columns = experimenter cohort
# (2016/AS exp 1-5 left, 2022/JZ exp 6-10 right). Within a row both columns
# share y-limits so the two cohorts are directly comparable; the right column
# drops its y-axis to give the panels more horizontal room.
#
# Each point is the (experiment, remedy) bag-level mean and the error bar is
# +/- 1 SE across bags within that cell (SE = sd / sqrt(n_bags)). The error
# bars therefore represent bag-to-bag variability, NOT seed-to-seed -- this
# is intentional: the bag is the experimental unit and seed-level SE would
# pseudoreplicate (see s6 ICC for the pseudoreplication penalty).
#
# x-axis is numeric experiment_number so any missing experiment leaves a
# real gap (e.g. if exp 4 is absent for AS in the current DATASET_VER, the
# AS line skips that x position). Lines connect points of the same remedy
# across experiments to make trajectories easier to read.

# Texture-script palette and remedy display order. Keys must match the
# spelling in df_bags$potency exactly -- the s3 decoder writes "Ars. album"
# (space + lowercase a), not "Ars.Album".
potency_hierarchy <- c("Lactose", "Stannum", "Silicea", "Sulphur",
                       "Ars. album", "Mercury")
potency_colors <- c(
  "Lactose"    = "#51CFFD",
  "Stannum"    = "#FFC72C",
  "Silicea"    = "#E7298A",
  "Sulphur"    = "#9100AB",
  "Ars. album" = "#00BD5F",
  "Mercury"    = "#919191"
)

# Aggregate bag-level rows to one mean +/- SE per (cohort, experiment, remedy).
# experiment_number was factorised at the ANOVA stage; convert back to integer
# here so the x-axis can be numeric (real gaps for missing experiments).
df_lineplot <- df_bags %>%
  mutate(
    experiment_number  = as.integer(as.character(experiment_number)),
    potency            = as.character(potency),
    experimenter_label = ifelse(experimenter == "AS",
                                "2016 data (Experiments 1-5)",
                                "2022 data (Experiments 6-10)")
  ) %>%
  group_by(experimenter_label, experiment_number, potency) %>%
  summarise(
    across(all_of(response_vars),
           list(mean = ~mean(.x, na.rm = TRUE),
                se   = ~plotrix::std.error(.x, na.rm = TRUE))),
    n_bags = n(),
    .groups = "drop"
  ) %>%
  # Lock factor order so the legend follows potency_hierarchy and the colour
  # mapping by name is unambiguous.
  mutate(potency = factor(potency, levels = potency_hierarchy))

# Per-measure y-limits computed across BOTH cohorts so the 2016 and 2022
# panels in the same row line up. Extend the range by 5% top and bottom so
# error bars don't kiss the panel edge.
y_limits <- lapply(response_vars, function(v) {
  mn  <- df_lineplot[[paste0(v, "_mean")]]
  se  <- df_lineplot[[paste0(v, "_se")]]
  lo  <- min(mn - se, na.rm = TRUE)
  hi  <- max(mn + se, na.rm = TRUE)
  pad <- 0.05 * (hi - lo)
  c(lo - pad, hi + pad)
})
names(y_limits) <- response_vars

# Build one ggplot per (measure, cohort). Two nested loops -> 4 * 2 = 8
# panels in row-major order, which patchwork::wrap_plots(ncol = 2) lays out
# as the requested 4-row x 2-column grid.
cohort_labels <- c("2016 data (Experiments 1-5)",
                   "2022 data (Experiments 6-10)")

plot_list <- list()
for (var in response_vars) {
  for (col_idx in seq_along(cohort_labels)) {
    cohort <- cohort_labels[col_idx]
    panel_data <- df_lineplot %>% filter(experimenter_label == cohort)

    p <- ggplot(panel_data,
                aes(x     = experiment_number,
                    y     = .data[[paste0(var, "_mean")]],
                    color = potency,
                    group = potency)) +
      geom_line(position = position_dodge(width = 0.3)) +
      geom_point(position = position_dodge(width = 0.3), size = 2) +
      geom_errorbar(aes(ymin = .data[[paste0(var, "_mean")]] -
                                .data[[paste0(var, "_se")]],
                        ymax = .data[[paste0(var, "_mean")]] +
                                .data[[paste0(var, "_se")]]),
                    width = 0.3,
                    position = position_dodge(width = 0.3)) +
      scale_color_manual(values = potency_colors,
                         breaks = potency_hierarchy,
                         name   = "Remedy") +
      scale_y_continuous(limits = y_limits[[var]]) +
      scale_x_continuous(breaks = 1:10) +
      labs(title = cohort, y = var, x = "Experiment Number") +
      theme_bw() +
      theme(
        plot.title         = element_text(size = 15, face = "bold"),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 13),
        legend.title       = element_text(size = 13),
        legend.text        = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )

    if (col_idx == 1) {
      # Left (2016): hide legend (right column carries it), tighter right
      # margin so the two columns sit close together.
      p <- p + theme(legend.position = "none",
                     plot.margin     = margin(t = 5, r = 2, b = 5, l = 5))
    } else {
      # Right (2022): drop y-axis -- shared scale is already shown on the
      # left -- and tighten left margin to mirror the gap on the other side.
      p <- p + theme(axis.title.y = element_blank(),
                     axis.text.y  = element_blank(),
                     axis.ticks.y = element_blank(),
                     plot.margin  = margin(t = 5, r = 5, b = 5, l = 2))
    }

    plot_list[[length(plot_list) + 1]] <- p
  }
}

# Assemble the 4 x 2 grid. patchwork auto-aligns panel widths and heights so
# the shared-y-limit pairs in each row line up cleanly.
grid_plot <- patchwork::wrap_plots(plot_list,
                                   ncol = 2,
                                   nrow = length(response_vars)) +
  patchwork::plot_annotation(
    title    = "ASPS Cress Analysis: All six potencies",
    subtitle = paste0("Mean +/- SE across bags (n_bags varies per cell); ",
                      "dataset version: ", DATASET_VER),
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    )
  )

# Output sized like the texture script: 30 cm wide, 8.2 cm per row plus a
# 5 cm header allowance for title + subtitle.
out_lineplot <- out_path("lineplot_2016_vs_2022_all_measures", "png")
ggsave(filename = out_lineplot,
       plot     = grid_plot,
       width    = 30,
       height   = 5 + length(response_vars) * 8.2,
       units    = "cm",
       dpi      = 300)
cat("\nWrote line plot: ", out_lineplot, "\n", sep = "")


#===== PLOTS: lactose-standardised line plots (divide by lactose mean) ======

# Same layout as the previous figure, but each bag value is first divided by
# its own experiment's Lactose-bag mean. After standardisation Lactose is 1.0
# by construction, so it's dropped from the plotted remedies and replaced by
# a dashed grey reference line at y = 1. The reference line gets its own
# legend entry via a constant linetype mapping ("Lactose baseline") and a
# matching scale_linetype_manual() call -- this is the standard ggplot
# trick for putting a non-aesthetic geom into the legend cleanly.
#
# Standardisation is done at bag level BEFORE aggregation: for each
# (experimenter, experiment_number) cell we compute the mean of Lactose bags
# in that cell and divide every bag in that cell by it. Mean +/- SE are then
# computed on the standardised bag values, so error bars again represent
# bag-to-bag variability (now relative to the local Lactose baseline).

baseline_value <- 1  # divide mode -> Lactose collapses to 1.0

# Standardise at bag level, per experiment.
df_bags_std <- df_bags %>%
  mutate(
    experiment_number = as.integer(as.character(experiment_number)),
    potency           = as.character(potency)
  ) %>%
  group_by(experimenter, experiment_number) %>%
  mutate(across(all_of(response_vars),
                ~ {
                  lac_mean <- mean(.x[potency == "Lactose"], na.rm = TRUE)
                  .x / lac_mean
                })) %>%
  ungroup()

# Aggregate the standardised bag values to (cohort, experiment, remedy).
# Lactose is dropped because it sits on the dashed baseline by construction.
df_lineplot_std <- df_bags_std %>%
  filter(potency != "Lactose") %>%
  mutate(experimenter_label = ifelse(experimenter == "AS",
                                     "2016 data (Experiments 1-5)",
                                     "2022 data (Experiments 6-10)")) %>%
  group_by(experimenter_label, experiment_number, potency) %>%
  summarise(
    across(all_of(response_vars),
           list(mean = ~mean(.x, na.rm = TRUE),
                se   = ~plotrix::std.error(.x, na.rm = TRUE))),
    n_bags = n(),
    .groups = "drop"
  ) %>%
  mutate(potency = factor(potency,
                          levels = setdiff(potency_hierarchy, "Lactose")))

# Per-measure y-limits across both cohorts. Force the baseline value into
# the range so the dashed Lactose reference line is always visible even
# when all other remedies sit far above or below 1.
y_limits_std <- lapply(response_vars, function(v) {
  mn  <- df_lineplot_std[[paste0(v, "_mean")]]
  se  <- df_lineplot_std[[paste0(v, "_se")]]
  lo  <- min(c(mn - se, baseline_value), na.rm = TRUE)
  hi  <- max(c(mn + se, baseline_value), na.rm = TRUE)
  pad <- 0.05 * (hi - lo)
  c(lo - pad, hi + pad)
})
names(y_limits_std) <- response_vars

# Legend breaks for the colour scale: hierarchy minus Lactose so it doesn't
# appear as a colour swatch (Lactose is represented by the dashed line, not
# a colour).
legend_breaks_std <- setdiff(potency_hierarchy, "Lactose")

plot_list_std <- list()
for (var in response_vars) {
  for (col_idx in seq_along(cohort_labels)) {
    cohort     <- cohort_labels[col_idx]
    panel_data <- df_lineplot_std %>% filter(experimenter_label == cohort)

    p <- ggplot(panel_data,
                aes(x     = experiment_number,
                    y     = .data[[paste0(var, "_mean")]],
                    color = potency,
                    group = potency)) +
      # Dashed grey reference at the lactose baseline. Mapping linetype to a
      # constant string puts this geom into its own legend entry; the actual
      # dashed style is set by scale_linetype_manual() below.
      geom_hline(aes(yintercept = baseline_value,
                     linetype   = "Lactose baseline"),
                 color = "grey40", linewidth = 0.4) +
      geom_line(position = position_dodge(width = 0.3)) +
      geom_point(position = position_dodge(width = 0.3), size = 2) +
      geom_errorbar(aes(ymin = .data[[paste0(var, "_mean")]] -
                                .data[[paste0(var, "_se")]],
                        ymax = .data[[paste0(var, "_mean")]] +
                                .data[[paste0(var, "_se")]]),
                    width = 0.3,
                    position = position_dodge(width = 0.3)) +
      scale_color_manual(values = potency_colors,
                         breaks = legend_breaks_std,
                         name   = "Remedy") +
      # Second legend block for the dashed baseline. name = NULL keeps it
      # header-less so it sits cleanly under the Remedy legend.
      scale_linetype_manual(name   = NULL,
                            values = c("Lactose baseline" = "dashed")) +
      # Force legend stacking order: Remedy on top (order = 1), dashed
      # Lactose baseline below it (order = 2). Without this, ggplot picks
      # its own order across aesthetics and the baseline can land on top.
      guides(color    = guide_legend(order = 1),
             linetype = guide_legend(order = 2)) +
      scale_y_continuous(limits = y_limits_std[[var]]) +
      scale_x_continuous(breaks = 1:10) +
      labs(title = cohort,
           y     = paste0(var, "\n(/ lactose mean)"),
           x     = "Experiment Number") +
      theme_bw() +
      theme(
        plot.title         = element_text(size = 15, face = "bold"),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 13),
        legend.title       = element_text(size = 13),
        legend.text        = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )

    if (col_idx == 1) {
      p <- p + theme(legend.position = "none",
                     plot.margin     = margin(t = 5, r = 2, b = 5, l = 5))
    } else {
      p <- p + theme(axis.title.y = element_blank(),
                     axis.text.y  = element_blank(),
                     axis.ticks.y = element_blank(),
                     plot.margin  = margin(t = 5, r = 5, b = 5, l = 2))
    }

    plot_list_std[[length(plot_list_std) + 1]] <- p
  }
}

grid_plot_std <- patchwork::wrap_plots(plot_list_std,
                                       ncol = 2,
                                       nrow = length(response_vars)) +
  patchwork::plot_annotation(
    title    = "ASPS Cress Analysis (lactose-standardised, divide): All six potencies",
    subtitle = paste0("Each bag value = (treatment / per-experiment lactose mean); ",
                      "dashed line = lactose baseline (1); ",
                      "dataset version: ", DATASET_VER),
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    )
  )

out_lineplot_std <- out_path("lineplot_2016_vs_2022_lactose_std_divide", "png")
ggsave(filename = out_lineplot_std,
       plot     = grid_plot_std,
       width    = 30,
       height   = 5 + length(response_vars) * 8.2,
       units    = "cm",
       dpi      = 300)
cat("Wrote lactose-std line plot: ", out_lineplot_std, "\n", sep = "")


#===== SUMMARY ==============================================================

cat("\n\n", strrep("#", 80), "\n", sep = "")
cat("DONE\n")
cat(strrep("#", 80), "\n", sep = "")
cat("Output folder: ", out_dir, "\n", sep = "")
cat("ANOVA xlsx   : ", basename(out_anova), "\n", sep = "")
cat("Post-hoc xlsx: ", basename(out_posthoc), "\n", sep = "")
cat("Plots        : 24 PNG files (4 vars * 3 groups * 2 layouts)\n")
cat("Line plot    : ", basename(out_lineplot), "\n", sep = "")
cat("Line plot std: ", basename(out_lineplot_std), "\n", sep = "")
for (group_key in names(analysis_groups)) {
  cat(sprintf("  %s: %d bags\n", group_key, nrow(analysis_groups[[group_key]]$data)))
}
