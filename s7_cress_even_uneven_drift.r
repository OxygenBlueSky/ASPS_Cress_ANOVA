# s7_cress_even_uneven_drift.r
#
# Pipeline step 7: AS-only (ASPS 1-5) drift check based on bag-number
# parity. AS measured all even-numbered bags first, then all uneven
# bags; if she drifted over time (fatigue, calibration, technique
# change), the even/uneven split should show up as a systematic
# difference. Confounders exist (potency is assigned per bag), but
# pooling over potency and running a 2-way ANOVA with experiment_number
# and bag_parity isolates the order-of-measurement question.
#
# This is a diagnostic script. It does NOT make a hypothesis test
# about the remedies -- potency is collapsed away.
#
# Toggles in CONFIG:
#   USE_SKEWNESS_CORRECTED   -- raw s3 file vs latest s4 truncated file
#   ANALYSIS_LEVEL           -- "bag" (recommended) vs "seedling"
#                               (pseudoreplicated comparator)
#
# Inputs : either the s3 v1v2 combined file or an s4 skewness-corrected
#          file, same resolver pattern as s5.
# Outputs (in outputs/<date>_s7_evenuneven_.../):
#   - ANOVA xlsx (AS only, 4 vars * 3 p-columns, colour-coded)
#   - Descriptive table xlsx (AS only, one sheet per variable, mean/sd/
#     se/n per experiment x parity cell, plus diff = mean_uneven -
#     mean_even)
#   - ONE combined line-plot PNG: 4 rows (one per response variable) x
#     2 cols (AS analysis on the left, ASPS 6-10 JZ comparator on the
#     right). JZ is always v2 regardless of DATASET_VER (JZ has no v1
#     measurements) and serves as a comparator panel. JZ's measurement
#     order is unknown (not recorded in notes), so the JZ side is just
#     a reference -- it can't function as a clean negative control.


library(readxl)
library(car)        # Anova(type="III")
library(here)
library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(patchwork)  # 4-row x 2-col (AS | JZ) line-plot grid
library(plotrix)    # std.error() = sd / sqrt(n) for SE error bars


#===== CONFIG ===============================================================

SCRIPT_TAG             <- "s7"
DATASET_VER            <- "v1"   # "v1" | "v2" | "v1v2"
USE_SKEWNESS_CORRECTED <- FALSE   # TRUE -> read latest s4 output for DATASET_VER
RUN_DATE               <- format(Sys.Date(), "%Y%m%d")

# Unit of analysis. Same toggle as s5.
#   "bag"      -- one row per bag mean (~16 seeds averaged first); bag is
#                 the experimental unit, so the ANOVA is statistically
#                 defensible.
#   "seedling" -- one row per seed; pseudoreplicated comparator. Seeds
#                 within a bag are NOT independent, so p-values are
#                 optimistic. Useful only to mirror the s5 contrast.
ANALYSIS_LEVEL <- "seedling"   # "bag" | "seedling"

# Response variables analysed. As in s5, when USE_SKEWNESS_CORRECTED is
# TRUE we copy the matching T<var>_cut* column over the original name.
response_vars <- c("sprout_length", "root_length",
                   "seedling_length", "root_sprout_ratio")

# Inputs (paths identical to s5).
INPUT_S3_BASENAME <- "cress_length_ASPS_1-10_alldata_decoded_v1v2.xlsx"
INPUT_S3_PARENT   <- "cress_combine_files"
S4_OUTPUTS_ROOT   <- "outputs"


#===== DERIVED PATHS (don't edit) ===========================================

SCRIPT_PURPOSE <- paste0("evenuneven_",
                         if (USE_SKEWNESS_CORRECTED) "skewcorr" else "raw")

if (!ANALYSIS_LEVEL %in% c("bag", "seedling")) {
  stop("ANALYSIS_LEVEL must be 'bag' or 'seedling'; got: ", ANALYSIS_LEVEL)
}
LEVEL_TAG <- paste0(ANALYSIS_LEVEL, "level")   # "baglevel" | "seedlinglevel"

out_dir <- file.path(
  S4_OUTPUTS_ROOT,
  paste(RUN_DATE, SCRIPT_TAG, SCRIPT_PURPOSE, DATASET_VER, LEVEL_TAG,
        sep = "_")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_path <- function(suffix, ext) {
  file.path(
    out_dir,
    paste0(RUN_DATE, "_", SCRIPT_TAG, "_", DATASET_VER, "_", LEVEL_TAG,
           "_", suffix, ".", ext)
  )
}


#===== RESOLVE INPUT (same resolvers as s5) =================================

# Case 1: raw s3 file. Most-recent <date>_cress_combined/ under
# cress_combine_files/.
# Case 2: skewness-corrected. Most recent <date>_s4_skewness_<DATASET_VER>/
# under outputs/, read its *_skewness_corr.xlsx.
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
cat("Analysis level: ", ANALYSIS_LEVEL,
    " (", if (ANALYSIS_LEVEL == "bag") "experimental unit -- defensible"
          else "pseudoreplicated -- inflated p-values expected",
    ")\n", sep = "")
cat("Cohort        : AS only (ASPS 1-5) -- the even-then-uneven order ",
    "was applied by AS only\n", sep = "")
cat("Output folder : ", out_dir, "\n\n", sep = "")

df_raw_full <- read_excel(input_path, sheet = "Sheet 1")


#===== SWAP TO TRUNCATED COLUMNS WHEN SKEWNESS-CORRECTED ====================

# s4 appends T<var>_cut<value> columns rather than overwriting the
# originals; copy them back over the original column name so downstream
# code stays uniform. Variables with no truncated counterpart
# (root_sprout_ratio) flow through unchanged. Applied on df_raw_full so
# BOTH the AS analysis frame and the JZ comparator frame inherit the
# corrected values.
if (USE_SKEWNESS_CORRECTED) {
  for (var in response_vars) {
    trunc_cols <- grep(paste0("^T", var, "_cut"), colnames(df_raw_full),
                       value = TRUE)
    if (length(trunc_cols) >= 1) {
      df_raw_full[[var]] <- df_raw_full[[trunc_cols[1]]]
      cat("  swapped ", var, " <- ", trunc_cols[1], "\n", sep = "")
    }
  }
}


#===== FILTER FOR AS (user-selected version) ================================

# Filter for the AS analysis using the user-selected DATASET_VER (v1 / v2 /
# v1v2). df_raw_full is kept intact so the JZ comparator block below can
# apply its own filter (always v2 -- JZ has no v1 measurements).
df_raw <- switch(DATASET_VER,
  "v1"   = df_raw_full[df_raw_full$in_v1_analysis, ],
  "v2"   = df_raw_full[df_raw_full$in_v2_analysis, ],
  "v1v2" = df_raw_full,
  stop("DATASET_VER must be 'v1', 'v2', or 'v1v2'; got: ", DATASET_VER)
)
cat("AS-side rows after DATASET_VER (", DATASET_VER, ") filter: ",
    nrow(df_raw), "\n", sep = "")


#===== PARSE EXPERIMENT, DERIVE PARITY, FILTER TO AS ========================

# exp_no in the data is "<experiment_number>_<potency_code>" (e.g. "3_A").
# bag is an integer column written by s3; parity is bag %% 2.
# Filter to AS (experiments 1-5) here -- JZ has no even-then-uneven
# ordering, so an even/uneven split there is just noise and would
# pollute the descriptive view if mixed in.
df_parsed <- df_raw %>%
  separate(exp_no, into = c("experiment_number", "potency_code"),
           sep = "_", remove = FALSE) %>%
  mutate(
    experiment_number = as.integer(experiment_number),
    bag               = as.integer(bag),
    experimenter      = ifelse(experiment_number <= 5, "AS", "JZ"),
    bag_parity        = ifelse(bag %% 2 == 0, "even", "uneven")
  ) %>%
  filter(experimenter == "AS")

cat("AS-only seed-level rows: ", nrow(df_parsed), "\n", sep = "")


#===== CALCULATE BAG-LEVEL MEANS ============================================

# bag_parity is constant within a bag, so adding it to the grouping
# keys just carries the column through without splitting anything.
df_bags <- df_parsed %>%
  group_by(experiment_number, experimenter, potency_code, potency, bag,
           bag_parity, exp_no, label) %>%
  summarise(
    n_seeds           = n(),
    sprout_length     = mean(sprout_length,     na.rm = TRUE),
    root_length       = mean(root_length,       na.rm = TRUE),
    seedling_length   = mean(seedling_length,   na.rm = TRUE),
    root_sprout_ratio = mean(root_sprout_ratio, na.rm = TRUE),
    .groups = "drop"
  )

cat("Bag-level rows (AS only): ", nrow(df_bags),
    "  (mean ", round(mean(df_bags$n_seeds), 1),
    " seeds/bag, range ", min(df_bags$n_seeds),
    "-", max(df_bags$n_seeds), ")\n", sep = "")


#===== SELECT ANALYSIS FRAME ================================================

# Everything downstream consumes df_analysis. Single switch point.
if (ANALYSIS_LEVEL == "bag") {
  df_analysis <- df_bags
} else {
  df_analysis <- df_parsed %>%
    select(experiment_number, experimenter, potency_code, potency, bag,
           bag_parity, exp_no, label, all_of(response_vars))
  cat("Seedling-level rows (AS only): ", nrow(df_analysis), "\n", sep = "")
}


#===== JZ COMPARATOR FRAME (always v2, for plotting only) ===================

# JZ (ASPS 6-10) did NOT use the even-then-uneven measurement order, so
# even/uneven there is expected to be ~null. The JZ block is built no
# matter what DATASET_VER the user picked for AS, so the comparator plot
# is always available. JZ has no v1 measurements -- only v2 exists --
# so we always filter on in_v2_analysis here.
df_raw_jz   <- df_raw_full[df_raw_full$in_v2_analysis, ]
df_parsed_jz <- df_raw_jz %>%
  separate(exp_no, into = c("experiment_number", "potency_code"),
           sep = "_", remove = FALSE) %>%
  mutate(
    experiment_number = as.integer(experiment_number),
    bag               = as.integer(bag),
    experimenter      = ifelse(experiment_number <= 5, "AS", "JZ"),
    bag_parity        = ifelse(bag %% 2 == 0, "even", "uneven")
  ) %>%
  filter(experimenter == "JZ")

df_bags_jz <- df_parsed_jz %>%
  group_by(experiment_number, experimenter, potency_code, potency, bag,
           bag_parity, exp_no, label) %>%
  summarise(
    n_seeds           = n(),
    sprout_length     = mean(sprout_length,     na.rm = TRUE),
    root_length       = mean(root_length,       na.rm = TRUE),
    seedling_length   = mean(seedling_length,   na.rm = TRUE),
    root_sprout_ratio = mean(root_sprout_ratio, na.rm = TRUE),
    .groups = "drop"
  )

# Mirror AS's ANALYSIS_LEVEL switch so the comparator panels show the
# same unit (bag-level vs seedling-level) as the AS panels.
if (ANALYSIS_LEVEL == "bag") {
  df_analysis_jz <- df_bags_jz
} else {
  df_analysis_jz <- df_parsed_jz %>%
    select(experiment_number, experimenter, potency_code, potency, bag,
           bag_parity, exp_no, label, all_of(response_vars))
}
cat("JZ comparator rows (always v2, ", ANALYSIS_LEVEL, "-level): ",
    nrow(df_analysis_jz), "\n", sep = "")


#===== INVENTORY / SANITY CHECK =============================================

# Two views printed:
#   (1) compact (experiment x parity) cell counts so balance is visible
#       at a glance before the ANOVA runs;
#   (2) full per-group bag listing -- every bag included, sorted by
#       experiment / potency / bag, with n_seeds. Lets you eyeball that
#       the "even" group really does contain only bags 2, 4, 6, ...
#       and that all five experiments and all six potencies show up.
# Both views use df_bags (the natural unit for "which bags are in
# which group?"), even in seedling-level mode -- the question being
# answered is about bag membership, not row count.

cat("\n", strrep("#", 80), "\n", sep = "")
cat("INVENTORY -- bags included in each parity group (AS only)\n")
cat(strrep("#", 80), "\n", sep = "")

cell_counts <- df_bags %>%
  group_by(experiment_number, bag_parity) %>%
  summarise(n_bags = n(), n_seeds_total = sum(n_seeds), .groups = "drop") %>%
  pivot_wider(names_from = bag_parity,
              values_from = c(n_bags, n_seeds_total),
              values_fill = 0) %>%
  arrange(experiment_number)

cat("\n(1) Cell counts -- bags per (experiment x parity):\n")
print.data.frame(cell_counts, row.names = FALSE)

# Full bag listing per parity group. Printing the whole table so the
# user can confirm that no even bag accidentally landed in the uneven
# group (or vice-versa) and that coverage across (experiment, potency)
# is what they expect.
bag_listing <- df_bags %>%
  select(bag_parity, experiment_number, potency_code, bag, n_seeds) %>%
  arrange(bag_parity, experiment_number, potency_code, bag)

for (parity_group in c("even", "uneven")) {
  group_table <- bag_listing %>% filter(bag_parity == parity_group)
  cat("\n(2) Full bag listing -- ", toupper(parity_group),
      " group (", nrow(group_table), " bags):\n", sep = "")
  print.data.frame(group_table %>% select(-bag_parity), row.names = FALSE)
}

cat("\nGrand totals: ",
    sum(df_bags$bag_parity == "even"),   " even bags, ",
    sum(df_bags$bag_parity == "uneven"), " uneven bags\n", sep = "")


#===== FACTORISE GROUPING VARS ==============================================

# contr.sum requires factors for Anova(type="III") to be interpretable.
df_analysis$experiment_number <- as.factor(df_analysis$experiment_number)
df_analysis$bag_parity        <- as.factor(df_analysis$bag_parity)


#===== HELPERS ==============================================================

sig_stars <- function(p) {
  if (is.na(p))     return("")
  if (p < 0.001)    return("***")
  if (p < 0.01)     return("**")
  if (p < 0.05)     return("*")
  if (p < 0.10)     return(".")
  return("ns")
}


#===== DESCRIPTIVE TABLE ====================================================

# For each response variable: per-cell (experiment x parity) summary
# (n, mean, sd, se) plus a per-experiment "diff" = mean_uneven -
# mean_even, which is the eyeball drift estimate the ANOVA formalises.

cat("\n", strrep("#", 80), "\n", sep = "")
cat("DESCRIPTIVE TABLE (mean +/- SE per experiment x parity)\n")
cat(strrep("#", 80), "\n", sep = "")

# Returns a wide-format data frame: one row per experiment, columns
# laid out as even_n, even_mean, even_sd, even_se, uneven_..., diff.
# Used both for console printing and as the body of one xlsx sheet.
build_descriptives <- function(df, variable) {
  long <- df %>%
    group_by(experiment_number, bag_parity) %>%
    summarise(
      n    = sum(!is.na(.data[[variable]])),
      mean = mean(.data[[variable]], na.rm = TRUE),
      sd   = sd(.data[[variable]],   na.rm = TRUE),
      se   = plotrix::std.error(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    )

  wide <- long %>%
    pivot_wider(names_from = bag_parity,
                values_from = c(n, mean, sd, se),
                names_glue = "{bag_parity}_{.value}") %>%
    arrange(experiment_number) %>%
    mutate(diff_uneven_minus_even = uneven_mean - even_mean)

  # Reorder columns into a readable block: even-stats, uneven-stats, diff.
  col_order <- c("experiment_number",
                 "even_n", "even_mean", "even_sd", "even_se",
                 "uneven_n", "uneven_mean", "uneven_sd", "uneven_se",
                 "diff_uneven_minus_even")
  # Some cells may be missing entirely; only keep columns that exist.
  wide[, intersect(col_order, names(wide))]
}

descriptives_list <- list()
for (var in response_vars) {
  cat("\n--- ", toupper(var), " ---\n", sep = "")
  tbl <- build_descriptives(df_analysis, var)
  print.data.frame(tbl, row.names = FALSE, digits = 4)
  descriptives_list[[var]] <- tbl
}


#===== EXPORT DESCRIPTIVES (xlsx, one sheet per variable) ==================

wb_desc      <- createWorkbook()
style_header <- createStyle(textDecoration = "bold", fgFill = "#D3D3D3")
style_4dec   <- createStyle(numFmt = "0.0000")

for (var in response_vars) {
  tbl <- descriptives_list[[var]]
  # openxlsx sheet-name length limit is 31 chars; response_vars are short
  # enough that no truncation is needed for the current set.
  sheet <- var
  addWorksheet(wb_desc, sheet)
  writeData(wb_desc, sheet,
            paste0("Descriptives [", toupper(ANALYSIS_LEVEL),
                   "-LEVEL] -- ", var, " (AS only, ASPS 1-5)"),
            startRow = 1, startCol = 1)
  writeData(wb_desc, sheet, tbl, startRow = 3, rowNames = FALSE)
  addStyle(wb_desc, sheet, style_header,
           rows = 3, cols = seq_len(ncol(tbl)), gridExpand = TRUE)
  # Numeric-format the mean/sd/se/diff columns (everything except the
  # experiment number and the n columns).
  fmt_cols <- which(grepl("(mean|sd|se|diff)", names(tbl)))
  if (length(fmt_cols) > 0) {
    addStyle(wb_desc, sheet, style_4dec,
             rows = 4:(3 + nrow(tbl)),
             cols = fmt_cols,
             gridExpand = TRUE, stack = TRUE)
  }
  setColWidths(wb_desc, sheet, cols = 1:ncol(tbl),
               widths = c(18, rep(12, ncol(tbl) - 1)))
}

out_desc <- out_path("descriptives", "xlsx")
saveWorkbook(wb_desc, out_desc, overwrite = TRUE)
cat("\nWrote descriptives xlsx: ", out_desc, "\n", sep = "")


#===== 2-WAY ANOVA (Type-III SS) ============================================

# Model: var ~ experiment_number * bag_parity.
# - experiment_number main effect = between-experiment drift (already
#   reported by s5; here it's a covariate-like control).
# - bag_parity main effect       = the order-of-measurement drift (the
#                                  question this script exists for).
# - interaction                  = "did the even/uneven gap differ
#                                  between experiments?".
# contr.sum is required so Type-III SS for main effects is testable
# independently of the interaction term.

cat("\n", strrep("#", 80), "\n", sep = "")
cat("ANOVA (Type-III SS): var ~ experiment_number * bag_parity\n")
if (ANALYSIS_LEVEL == "seedling") {
  cat("WARNING: seedling-level -- seeds within a bag are not ",
      "independent; p-values are optimistic.\n", sep = "")
}
cat(strrep("#", 80), "\n", sep = "")

options(contrasts = c("contr.sum", "contr.poly"))

# Three p-values per response variable, captured into a 4-row table for
# export. Row order matches response_vars.
anova_summary <- data.frame(
  variable          = character(),
  p_experiment      = numeric(),
  p_bag_parity      = numeric(),
  p_interaction     = numeric(),
  stringsAsFactors  = FALSE
)

for (var in response_vars) {
  cat("\n--- ", toupper(var), " ---\n", sep = "")
  model <- aov(as.formula(paste(var, "~ experiment_number * bag_parity")),
               data = df_analysis)
  a <- Anova(model, type = "III")
  print(a, digits = 6)

  p_exp <- a["experiment_number",            "Pr(>F)"]
  p_par <- a["bag_parity",                   "Pr(>F)"]
  p_int <- a["experiment_number:bag_parity", "Pr(>F)"]

  cat(sprintf("\n  Experiment:  p = %.6f  %s\n", p_exp, sig_stars(p_exp)))
  cat(sprintf("  Bag parity:  p = %.6f  %s\n",   p_par, sig_stars(p_par)))
  cat(sprintf("  Interaction: p = %.6f  %s\n",   p_int, sig_stars(p_int)))

  anova_summary <- rbind(anova_summary, data.frame(
    variable        = var,
    p_experiment    = p_exp,
    p_bag_parity    = p_par,
    p_interaction   = p_int,
    stringsAsFactors = FALSE
  ))
}


#===== EXPORT ANOVA SUMMARY (xlsx, colour-coded) ============================

# Colour cues match s5: red < 0.01, orange < 0.05, lilac < 0.10.
wb_anova     <- createWorkbook()
style_red    <- createStyle(fgFill = "#FFB3BA")
style_orange <- createStyle(fgFill = "#FFDFBA")
style_lilac  <- createStyle(fgFill = "#E0BBE4")

fill_p <- function(wb, sheet, row, col, p) {
  if (is.na(p)) return(invisible())
  s <- if (p < 0.01) style_red
       else if (p < 0.05) style_orange
       else if (p < 0.10) style_lilac
       else NULL
  if (!is.null(s)) addStyle(wb, sheet, s, rows = row, cols = col, stack = TRUE)
}

sheet <- "ANOVA"
addWorksheet(wb_anova, sheet)
writeData(wb_anova, sheet,
          paste0("2-way ANOVA [", toupper(ANALYSIS_LEVEL),
                 "-LEVEL] -- experiment_number * bag_parity (AS only)"),
          startRow = 1, startCol = 1)
if (ANALYSIS_LEVEL == "seedling") {
  writeData(wb_anova, sheet,
            "Seedling-level (pseudoreplicated -- see s6 ICC).",
            startRow = 2, startCol = 1)
}
writeData(wb_anova, sheet, anova_summary, startRow = 4, rowNames = FALSE)
addStyle(wb_anova, sheet, style_header,
         rows = 4, cols = 1:4, gridExpand = TRUE)

# Colour each p-cell. Rows in the sheet are offset by +4 (title +
# optional caveat + header).
for (r in seq_len(nrow(anova_summary))) {
  for (c in 2:4) {
    fill_p(wb_anova, sheet, r + 4, c, anova_summary[r, c])
  }
}
addStyle(wb_anova, sheet, style_4dec,
         rows = 5:(4 + nrow(anova_summary)),
         cols = 2:4, gridExpand = TRUE, stack = TRUE)
setColWidths(wb_anova, sheet, cols = 1:4, widths = c(22, 16, 16, 16))

legend_row <- nrow(anova_summary) + 7
writeData(wb_anova, sheet, "Color Legend:",
          startRow = legend_row,     startCol = 1)
writeData(wb_anova, sheet, "Light lilac = p<0.10",
          startRow = legend_row + 1, startCol = 1)
writeData(wb_anova, sheet, "Light orange = p<0.05",
          startRow = legend_row + 2, startCol = 1)
writeData(wb_anova, sheet, "Light red = p<0.01",
          startRow = legend_row + 3, startCol = 1)
addStyle(wb_anova, sheet, style_lilac,  rows = legend_row + 1, cols = 1)
addStyle(wb_anova, sheet, style_orange, rows = legend_row + 2, cols = 1)
addStyle(wb_anova, sheet, style_red,    rows = legend_row + 3, cols = 1)

out_anova <- out_path("anova_summary", "xlsx")
saveWorkbook(wb_anova, out_anova, overwrite = TRUE)
cat("\nWrote ANOVA summary xlsx: ", out_anova, "\n", sep = "")


#===== LINE PLOTS (single combined image: 4 rows x 2 cols, AS | JZ) ========

# Layout mirrors s5's 2016-vs-2022 grid: one row per response variable,
# two columns (AS left = ASPS 1-5 where the even-then-uneven order was
# applied; JZ right = ASPS 6-10, always v2, included as a comparator).
# JZ's measurement order is unknown (not recorded in notes), so any
# even/uneven gap on the JZ side cannot be attributed to drift -- it's
# just an extra reference panel rather than a clean negative control.
#
# Per-row y-limits are computed across BOTH cohorts so the two columns
# in the same row are directly comparable; the right column drops its
# y-axis to give panels more horizontal room.

# Build per-cohort (experiment, parity) summary frames. Same recipe for
# both -- factor levels locked so colour mapping is consistent.
summarise_cohort <- function(df) {
  df %>%
    mutate(experiment_number = as.integer(as.character(experiment_number)),
           bag_parity        = as.character(bag_parity)) %>%
    group_by(experiment_number, bag_parity) %>%
    summarise(across(all_of(response_vars),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          se   = ~plotrix::std.error(.x, na.rm = TRUE))),
              n_rows = n(),
              .groups = "drop") %>%
    mutate(bag_parity = factor(bag_parity, levels = c("even", "uneven")))
}

df_plot_as <- summarise_cohort(df_analysis)
df_plot_jz <- summarise_cohort(df_analysis_jz)

parity_colors <- c("even" = "#1f77b4", "uneven" = "#d62728")

# Per-variable shared y-limits across both cohorts. 5% padding top/bottom
# so error bars don't kiss the panel edge.
y_limits <- lapply(response_vars, function(v) {
  mn_as <- df_plot_as[[paste0(v, "_mean")]]
  se_as <- df_plot_as[[paste0(v, "_se")]]
  mn_jz <- df_plot_jz[[paste0(v, "_mean")]]
  se_jz <- df_plot_jz[[paste0(v, "_se")]]
  lo  <- min(c(mn_as - se_as, mn_jz - se_jz), na.rm = TRUE)
  hi  <- max(c(mn_as + se_as, mn_jz + se_jz), na.rm = TRUE)
  pad <- 0.05 * (hi - lo)
  c(lo - pad, hi + pad)
})
names(y_limits) <- response_vars

# Build a single panel for one (variable, cohort) pair. x-axis range is
# fixed to the cohort's experiment numbers (1-5 for AS, 6-10 for JZ).
build_panel <- function(panel_df, var, cohort_title, x_breaks,
                        is_right_col) {
  mean_col <- paste0(var, "_mean")
  se_col   <- paste0(var, "_se")
  p <- ggplot(panel_df,
              aes(x     = experiment_number,
                  y     = .data[[mean_col]],
                  color = bag_parity,
                  group = bag_parity)) +
    geom_line(position = position_dodge(width = 0.2), linewidth = 0.8) +
    geom_point(position = position_dodge(width = 0.2), size = 2.5) +
    geom_errorbar(aes(ymin = .data[[mean_col]] - .data[[se_col]],
                      ymax = .data[[mean_col]] + .data[[se_col]]),
                  width = 0.25,
                  position = position_dodge(width = 0.2)) +
    scale_color_manual(values = parity_colors, name = "Bag parity") +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(limits = y_limits[[var]]) +
    labs(title = cohort_title,
         x     = "Experiment number",
         y     = paste0(var, " (mean)")) +
    theme_bw() +
    theme(
      plot.title         = element_text(size = 13, face = "bold"),
      axis.title         = element_text(size = 12),
      axis.text          = element_text(size = 11),
      legend.title       = element_text(size = 11),
      legend.text        = element_text(size = 10),
      panel.grid.minor.x = element_blank()
    )

  if (is_right_col) {
    # JZ column: drop y-axis (shared scale is shown on AS side) and
    # tighten left margin so the two columns sit close together.
    p <- p + theme(axis.title.y = element_blank(),
                   axis.text.y  = element_blank(),
                   axis.ticks.y = element_blank(),
                   plot.margin  = margin(t = 5, r = 5, b = 5, l = 2))
  } else {
    p <- p + theme(legend.position = "none",
                   plot.margin     = margin(t = 5, r = 2, b = 5, l = 5))
  }
  p
}

# Assemble in row-major order: for each variable, AS panel then JZ panel.
# patchwork::wrap_plots(ncol = 2) lays the resulting list out as a
# 4-row x 2-col grid that auto-aligns panel widths and heights.
plot_list <- list()
for (var in response_vars) {
  plot_list[[length(plot_list) + 1]] <-
    build_panel(df_plot_as, var,
                cohort_title = "ASPS 1-5 (AS, even-then-uneven order)",
                x_breaks     = 1:5,
                is_right_col = FALSE)
  plot_list[[length(plot_list) + 1]] <-
    build_panel(df_plot_jz, var,
                cohort_title = "ASPS 6-10 (JZ, comparator -- unknown measurement order)",
                x_breaks     = 6:10,
                is_right_col = TRUE)
}

grid_plot <- patchwork::wrap_plots(plot_list,
                                   ncol = 2,
                                   nrow = length(response_vars)) +
  patchwork::plot_annotation(
    title    = paste0("Even vs uneven bags (", ANALYSIS_LEVEL,
                      "-level): AS analysis | JZ comparator"),
    # Two-line subtitle: line 1 = which data is on each side, line 2 =
    # what the error bars and y-axis mean. Splitting at "\n" stops the
    # subtitle running off the left and right edges of a 30 cm canvas.
    subtitle = paste0(
      "AS (left, ASPS 1-5): DATASET_VER = ", DATASET_VER,
      if (USE_SKEWNESS_CORRECTED) ", skewness-corrected" else ", raw",
      ".  JZ (right, ASPS 6-10): always v2 (no v1 exists for JZ), ",
      "shown as a comparator -- measurement order unknown.",
      "\n",
      "Error bars = +/- 1 SE across ",
      if (ANALYSIS_LEVEL == "bag") "bags" else "seedlings",
      " per (experiment, parity) cell. Shared y-axis per row."),
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 17, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5)
    )
  )

# Size matches the s5 line-plot output: 30 cm wide, ~8.2 cm per row plus
# a header allowance for title + (now two-line) subtitle.
out_lineplot <- out_path("lineplot_AS_vs_JZ_all_measures", "png")
ggsave(filename = out_lineplot,
       plot     = grid_plot,
       width    = 30,
       height   = 6 + length(response_vars) * 8.2,
       units    = "cm",
       dpi      = 300)
cat("Wrote combined line plot: ", out_lineplot, "\n", sep = "")


#===== SUMMARY ==============================================================

cat("\n", strrep("#", 80), "\n", sep = "")
cat("DONE\n")
cat(strrep("#", 80), "\n", sep = "")
cat("Cohort        : AS only (ASPS 1-5)\n")
cat("Analysis level: ", ANALYSIS_LEVEL, "\n", sep = "")
cat("Input         : ", if (USE_SKEWNESS_CORRECTED) "skewness-corrected"
                        else "raw s3", "\n", sep = "")
cat("Output folder : ", out_dir, "\n", sep = "")
cat("Descriptives  : ", basename(out_desc),  "\n", sep = "")
cat("ANOVA summary : ", basename(out_anova), "\n", sep = "")
cat("Line plot     : ", basename(out_lineplot),
    " (4 rows x 2 cols, AS | JZ)\n", sep = "")
cat("Rows analysed : AS = ", nrow(df_analysis),
    ", JZ comparator = ", nrow(df_analysis_jz), " (",
    if (ANALYSIS_LEVEL == "bag") "bags" else "seedlings", ")\n", sep = "")
