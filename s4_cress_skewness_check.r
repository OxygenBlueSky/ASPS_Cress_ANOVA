# s4_cress_skewness_check.r
#
# Pipeline step 4: pick a left-side cutoff per response variable that brings
# the distribution close to symmetric (skewness ~ 0), then write a copy of
# the dataset with truncated columns added.
#
# Why a left cutoff: very small seedling/sprout/root lengths are dominated
# by measurement noise and unsprouted seeds, producing a long left tail
# (negative skew). Removing them above a per-variable threshold pulls the
# distribution back toward symmetry without touching the biological signal
# in the bulk of the data.
#
# Inputs : combined+decoded v1v2 xlsx from s3 (cress_combine_files/), filtered
#          by DATASET_VER via the in_v1_analysis / in_v2_analysis columns.
#          DATASET_VER = "v2" means "v2 ASPS 1-5 + v1 ASPS 6-10" -- the
#          best-available view, set up in s3.
# Outputs: per-variable before/after histograms, console-cutoff scan, and
#          one xlsx with `T<var>_cut<value>` columns appended.
#
# Adapted from Paul's original skewness script. No statistical changes.


library(readxl)
library(here)
library(openxlsx)
library(moments)  # skewness
library(ggplot2)


#===== CONFIG ===============================================================

SCRIPT_TAG     <- "s4"
SCRIPT_PURPOSE <- "skewness"
DATASET_VER    <- "v2"   # "v1" | "v2" | "v1v2"
RUN_DATE       <- format(Sys.Date(), "%Y%m%d")

# s3 output is the single source of truth for downstream analyses. We always
# read the v1v2 combined xlsx and select rows by membership boolean below.
INPUT_S3_BASENAME <- "cress_length_ASPS_1-10_alldata_decoded_v1v2.xlsx"
INPUT_S3_PARENT   <- "cress_combine_files"  # holds <date>_cress_combined/

# Per-variable left-cutoffs. Set after inspecting the cutoff scan that prints
# at the start of each variable's loop iteration.
cutoffs <- list(
  seedling_length = 8.0,
  sprout_length   = 2.8,
  root_length     = 4.8
)
response_vars <- names(cutoffs)


#===== DERIVED PATHS (don't edit) ===========================================

# Output folder mirrors the convention used by the cress_combine_files
# scripts: outputs/<date>_<tag>_<purpose>_<datasetver>/
out_dir <- file.path(
  "outputs",
  paste(RUN_DATE, SCRIPT_TAG, SCRIPT_PURPOSE, DATASET_VER, sep = "_")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Build a standard output filepath: <date>_<tag>_<datasetver>_<suffix>.<ext>
out_path <- function(suffix, ext) {
  file.path(
    out_dir,
    paste0(RUN_DATE, "_", SCRIPT_TAG, "_", DATASET_VER, "_", suffix, ".", ext)
  )
}


#===== RESOLVE INPUT ========================================================

# Locate the most-recent <date>_cress_combined/ folder under cress_combine_files/
# and read the v1v2 combined xlsx from it. Folder-name sort is fine because
# the date prefix is YYYYMMDD (lexicographic == chronological).
resolve_s3_input <- function() {
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

input_path <- resolve_s3_input()
cat("Reading: ", input_path, "\n", sep = "")

df <- read_excel(input_path, sheet = "Sheet 1")

# Filter by dataset version using the membership flags written by s3.
# "v1v2" keeps every row (both streams side-by-side), useful for comparison
# but rarely the right thing to feed into ANOVA -- the same biological
# samples appear twice.
df <- switch(DATASET_VER,
  "v1"   = df[df$in_v1_analysis, ],
  "v2"   = df[df$in_v2_analysis, ],
  "v1v2" = df,
  stop("DATASET_VER must be 'v1', 'v2', or 'v1v2'; got: ", DATASET_VER)
)
cat("Rows after dataset_ver filter (", DATASET_VER, "): ", nrow(df), "\n", sep = "")

# Sanity check: every retained row should have a resolved (exp_no, bag).
# s3 drops the unresolved-filename rows, so a non-zero count here means the
# upstream guarantee broke and bag-level aggregation would silently lose data.
n_missing_bagkey <- sum(is.na(df$exp_no) | is.na(df$bag))
cat("Rows missing exp_no/bag (should be 0): ", n_missing_bagkey, "\n", sep = "")


#===== HELPERS ==============================================================

# Plot a density-overlaid histogram for a single column, captioned with
# the current skewness. Used both for "before cutoff" and "after cutoff".
make_hist <- function(values, var_label, caption_extra = "") {
  skw <- round(skewness(values, na.rm = TRUE), 2)
  ggplot(data.frame(x = values), aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)),
                   colour = 1, fill = "white", bins = 30) +
    geom_density(linewidth = 1, colour = 4, fill = 4, alpha = 0.2) +
    xlim(min(values, na.rm = TRUE), max(values, na.rm = TRUE)) +
    labs(x = paste0(var_label, " (cm)"),
         caption = paste0("Skewness = ", skw, caption_extra))
}

# Print the skewness cutoff scan to console AND capture it as plain text
# next to the histograms, so the chosen cutoff has a paper trail.
write_cutoff_scan <- function(values, var, scan_seq) {
  lines <- c(
    paste0("Cutoff scan for ", var,
           "  (rows with ", var, " > cutoff are kept)"),
    paste0("Initial skewness = ",
           round(skewness(values, na.rm = TRUE), 2),
           "   n = ", sum(!is.na(values)))
  )
  for (i in scan_seq) {
    keep <- values[values > i & !is.na(values)]
    skw  <- round(skewness(keep), 2)
    lines <- c(lines,
               sprintf("  cutoff > %5.2f  ->  skewness = %5.2f   n = %d",
                       i, skw, length(keep)))
  }
  writeLines(lines, out_path(paste0("cutoff_scan_", var), "txt"))
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
}

# Aggregate a seed-level numeric vector to one mean per (exp_no, bag). Bags
# that end up with zero non-NA seeds (can happen for the post-cutoff vector
# when an entire bag's seeds are below the cutoff) come back as NaN from
# mean(..., na.rm = TRUE); we coerce those to NA so skewness/ggplot drop
# them rather than choking. The order of the returned vector doesn't matter
# -- it's only consumed by histogram + skewness, both of which are
# order-invariant.
bag_means <- function(values, exp_no, bag) {
  key <- paste(exp_no, bag, sep = "__")
  means <- tapply(values, key, function(v) mean(v, na.rm = TRUE))
  means <- as.numeric(means)
  means[is.nan(means)] <- NA
  means
}


# Per-variable scan ranges. Match the original script's choices so the scans
# remain comparable across runs.
scan_ranges <- list(
  seedling_length = 0:10,
  sprout_length   = seq(0, 3.4, by = 0.2),
  root_length     = seq(0, 5.0, by = 0.2)
)


#===== APPLY SKEWNESS CORRECTION ============================================

# For each response variable: print/capture the cutoff scan, save before and
# after histograms, and append a truncated copy of the column. The new column
# is named T<var>_cut<value> so the chosen cutoff is visible in the schema
# without having to read the script.
for (var in response_vars) {

  cat("\n---- ", var, " ----\n", sep = "")

  values <- df[[var]]
  cutoff <- cutoffs[[var]]

  write_cutoff_scan(values, var, scan_ranges[[var]])

  ggsave(out_path(paste0("hist_", var, "_before"), "png"),
         plot   = make_hist(values, var),
         width  = 14, height = 10, units = "cm", dpi = 300)

  truncated <- values
  truncated[values <= cutoff] <- NA  # left-side cutoff: drop tail to NA

  ggsave(out_path(paste0("hist_", var, "_after_cut", cutoff), "png"),
         plot   = make_hist(truncated, var,
                            caption_extra = paste0("  (cutoff > ", cutoff, ")")),
         width  = 14, height = 10, units = "cm", dpi = 300)

  # Bag-level view: the ANOVA in s5 operates on per-bag means, so the
  # skewness that actually matters for downstream inference is the skewness
  # of those means -- not of the raw seed-level distribution. Averaging
  # ~16 seeds per bag pulls the distribution toward Gaussian (CLT), so the
  # "before" panel is typically much closer to symmetric than its seed-level
  # counterpart, and the seed-level cutoff helps less dramatically here.
  bag_before <- bag_means(values,    df$exp_no, df$bag)
  bag_after  <- bag_means(truncated, df$exp_no, df$bag)

  ggsave(out_path(paste0("hist_", var, "_bag_before"), "png"),
         plot   = make_hist(bag_before,
                            paste0(var, " (bag mean)")),
         width  = 14, height = 10, units = "cm", dpi = 300)

  ggsave(out_path(paste0("hist_", var, "_bag_after_cut", cutoff), "png"),
         plot   = make_hist(bag_after,
                            paste0(var, " (bag mean)"),
                            caption_extra = paste0("  (cutoff > ", cutoff, ")")),
         width  = 14, height = 10, units = "cm", dpi = 300)

  new_col <- paste0("T", var, "_cut", cutoff)
  df[[new_col]] <- truncated

  cat(sprintf("  cutoff = %.2f  ->  %d/%d rows retained  (column: %s)\n",
              cutoff,
              sum(!is.na(truncated)), length(values),
              new_col))
  cat(sprintf("  bag-level skewness:  before = %5.2f   after = %5.2f   (n_bags = %d)\n",
              round(skewness(bag_before, na.rm = TRUE), 2),
              round(skewness(bag_after,  na.rm = TRUE), 2),
              sum(!is.na(bag_before))))
}


#===== EXPORT ===============================================================

out_xlsx <- out_path("skewness_corr", "xlsx")
write.xlsx(df, file = out_xlsx)
cat("\nWrote: ", out_xlsx, "\n", sep = "")
cat("Output folder: ", out_dir, "\n", sep = "")
