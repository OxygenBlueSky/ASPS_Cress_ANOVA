# Build a parallel-streams dataset that contains both the v1 (original)
# ASPS 1-10 measurements and the v2 (remeasured ASPS 1-5) measurements,
# tagged by a `version` column for provenance. Two boolean membership
# columns mark which analysis each row belongs to:
#
#   in_v1_analysis = (version == "v1_original")
#                    -- the original ASPS 1-10 dataset, untouched.
#   in_v2_analysis = v2 ASPS 1-5 + v1 ASPS 6-10
#                    -- the "best available" view: remeasured where we
#                    have it (1-5), original where we don't (6-10).
#                    ASPS 6-10 was never remeasured, so its v1 rows are
#                    the canonical v2 data for those experiments.
#
# Downstream s4/s5 read this single combined file and filter rows by the
# appropriate membership column. Without filtering, both streams sit
# side-by-side -- the "v1v2" comparison view.
#
# Inputs:
#   cress_length_ASPS_1-10_alldata_decoded.xlsx              (v1, frozen
#                                                             output of the
#                                                             legacy v1 combine
#                                                             script in OLD/)
#   <date>_cress_remeasured/cress_length_ASPS_1-5_remeasured.xlsx
#                                                            (v2, produced by
#                                                             s2_import_imagej_remeasured.r)
# Outputs (under <run_date>_cress_combined/):
#   cress_length_ASPS_1-10_alldata_decoded_v1v2.xlsx
#   cress_length_ASPS_1-5_repeatability_v1_vs_v2.xlsx
#
# Manual v1 data-quality drops
# ----------------------------
# Because ASPS 6-10 has no live derivation step (it enters the analysis only
# via the frozen v1 xlsx -- see REMEASUREMENT_PIPELINE.md), bag-level fixes
# for those experiments must live here. They are listed in the
# `v1_bags_to_drop` tibble in section 2 and removed from the v1 stream
# before bind. To add a new drop, append one row -- no other code change.
#
# Currently dropped (both rows are JZ data-entry errors in the source
# only_combined_data_Kresselaenge_ASPS_6-10_SL.xlsx -- duplicate measurement
# blocks pasted under wrong filenames; the photos themselves are distinct):
#   exp_no = "6_A", bag = "10"  -- duplicate data under wrong filename (JZ
#                                  error); identical to 6_A bag 9.
#   exp_no = "6_C", bag = "11"  -- duplicate data under wrong filename (JZ
#                                  error); identical to 6_C bag 10.

library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)


#===== SECTION 0: Output folder and v2 input resolution ======================

# Outputs go into <YYYYMMDD>_cress_combined/ alongside this script. The v2
# input is read from the most recent *_cress_remeasured/ folder, resolved by
# folder-name sort.

run_date   <- format(Sys.Date(), "%Y%m%d")
output_dir <- paste0(run_date, "_cress_combined")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

remeasured_dirs <- sort(list.dirs(".", full.names = FALSE, recursive = FALSE),
                        decreasing = TRUE)
remeasured_dirs <- remeasured_dirs[grepl("_cress_remeasured$", remeasured_dirs)]

v2_input <- NULL
for (d in remeasured_dirs) {
  candidate <- file.path(d, "cress_length_ASPS_1-5_remeasured.xlsx")
  if (file.exists(candidate)) {
    v2_input <- candidate
    break
  }
}
if (is.null(v2_input)) {
  stop("No cress_length_ASPS_1-5_remeasured.xlsx found in any ",
       "*_cress_remeasured/ folder. Run s2_import_imagej_remeasured.r first.")
}
cat("Using v2 input:", v2_input, "\n")
cat("Output folder :", output_dir, "\n\n")


#===== SECTION 1: Decoding helper (shared with the v1 combine script) ========

# Same decoding logic as in the legacy v1 combine script. We re-use it here
# because the v2 import step left potency blank: the lookup table only carries
# (asps_exp_num, code, bag_no), not the decoded remedy name.

decode_potency <- function() {
  decoding <- read.csv("../input_data/ASPS1-10-decoding table.csv",
                       skip = 3, header = TRUE,
                       stringsAsFactors = FALSE)
  names(decoding) <- c("Experiment_number", "Lactose", "Stannum",
                       "Silicea", "Sulphur", "Ars. album", "Mercury")
  decoding <- decoding[!is.na(decoding$Experiment_number) &
                       decoding$Experiment_number != "", ]
  decoding$Experiment_number <- as.numeric(decoding$Experiment_number)
  decoding
}

get_potency <- function(exp_num, code_letter, decoding_table) {
  if (is.na(exp_num) || is.na(code_letter) || code_letter == "") return(NA)
  exp_row <- decoding_table[decoding_table$Experiment_number == exp_num, ]
  if (nrow(exp_row) == 0) return(NA)
  if (nrow(exp_row) > 1) exp_row <- exp_row[1, ]
  code_letter <- toupper(as.character(code_letter))
  remedy_cols <- c("Lactose", "Stannum", "Silicea", "Sulphur",
                   "Ars. album", "Mercury")
  for (remedy in remedy_cols) {
    remedy_code <- as.character(exp_row[[remedy]][1])
    if (!is.na(remedy_code) && toupper(remedy_code) == code_letter) {
      return(remedy)
    }
  }
  NA
}


#===== SECTION 2: Load v1 and v2 streams =====================================

# v1 already has `potency` filled. v2 needs decoding here. We also strip
# the auxiliary columns from v2 (source_filename, source_coord,
# needs_resolution) so the schemas match before bind_rows.

v1 <- read_excel("../input_data/cress_length_ASPS_1-10_alldata_decoded.xlsx") %>%
  mutate(version = "v1_original")

# Manual data-quality drops applied to the v1 stream. Documented at the
# top of this script. Each row below is one (exp_no, bag) to remove; the
# reason column is documentation only and is not used in the join. To add
# a drop, append a row.
v1_bags_to_drop <- tibble::tribble(
  ~exp_no, ~bag, ~reason,
  "6_A",   "10", "Duplicate data under wrong filename, JZ error (identical to 6_A bag 9)",
  "6_C",   "11", "Duplicate data under wrong filename, JZ error (identical to 6_C bag 10)"
)

# Apply drops with anti_join and report each removal explicitly to the
# console so the audit trail is in the run log, not just in this script.
n_v1_before <- nrow(v1)
v1 <- v1 %>%
  anti_join(v1_bags_to_drop %>% select(exp_no, bag),
            by = c("exp_no", "bag"))
cat("\nManual v1 drops (", nrow(v1_bags_to_drop), " bag(s), ",
    n_v1_before - nrow(v1), " rows removed):\n", sep = "")
for (i in seq_len(nrow(v1_bags_to_drop))) {
  cat("  ", v1_bags_to_drop$exp_no[i], " bag ", v1_bags_to_drop$bag[i],
      "  --  ", v1_bags_to_drop$reason[i], "\n", sep = "")
}
cat("\n")

v2_raw <- read_excel(v2_input)

decoding_table <- decode_potency()

# Re-derive asps_exp_num and code from exp_no ("<num>_<CODE>") so we can
# call get_potency without re-reading the labelled files.
v2 <- v2_raw %>%
  mutate(
    asps_exp_num = suppressWarnings(as.numeric(sub("_.*$", "", exp_no))),
    code_letter  = sub("^[^_]*_", "", exp_no),
    potency = mapply(get_potency, asps_exp_num, code_letter,
                     MoreArgs = list(decoding_table = decoding_table)),
    version = "v2_remeasured"
  ) %>%
  select(reference_cell, count, label, sprout_length, seedling_length,
         root_length, root_sprout_ratio, exp_no, bag, potency, version)


#===== SECTION 3: Drop unresolved v2 rows from the canonical bind ============

# Rows where the filename couldn't be mapped to a single bag (ambiguous
# filenames pending manual resolution) have NA in exp_no/bag/potency.
# Keeping them in the analysis dataset would corrupt grouping. They live
# in the v2_raw file for hand-resolution; here we exclude them from the
# combined output and report the count.

n_dropped <- sum(is.na(v2$potency))
if (n_dropped > 0) {
  cat("Dropping", n_dropped,
      "v2 rows with unresolved filename->bag mapping.\n")
  cat("These remain in", v2_input,
      "(rows where needs_resolution = TRUE).\n\n")
}

v2 <- v2 %>% filter(!is.na(potency))

# Align column types with v1: the v1 decoded file stores `bag` as character
# (carried over from the labelled files), so cast v2's numeric bag to match.
v1 <- v1 %>% mutate(bag = as.character(bag))
v2 <- v2 %>% mutate(bag = as.character(bag))


#===== SECTION 4: Bind and write =============================================

# bind_rows tolerates the v1 frame not yet having a `version` column on disk
# (it was added in section 2) and aligns columns by name. Sort matches the
# v1 combine script's final ordering for diff-friendliness.

combined <- bind_rows(v1, v2) %>%
  arrange(version, exp_no, bag, count)

# Analysis-membership flags. exp_no is "<asps_exp_num>_<code>", e.g. "7_C",
# so we re-derive the experiment number with sub() rather than depending on
# any column already present in v1 (the v1 file doesn't carry asps_exp_num).
# in_v2_analysis is the user-facing "best available" view: remeasured rows
# for ASPS 1-5, original rows for ASPS 6-10 (which were never remeasured).
asps_exp_num_from_exp_no <- function(x) {
  suppressWarnings(as.numeric(sub("_.*$", "", x)))
}

combined <- combined %>%
  mutate(
    in_v1_analysis = (version == "v1_original"),
    in_v2_analysis = (version == "v2_remeasured") |
                     (version == "v1_original" &
                      asps_exp_num_from_exp_no(exp_no) >= 6)
  )

cat("v1 rows:", nrow(v1), "\n")
cat("v2 rows (after resolution filter):", nrow(v2), "\n")
cat("Combined rows:", nrow(combined), "\n")
cat("  in_v1_analysis = TRUE:", sum(combined$in_v1_analysis), "\n")
cat("  in_v2_analysis = TRUE:", sum(combined$in_v2_analysis),
    "  (v2 ASPS 1-5 + v1 ASPS 6-10)\n")

out_file <- file.path(output_dir,
                      "cress_length_ASPS_1-10_alldata_decoded_v1v2.xlsx")
write.xlsx(combined, file = out_file, rowNames = FALSE)
cat("Wrote", out_file, "\n")


#===== SECTION 5: Quick repeatability summary (v1 vs v2 on ASPS 1-5) =========

# Bag-level means side-by-side. Useful first sanity check before formal
# Bland-Altman / scatter plotting in a separate analysis script.

repeatability <- combined %>%
  filter(exp_no %in% unique(v2$exp_no)) %>%
  group_by(version, exp_no, bag, potency) %>%
  summarise(
    n              = n(),
    mean_seedling  = mean(seedling_length, na.rm = TRUE),
    mean_sprout    = mean(sprout_length, na.rm = TRUE),
    mean_root      = mean(root_length, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = version,
    values_from = c(n, mean_seedling, mean_sprout, mean_root)
  )

repeat_file <- file.path(output_dir,
                         "cress_length_ASPS_1-5_repeatability_v1_vs_v2.xlsx")
write.xlsx(repeatability, file = repeat_file, rowNames = FALSE)
cat("Wrote", repeat_file, "\n")
