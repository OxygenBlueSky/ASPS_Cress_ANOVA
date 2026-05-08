# Build filename -> (asps_exp_num, code, bag_no) lookup from the hand-labelled
# ASPS 1-5 files, so the new remeasurement (260506_ASPS1-5_cress_measures.xlsx)
# can be joined to bag/potency without re-entering metadata.
#
# Background: the existing labelled xlsx files in this folder carry a Label
# column of the form "ASPS<n>_<ge|un>_P<photoid>.jpg:<x>-<y>" together with
# hand-added `code` and `bag no` columns. The new remeasurement file uses the
# same image filenames, so the filename portion of Label is the join key.
#
# Two failure modes are surfaced as separate side-files:
#   - "copies"   : single rows where `code` is not A-F or `bag no` is text
#                  (the v1 doublet-removal pattern)
#   - "ambiguous": filenames mapping to >1 distinct (exp, code, bag) triplet
#                  across the labelled files, needing manual resolution

library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)


#===== SECTION 1: Output folder ==============================================

# All outputs from this run go into <YYYYMMDD>_cress_lookup/ alongside this
# script, so the folder name is self-descriptive and re-running on a different
# day doesn't overwrite earlier outputs. The downstream import script picks
# the most recent *_cress_lookup folder by name.

run_date   <- format(Sys.Date(), "%Y%m%d")
output_dir <- paste0(run_date, "_cress_lookup")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
cat("Output folder:", output_dir, "\n\n")


#===== SECTION 2: List labelled source files =================================

# Same list as in the legacy v1 combine script so the lookup stays consistent
# with the v1 pipeline (10 files: 5 experiments x gerade/ungerade, plus the
# extra 20251020-ASPS2gerade reprocessed by Paul).

# Project-wide raw inputs live in ../input_data/. Keep filenames bare here
# and prepend INPUT_DIR at read time, so the list stays readable.
INPUT_DIR <- "../input_data"

labelled_files <- file.path(INPUT_DIR, c(
  "20251003-ASPS1gerade_labeled.xlsx",
  "20251003-ASPS1ungerade_labeled.xlsx",
  "20251003-ASPS2ungerade_labeled.xlsx",
  "20251020-ASPS2gerade_labeled.xlsx",
  "20251003-ASPS3gerade_labeled.xlsx",
  "20251003-ASPS3ungerade_labeled.xlsx",
  "20251003-ASPS4gerade_labeled.xlsx",
  "20251003-ASPS4ungerade_labeled.xlsx",
  "20251003-ASPS5gerade_labeled.xlsx",
  "20251003-ASPS5ungerade_labeled.xlsx"
))


#===== SECTION 3: Read and tidy each labelled file ===========================

# We extract only the metadata columns needed for the join. The "skift" marker
# column (unnamed, position 2) is dropped. asps_exp_num is parsed from Label
# rather than the `exp no` column, matching the convention used in
# the legacy v1 combine script (the `exp no` column doesn't match the ASPS
# numbering scheme).

read_labelled_metadata <- function(file) {

  df <- read_excel(file, sheet = 1)

  # Drop the unnamed second column ("skift" marker), if present
  if (ncol(df) >= 2) {
    second_name <- names(df)[2]
    if (is.na(second_name) || second_name == "" || second_name == "...2") {
      df <- df[, -2]
    }
  }

  # Filename portion = everything before the ":" coordinate suffix.
  # bag_no is read as character first so we can preserve text annotations
  # like "COPY OF E2" for the copies log; numeric coercion happens in
  # section 4 after copies are split off.
  df <- df %>%
    mutate(
      filename = sub(":.*$", "", Label),
      asps_exp_num = as.numeric(str_extract(Label, "(?<=ASPS)\\d+")),
      code = toupper(as.character(code)),
      bag_no_raw = as.character(`bag no`),
      source_file = file
    ) %>%
    select(filename, asps_exp_num, code, bag_no_raw, source_file) %>%
    filter(!is.na(filename), filename != "")

  return(df)
}

cat("Reading labelled files...\n")
all_metadata <- bind_rows(lapply(labelled_files, read_labelled_metadata))
cat("Total rows read:", nrow(all_metadata), "\n\n")


#===== SECTION 4: Split off copy/doublet annotations =========================

# Convention used in the labelled files (and relied on by the v1 combine
# script): a photo that's a copy of another gets a non-A-F entry in `code`
# (e.g. blank) or a text annotation in `bag no` like "COPY OF E2", so the
# downstream potency lookup returns NA and the row is dropped at the
# doublet-removal filter. We mirror that here: rows whose code is not a
# single A-F letter, or whose bag_no cannot be coerced to a number, are
# split into a side log so you can inspect what was marked as a copy.

is_valid_code <- function(x) !is.na(x) & grepl("^[A-F]$", x)

bag_no_numeric <- suppressWarnings(as.numeric(all_metadata$bag_no_raw))
is_valid_bag   <- !is.na(bag_no_numeric)

is_copy <- !(is_valid_code(all_metadata$code) & is_valid_bag)

copy_rows <- all_metadata[is_copy, ] %>%
  arrange(filename, source_file)

valid_rows <- all_metadata[!is_copy, ] %>%
  mutate(bag_no = bag_no_numeric[!is_copy]) %>%
  select(filename, asps_exp_num, code, bag_no, source_file)

cat("Copy/doublet-marked rows split off:", nrow(copy_rows), "\n")
cat("Valid rows kept for lookup        :", nrow(valid_rows), "\n\n")


#===== SECTION 5: Collapse to per-filename records ===========================

# Each image should be hand-labelled with one (asps_exp_num, code, bag_no)
# triplet, but some filenames appear under multiple bags because the same
# photo got measured into two different bag groups. Keep distinct triplets
# per filename so we can spot those cases.

per_filename <- valid_rows %>%
  distinct(filename, asps_exp_num, code, bag_no) %>%
  arrange(filename, asps_exp_num, code, bag_no)


#===== SECTION 6: Split clean vs. ambiguous filenames ========================

# A filename is "clean" if it maps to exactly one (asps_exp_num, code, bag_no)
# triplet across all labelled files. Otherwise it is "ambiguous" and needs
# manual resolution before the new remeasurement file can be joined.

mapping_counts <- per_filename %>%
  count(filename, name = "n_distinct_mappings")

clean_lookup <- per_filename %>%
  inner_join(filter(mapping_counts, n_distinct_mappings == 1),
             by = "filename") %>%
  select(filename, asps_exp_num, code, bag_no)

ambiguous_lookup <- per_filename %>%
  inner_join(filter(mapping_counts, n_distinct_mappings > 1),
             by = "filename") %>%
  arrange(filename, asps_exp_num, code, bag_no)

cat("Unique filenames               :", nrow(mapping_counts), "\n")
cat("Filenames with single mapping  :", nrow(clean_lookup), "\n")
cat("Filenames with multiple mapping:",
    nrow(distinct(ambiguous_lookup, filename)), "\n")
cat("Total ambiguous rows           :", nrow(ambiguous_lookup), "\n\n")


#===== SECTION 7: Write outputs ==============================================

# Three files in output_<date>/:
#   filename_to_bag_lookup.xlsx           clean 1:1 mappings, used by import
#   filename_to_bag_lookup_ambiguous.xlsx filenames with >1 distinct mapping
#   filename_to_bag_lookup_copies.xlsx    rows marked as copies (non-A-F code
#                                         or non-numeric bag_no)

write.xlsx(clean_lookup,
           file = file.path(output_dir, "filename_to_bag_lookup.xlsx"),
           rowNames = FALSE)

write.xlsx(ambiguous_lookup,
           file = file.path(output_dir, "filename_to_bag_lookup_ambiguous.xlsx"),
           rowNames = FALSE)

write.xlsx(copy_rows,
           file = file.path(output_dir, "filename_to_bag_lookup_copies.xlsx"),
           rowNames = FALSE)

cat("Wrote", file.path(output_dir, "filename_to_bag_lookup.xlsx"),
    "(", nrow(clean_lookup), "rows )\n")
cat("Wrote", file.path(output_dir, "filename_to_bag_lookup_ambiguous.xlsx"),
    "(", nrow(ambiguous_lookup), "rows )\n")
cat("Wrote", file.path(output_dir, "filename_to_bag_lookup_copies.xlsx"),
    "(", nrow(copy_rows), "rows )\n")
