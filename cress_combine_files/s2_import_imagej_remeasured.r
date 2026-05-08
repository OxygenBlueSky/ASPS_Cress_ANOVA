# Import the remeasured ASPS 1-5 ImageJ output (260506_ASPS1-5_cress_measures.xlsx)
# and produce a long-format dataset with the same schema as the existing v1
# combined file (cress_length_ASPS_1-10_alldata_decoded.xlsx), tagged as the
# v2 remeasurement stream.
#
# Adapted from Paul's Import_ImageJ_data_withSetup.R. Two key differences:
# (a) the per-block header is an image filename, not a bag number, so the
#     Setup-file / Code-start-end machinery is replaced by a filename-based
#     left-join against filename_to_bag_lookup.xlsx (built in
#     s1_build_filename_lookup.r).
# (b) we DO NOT strip the ":xxxx-yyyy" coordinate suffix from Label until
#     after the join, because the lookup itself only uses the filename portion
#     and we want to keep coordinates available for later disambiguation of
#     ambiguous-photo cases.

library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)


#===== SECTION 1: Output folder and inputs ===================================

# Outputs go into <YYYYMMDD>_cress_remeasured/ alongside this script. The
# lookup is read from the most recent *_cress_lookup/ folder, resolved by
# folder-name sort (lexicographic == chronological for YYYYMMDD prefixes),
# so the script still works if you run it days after the lookup was built.

run_date   <- format(Sys.Date(), "%Y%m%d")
output_dir <- paste0(run_date, "_cress_remeasured")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

lookup_dirs <- sort(list.dirs(".", full.names = FALSE, recursive = FALSE),
                    decreasing = TRUE)
lookup_dirs <- lookup_dirs[grepl("_cress_lookup$", lookup_dirs)]

lookup_file <- NULL
for (d in lookup_dirs) {
  candidate <- file.path(d, "filename_to_bag_lookup.xlsx")
  if (file.exists(candidate)) {
    lookup_file <- candidate
    break
  }
}
if (is.null(lookup_file)) {
  stop("No filename_to_bag_lookup.xlsx found in any *_cress_lookup/ folder. ",
       "Run s1_build_filename_lookup.r first.")
}
cat("Using lookup:", lookup_file, "\n")
cat("Output folder:", output_dir, "\n\n")

# The remeasurement xlsx lives in ../input_data/ (sibling of this script's
# working folder). Sheet "A" is the only sheet. Row 1 holds image filenames
# in columns 1, 5, 9, ...; row 2 holds the repeated subheader
# Count/Label/Length/empty; data starts at row 3. We skip both header rows
# and read everything positionally to avoid readxl auto-renaming the dozens
# of duplicate "Label" / "Count" / "Length" columns.

input_file <- "../input_data/260506_ASPS1-5_cress_measures.xlsx"

df_raw <- read_excel(input_file, sheet = "A", skip = 2, col_names = FALSE)
df <- as.data.frame(df_raw)

cat("Read", nrow(df), "rows x", ncol(df), "cols of measurement data\n")


#===== SECTION 2: Validate column count ======================================

# Each image occupies 4 columns (Count, Label, Length, empty). Paul's identity
# is ncol == 4 * n_images - 1 (the trailing "empty" column for the last image
# is dropped by Excel). If not, something is missing.

n_images <- (ncol(df) + 1) / 4

if (n_images != round(n_images)) {
  stop("Column count ", ncol(df),
       " is not 4*N - 1; cannot split into image blocks.")
}

cat("Image blocks detected:", n_images, "\n\n")


#===== SECTION 3: Tag odd/even rows as LASPR / LAGES =========================

# Within each image block, ImageJ alternates: odd row = sprout length (LASPR),
# even row = total seedling length (LAGES). We write a 1/2 marker into the
# empty 4th column of each block so we can later filter rows by type.
#
# The loop walks block-by-block (Length column is index a within each block,
# starting at a = 3 for block 1, then 7, 11, ...). For each block we count
# non-NA Length values (`whole`), then write 1/2 alternately for that many
# rows in the marker column at index a+1.

a <- 3
for (i in seq_len(n_images)) {

  whole <- sum(!is.na(df[, a]))

  # Skew-protect: warn if any image has an odd row count, because that means
  # one seedling is missing its sprout or total measurement.
  if ((whole %% 2) == 1) {
    label_sample <- df[1, a - 1]
    message("WARNING: odd row count (", whole,
            ") in block ", i, " (", label_sample, ")")
  }

  b <- 1
  for (j in seq_len(floor(whole / 2))) {
    df[b, a + 1] <- 1
    df[b + 1, a + 1] <- 2
    b <- b + 2
  }

  a <- a + 4
}


#===== SECTION 4: Stack LASPR and LAGES into long form =======================

# Walk each block again. For block i with marker column at index d (= 4, 8,
# 12, ...), Label is at d-2 and Length at d-1. Subset rows tagged 1 -> LASPR,
# rows tagged 2 -> LAGES. Bind across all blocks, then cbind LASPR and LAGES
# row-wise (same seedling shares the same Label/coordinate within a block).

df_laspr <- data.frame(label = character(), laspr = numeric())
df_lages <- data.frame(label = character(), lages = numeric())

d <- 4
for (i in seq_len(n_images)) {

  block_marker <- df[, d]
  block_label  <- df[, d - 2]
  block_length <- df[, d - 1]

  laspr_rows <- !is.na(block_marker) & block_marker == 1
  lages_rows <- !is.na(block_marker) & block_marker == 2

  df_laspr <- rbind(df_laspr,
                    data.frame(label = block_label[laspr_rows],
                               laspr = as.numeric(block_length[laspr_rows])))
  df_lages <- rbind(df_lages,
                    data.frame(label = block_label[lages_rows],
                               lages = as.numeric(block_length[lages_rows])))

  d <- d + 4
}

# Pair: each row of df_laspr corresponds to the same seedling as the
# matching row in df_lages because they were tagged in alternating order
# within each block. Sanity-check the row counts before cbind.

if (nrow(df_laspr) != nrow(df_lages)) {
  stop("LASPR and LAGES row counts differ (",
       nrow(df_laspr), " vs ", nrow(df_lages),
       "). Inspect the odd-row warnings above.")
}

dftot <- data.frame(
  label = df_laspr$label,
  laspr = df_laspr$laspr,
  lages = df_lages$lages,
  stringsAsFactors = FALSE
)


#===== SECTION 5: Derived lengths and QA =====================================

# LAWU  = root length      = total seedling - sprout
# LAWUSPR = root/sprout ratio (rounded to 2 dp, matching Paul's convention).

dftot <- dftot %>%
  mutate(
    lawu    = lages - laspr,
    lawuspr = round(lawu / laspr, 2)
  )

# Duplicate-Label warning: an exact (filename:coord) repeat means "Z" was
# pressed twice in ImageJ. Flag and let the user decide.
n_occur <- as.data.frame(table(dftot$label))
dup_labels <- n_occur$Var1[n_occur$Freq > 1]
if (length(dup_labels) > 0) {
  message("Duplicate Labels detected (likely 'Z' pressed twice): ",
          length(dup_labels), " labels affected.")
  print(dftot[dftot$label %in% dup_labels, ])
}

# LASPR == LAGES or LASPR > LAGES indicate measurement errors.
eq_idx <- which(dftot$laspr == dftot$lages)
if (length(eq_idx) > 0) {
  message("LASPR == LAGES at ", length(eq_idx), " rows; first 5:")
  print(head(dftot[eq_idx, ], 5))
}

gt_idx <- which(dftot$laspr > dftot$lages)
if (length(gt_idx) > 0) {
  message("LASPR > LAGES at ", length(gt_idx), " rows; first 5:")
  print(head(dftot[gt_idx, ], 5))
}


#===== SECTION 6: Join filename -> bag/code/exp ==============================

# Replace Paul's Setup-file join with our hand-mapping lookup. The filename
# portion of label is the join key; the coordinate suffix is kept in a
# separate column so ambiguous-photo cases can be resolved later.

lookup <- read_excel(lookup_file)

dftot <- dftot %>%
  mutate(
    filename    = sub(":.*$", "", label),
    coord       = sub("^[^:]*:", "", label)
  ) %>%
  left_join(lookup, by = "filename")

n_unjoined <- sum(is.na(dftot$bag_no))
if (n_unjoined > 0) {
  message("Unjoined filenames (likely in the ambiguous lookup): ",
          n_unjoined, " rows. Inspect these before proceeding.")
}


#===== SECTION 7: Reshape to v1 schema =======================================

# Match the exact column order used by the legacy v1 combine script so the
# v2 stream can be bound row-wise to v1 in the next step. potency is left
# blank here and resolved in the combine_v2 step (which loads the decoding
# table); doing it here would duplicate that logic.
#
# - exp_no follows the v1 convention: "<asps_exp_num>_<CODE>"
# - label is rebuilt as "ASPS_<exp>_<CODE>_<bag>" to match v1 row labels

dftot <- dftot %>%
  mutate(
    exp_no_v1 = ifelse(is.na(asps_exp_num) | is.na(code),
                       NA_character_,
                       paste0(asps_exp_num, "_", code)),
    label_v1  = ifelse(is.na(asps_exp_num) | is.na(code) | is.na(bag_no),
                       NA_character_,
                       paste0("ASPS_", asps_exp_num, "_", code, "_", bag_no))
  )

remeasured <- dftot %>%
  transmute(
    reference_cell    = NA,
    count             = seq_len(n()),
    label             = label_v1,
    sprout_length     = laspr,
    seedling_length   = lages,
    root_length       = lawu,
    root_sprout_ratio = lawuspr,
    exp_no            = exp_no_v1,
    bag               = bag_no,
    potency           = NA_character_,
    source_filename   = filename,
    source_coord      = coord,
    needs_resolution  = is.na(bag_no)
  )

cat("\nFinal remeasured rows:", nrow(remeasured), "\n")
cat("Rows needing manual resolution (no bag mapping):",
    sum(remeasured$needs_resolution), "\n\n")


#===== SECTION 8: Write output ===============================================

out_file <- file.path(output_dir,
                      "cress_length_ASPS_1-5_remeasured.xlsx")
write.xlsx(remeasured, file = out_file, rowNames = FALSE)
cat("Wrote", out_file, "\n")
