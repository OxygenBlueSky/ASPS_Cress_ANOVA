#
# 20260507_repeat_measurement_internal_variance.R
# Updated 20260508: third measurement round added; force text reading
# to avoid readxl mis-typing column A (mixed strings + integers).
#
# Bag-level reproducibility check on 4 bags. With 3 rounds we can compute
# proper variance components (between-bag vs. within-bag) instead of just
# the SD of paired differences.
#
# Data: Repeat_measurement_for_internal_variance.xlsx, sheet "Sheet1"
# Layout: stacked blocks (one per measurement round), each with 4 image
# columns side-by-side (Count, Label, Length triplets, separated by spacers).
# Within each image, seedlings alternate two rows: odd count = sprout,
# even count = total seedling length.
#

library(readxl)
library(dplyr)
library(tidyr)
library(here)
library(lme4)

#===== Load and reshape ============================================

# Read everything as text. Column A mixes integer counts with occasional
# header strings ("Count") and image names; readxl's type inference can
# miss the headers depending on which rows it samples. Reading as text
# and coercing numerics later is robust to any future layout change.

raw <- read_excel(
  here("Repeat measurement for internal variance.xlsx"),
  sheet = "Sheet1",
  col_names = FALSE,
  col_types = "text",
  .name_repair = "minimal"
)

# Locate header rows (one per measurement round). Each header has "Count"
# in col 1 and "Label" in col 2 — using col 2 as the anchor since the
# string content there is unambiguous.

header_rows <- which(raw[[2]] == "Label")
cat("Found", length(header_rows), "measurement round(s) at rows:",
    paste(header_rows, collapse = ", "), "\n")

# Define the 4 image-column blocks: columns 1-3, 5-7, 9-11, 13-15.
# Cols 4, 8, 12 are spacers.

block_starts <- c(1, 5, 9, 13)

#
# Pull a single (image × round) block into long format.
# Returns one row per measured value, tagged sprout vs. seedling
# based on its position within the seedling pair.
#
parse_block <- function(data, header_row, col_start, round_id) {
  count_col  <- col_start
  label_col  <- col_start + 1
  length_col <- col_start + 2
  
  # Data runs from the row after the header to either the next header
  # or the end of the sheet
  next_header <- header_rows[header_rows > header_row][1]
  data_end    <- if (is.na(next_header)) nrow(data) else next_header - 2
  data_rows   <- (header_row + 1):data_end
  
  block <- tibble(
    count  = as.numeric(data[[count_col]][data_rows]),
    label  = as.character(data[[label_col]][data_rows]),
    length = as.numeric(data[[length_col]][data_rows])
  ) |>
    filter(!is.na(length))
  
  # Image name = part of label before the colon. ImageJ outputs sprout
  # then total seedling per seedling, so odd count = sprout.
  block |>
    mutate(
      image = sub(":.*$", "", label),
      part  = if_else(count %% 2 == 1, "sprout", "seedling"),
      round = round_id
    ) |>
    select(image, part, length, round)
}

# Walk all rounds × all 4 image blocks
long_data <- bind_rows(
  lapply(seq_along(header_rows), function(i) {
    bind_rows(lapply(block_starts, function(cs) {
      parse_block(raw, header_rows[i], cs, round_id = i)
    }))
  })
)

#===== Bag-level summaries =========================================

# Bag mean per round, sprout and seedling separately. "Bag" = image
# (one image per bag here). All seedlings within a round contribute,
# even when round-to-round counts differ — the bag mean is what enters
# downstream analysis, so its round-to-round wobble is what we want.

bag_means <- long_data |>
  group_by(image, part, round) |>
  summarise(
    n_seedlings = n(),
    bag_mean    = mean(length),
    .groups     = "drop"
  )

print(bag_means)

# Wide for inspection: one row per bag × part, columns = rounds
bag_wide <- bag_means |>
  select(image, part, round, bag_mean) |>
  pivot_wider(names_from = round, values_from = bag_mean,
              names_prefix = "round_")

print(bag_wide)

#===== Repeat SD at the bag level (3 rounds) =======================

#
# Two equivalent estimators of measurement-occasion SD:
#
# (a) Pooled within-bag SD: for each bag, SD of its 3 round-means; pool
#     across bags as sqrt(mean(bag_sd^2)). df = n_bags * (n_rounds - 1) = 8.
#
# (b) One-way random-effects ANOVA: bag_mean ~ (1 | image). Same within-
#     bag variance plus a between-bag estimate, and ICC = var_between /
#     (var_between + var_within). Useful as a reliability number.
#

# (a) Pooled within-bag SD
repeat_sd <- bag_means |>
  group_by(image, part) |>
  summarise(
    bag_grand_mean       = mean(bag_mean),
    bag_sd_across_rounds = sd(bag_mean),
    .groups              = "drop"
  ) |>
  group_by(part) |>
  summarise(
    n_bags             = n(),
    typical_bag_mean   = mean(bag_grand_mean),
    sd_repeat_pooled   = sqrt(mean(bag_sd_across_rounds^2)),
    pct_of_bag_mean    = sd_repeat_pooled / typical_bag_mean * 100,
    .groups            = "drop"
  )

print(repeat_sd)

# (b) Random-effects model — gives ICC. Run separately per part rather
# than nesting list-columns, which is fragile with small samples.

run_icc <- function(part_name) {
  d <- bag_means |> filter(part == part_name)
  fit <- lmer(bag_mean ~ 1 + (1 | image), data = d, REML = TRUE)
  vc  <- as.data.frame(VarCorr(fit))
  var_between <- vc$vcov[vc$grp == "image"]
  var_within  <- vc$vcov[vc$grp == "Residual"]
  tibble(
    part        = part_name,
    var_between = var_between,
    var_within  = var_within,
    sd_repeat   = sqrt(var_within),         # should match sd_repeat_pooled
    icc         = var_between / (var_between + var_within)
  )
}

icc_table <- bind_rows(lapply(unique(bag_means$part), run_icc))
print(icc_table)