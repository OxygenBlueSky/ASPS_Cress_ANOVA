# Debug script to identify aliased coefficients issue
# Run this BEFORE the main analysis script

library(readxl)
library(here)
library(dplyr)
library(tidyr)

# Read data
filename <- "251021_cress_length_ASPS_1-10_alldata_decoded_no_dublets.xlsx"
df_raw <- read_excel(here(filename), sheet = "Sheet 1")

# Parse
df_parsed <- df_raw %>%
  separate(exp_no, into = c("experiment_number", "potency_code"), 
           sep = "_", remove = FALSE) %>%
  mutate(
    experiment_number = as.integer(experiment_number),
    bag = as.integer(bag),
    experimenter = ifelse(experiment_number <= 5, "AS", "JZ")
  )

# Calculate bag-level means
response_vars <- c("sprout_length", "root_length", "seedling_length", "root_sprout_ratio")

df_bags <- df_parsed %>%
  group_by(experiment_number, experimenter, potency_code, potency, bag, exp_no, label) %>%
  summarise(
    n_seeds = n(),
    sprout_length = mean(sprout_length, na.rm = TRUE),
    root_length = mean(root_length, na.rm = TRUE),
    seedling_length = mean(seedling_length, na.rm = TRUE),
    root_sprout_ratio = mean(root_sprout_ratio, na.rm = TRUE),
    .groups = "drop"
  )

# Convert to factors
df_bags$experiment_number <- as.factor(df_bags$experiment_number)
df_bags$potency <- as.factor(df_bags$potency)

cat("\n")
cat(strrep("=", 80), "\n")
cat("CHECKING FOR MISSING COMBINATIONS\n")
cat(strrep("=", 80), "\n\n")

# Check ALL data
cat("--- ALL DATA (ASPS 1-10) ---\n\n")
df_all <- df_bags

# Create contingency table
contingency_all <- df_all %>%
  group_by(experiment_number, potency) %>%
  summarise(n_bags = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = potency, values_from = n_bags, values_fill = 0)

cat("Contingency table (experiments × potencies):\n")
print(as.data.frame(contingency_all))
cat("\n")

# Check for empty cells
empty_cells_all <- df_all %>%
  group_by(experiment_number, potency) %>%
  summarise(n_bags = n(), .groups = "drop") %>%
  tidyr::complete(experiment_number, potency, fill = list(n_bags = 0)) %>%
  filter(n_bags == 0)

if (nrow(empty_cells_all) > 0) {
  cat("WARNING: Empty combinations detected in ALL data:\n")
  print(as.data.frame(empty_cells_all))
  cat("\n")
} else {
  cat("No empty combinations in ALL data\n\n")
}

# Check AS data
cat("\n--- AS DATA (ASPS 1-5) ---\n\n")
df_as <- df_bags %>% filter(experimenter == "AS")

contingency_as <- df_as %>%
  group_by(experiment_number, potency) %>%
  summarise(n_bags = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = potency, values_from = n_bags, values_fill = 0)

cat("Contingency table (experiments × potencies):\n")
print(as.data.frame(contingency_as))
cat("\n")

empty_cells_as <- df_as %>%
  group_by(experiment_number, potency) %>%
  summarise(n_bags = n(), .groups = "drop") %>%
  tidyr::complete(experiment_number, potency, fill = list(n_bags = 0)) %>%
  filter(n_bags == 0)

if (nrow(empty_cells_as) > 0) {
  cat("WARNING: Empty combinations detected in AS data:\n")
  print(as.data.frame(empty_cells_as))
  cat("\n")
} else {
  cat("No empty combinations in AS data\n\n")
}

# Check JZ data
cat("\n--- JZ DATA (ASPS 6-10) ---\n\n")
df_jz <- df_bags %>% filter(experimenter == "JZ")

contingency_jz <- df_jz %>%
  group_by(experiment_number, potency) %>%
  summarise(n_bags = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = potency, values_from = n_bags, values_fill = 0)

cat("Contingency table (experiments × potencies):\n")
print(as.data.frame(contingency_jz))
cat("\n")

empty_cells_jz <- df_jz %>%
  group_by(experiment_number, potency) %>%
  summarise(n_bags = n(), .groups = "drop") %>%
  tidyr::complete(experiment_number, potency, fill = list(n_bags = 0)) %>%
  filter(n_bags == 0)

if (nrow(empty_cells_jz) > 0) {
  cat("WARNING: Empty combinations detected in JZ data:\n")
  print(as.data.frame(empty_cells_jz))
  cat("\n")
} else {
  cat("No empty combinations in JZ data\n\n")
}


cat("\n")
cat(strrep("=", 80), "\n")
cat("TESTING ANOVA MODELS\n")
cat(strrep("=", 80), "\n\n")

# Test function
test_anova <- function(data_subset, group_name) {
  cat(sprintf("\nTesting %s:\n", group_name))
  cat(sprintf("  N bags: %d\n", nrow(data_subset)))
  cat(sprintf("  Experiments: %s\n", paste(levels(data_subset$experiment_number), collapse = ", ")))
  cat(sprintf("  Potencies: %s\n", paste(levels(data_subset$potency), collapse = ", ")))
  
  # Try to fit model
  tryCatch({
    model <- aov(sprout_length ~ potency * experiment_number, data = data_subset)
    
    # Check for aliased coefficients
    aliased <- alias(model)
    if (!is.null(aliased$Complete)) {
      cat("  ERROR: Model has aliased coefficients!\n")
      cat("  Aliased terms:\n")
      print(aliased$Complete)
    } else {
      cat("  SUCCESS: No aliased coefficients\n")
    }
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
  })
}

# Test all three groups
test_anova(df_bags, "ALL DATA")
test_anova(df_bags %>% filter(experimenter == "AS"), "AS ONLY")
test_anova(df_bags %>% filter(experimenter == "JZ"), "JZ ONLY")


cat("\n")
cat(strrep("=", 80), "\n")
cat("RECOMMENDATIONS\n")
cat(strrep("=", 80), "\n\n")

cat("If you see empty cells or aliased coefficients above:\n")
cat("1. Remove experiments or potencies with missing data\n")
cat("2. Use a model without interaction term for affected groups\n")
cat("3. Analyze only complete factorial subsets\n\n")