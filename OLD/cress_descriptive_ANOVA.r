# ASPS Cress Length Analysis - JZ Experiments 6-10
# Analysis of sprout_length, root_length, and seedling_length
# Date: 2025-10-15
# 
# Experimental unit: BAG (mean of ~16 seeds per bag)
# Design: potency (6 levels) x experiment (5 levels)
# ANOVA: Type III SS with interaction term


# Libraries ---------------------------------------------------------------

library(readxl)
library(car)
library(here)
library(dplyr)
library(tidyr)


# Determine export date
date <- Sys.Date() 
date2 <- gsub("-| |UTC", "", date)


# Read data ---------------------------------------------------------------

cat("\n")
cat(strrep("=", 80), "\n")
cat("READING DATA\n")
cat(strrep("=", 80), "\n")

# Read from Excel file
filename <- "only_combined_data_Kresselaenge_ASPS_6-10_SL.xlsx"
df_raw <- read_excel(here(filename), sheet = "ASPS 6-10")

cat("Raw data loaded:\n")
cat("  Total observations (individual seeds):", nrow(df_raw), "\n")
cat("  Variables:", paste(colnames(df_raw), collapse = ", "), "\n")


# Parse experiment number and potency code --------------------------------

cat("\n")
cat(strrep("=", 80), "\n")
cat("PARSING EXPERIMENT AND POTENCY\n")
cat(strrep("=", 80), "\n")

# Split exp_no into experiment_number and potency_code
df_parsed <- df_raw %>%
  separate(exp_no, into = c("experiment_number", "potency_code"), 
           sep = "_", remove = FALSE) %>%
  mutate(
    experiment_number = as.integer(experiment_number),
    bag = as.integer(bag)
  )

cat("Parsed structure:\n")
cat("  Experiments:", paste(sort(unique(df_parsed$experiment_number)), collapse = ", "), "\n")
cat("  Potency codes:", paste(sort(unique(df_parsed$potency_code)), collapse = ", "), "\n")
cat("  Decoded potencies:", paste(sort(unique(df_parsed$potency)), collapse = ", "), "\n")


# Note data issues --------------------------------------------------------

cat("\n")
cat(strrep("=", 80), "\n")
cat("DATA QUALITY NOTES\n")
cat(strrep("=", 80), "\n")

# Check for missing bags
missing_bags <- df_parsed %>%
  group_by(experiment_number, potency_code) %>%
  summarise(
    n_bags = n_distinct(bag),
    bags_present = paste(sort(unique(bag)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_bags < 15)

if (nrow(missing_bags) > 0) {
  cat("MISSING BAGS DETECTED:\n")
  print(as.data.frame(missing_bags))
  cat("\nNote: ASPS_6_B missing bags 1-3 (documented)\n")
  cat("Note: ASPS_6_D possible data transfer error in bag 1 (excluded)\n")
} else {
  cat("All experiment-potency combinations have complete bag data\n")
}

cat("\nProceeding with available data (unbalanced design)\n")


# Calculate bag-level means -----------------------------------------------

cat("\n")
cat(strrep("=", 80), "\n")
cat("CALCULATING BAG-LEVEL MEANS\n")
cat(strrep("=", 80), "\n")
cat("Experimental unit: BAG (averaging individual seed measurements)\n")
cat(strrep("=", 80), "\n")

# Define response variables
response_vars <- c("sprout_length", "root_length", "seedling_length")

# Calculate means per bag
df_bags <- df_parsed %>%
  group_by(experiment_number, potency_code, potency, bag, exp_no, label) %>%
  summarise(
    n_seeds = n(),
    sprout_length = mean(sprout_length, na.rm = TRUE),
    root_length = mean(root_length, na.rm = TRUE),
    seedling_length = mean(seedling_length, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nBag-level dataset created:\n")
cat("  Total bags:", nrow(df_bags), "\n")
cat("  Seeds per bag: min =", min(df_bags$n_seeds), 
    ", max =", max(df_bags$n_seeds),
    ", mean =", round(mean(df_bags$n_seeds), 1), "\n")

# Summary by experiment-potency
bags_summary <- df_bags %>%
  group_by(experiment_number, potency_code, potency) %>%
  summarise(
    n_bags = n(),
    .groups = "drop"
  ) %>%
  arrange(experiment_number, potency_code)

cat("\nBags per experiment-potency combination:\n")
print(as.data.frame(bags_summary), row.names = FALSE)


# Convert to factors for ANOVA -------------------------------------------

df_bags$experiment_number <- as.factor(df_bags$experiment_number)
df_bags$potency <- as.factor(df_bags$potency)
df_bags$potency_code <- as.factor(df_bags$potency_code)


# Comprehensive descriptive statistics -----------------------------------

cat("\n")
cat(strrep("#", 80), "\n")
cat("DESCRIPTIVE STATISTICS\n")
cat(strrep("#", 80), "\n")

for (var in response_vars) {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat(toupper(var), "\n")
  cat(strrep("=", 80), "\n")
  
  # Overall statistics
  overall <- df_bags %>%
    summarise(
      Mean = mean(!!sym(var), na.rm = TRUE),
      SD = sd(!!sym(var), na.rm = TRUE),
      Min = min(!!sym(var), na.rm = TRUE),
      Max = max(!!sym(var), na.rm = TRUE),
      N_bags = n()
    )
  
  cat("\nOVERALL (all experiments and potencies):\n")
  cat(sprintf("  Mean ± SD: %.3f ± %.3f\n", overall$Mean, overall$SD))
  cat(sprintf("  Range: %.3f - %.3f\n", overall$Min, overall$Max))
  cat(sprintf("  N bags: %d\n", overall$N_bags))
  
  # By potency (pooled across experiments)
  by_potency <- df_bags %>%
    group_by(potency) %>%
    summarise(
      Mean = mean(!!sym(var), na.rm = TRUE),
      SD = sd(!!sym(var), na.rm = TRUE),
      Min = min(!!sym(var), na.rm = TRUE),
      Max = max(!!sym(var), na.rm = TRUE),
      N_bags = n(),
      .groups = "drop"
    ) %>%
    arrange(potency)
  
  cat("\n--- BY POTENCY (pooled across experiments) ---\n")
  cat(sprintf("%-15s %10s %10s %10s %10s %8s\n", 
              "Potency", "Mean", "SD", "Min", "Max", "N_bags"))
  cat(strrep("-", 80), "\n")
  for (i in 1:nrow(by_potency)) {
    cat(sprintf("%-15s %10.3f %10.3f %10.3f %10.3f %8d\n",
                by_potency$potency[i],
                by_potency$Mean[i],
                by_potency$SD[i],
                by_potency$Min[i],
                by_potency$Max[i],
                by_potency$N_bags[i]))
  }
  
  # By experiment (pooled across potencies)
  by_experiment <- df_bags %>%
    group_by(experiment_number) %>%
    summarise(
      Mean = mean(!!sym(var), na.rm = TRUE),
      SD = sd(!!sym(var), na.rm = TRUE),
      Min = min(!!sym(var), na.rm = TRUE),
      Max = max(!!sym(var), na.rm = TRUE),
      N_bags = n(),
      .groups = "drop"
    ) %>%
    arrange(experiment_number)
  
  cat("\n--- BY EXPERIMENT (pooled across potencies) ---\n")
  cat(sprintf("%-15s %10s %10s %10s %10s %8s\n", 
              "Experiment", "Mean", "SD", "Min", "Max", "N_bags"))
  cat(strrep("-", 80), "\n")
  for (i in 1:nrow(by_experiment)) {
    cat(sprintf("%-15s %10.3f %10.3f %10.3f %10.3f %8d\n",
                paste0("ASPS_", by_experiment$experiment_number[i]),
                by_experiment$Mean[i],
                by_experiment$SD[i],
                by_experiment$Min[i],
                by_experiment$Max[i],
                by_experiment$N_bags[i]))
  }
  
  # By experiment x potency (detailed)
  by_exp_pot <- df_bags %>%
    group_by(experiment_number, potency) %>%
    summarise(
      Mean = mean(!!sym(var), na.rm = TRUE),
      SD = sd(!!sym(var), na.rm = TRUE),
      Min = min(!!sym(var), na.rm = TRUE),
      Max = max(!!sym(var), na.rm = TRUE),
      N_bags = n(),
      .groups = "drop"
    ) %>%
    arrange(experiment_number, potency)
  
  cat("\n--- BY EXPERIMENT x POTENCY (all combinations) ---\n")
  cat(sprintf("%-10s %-15s %10s %10s %10s %10s %8s\n", 
              "Exp", "Potency", "Mean", "SD", "Min", "Max", "N_bags"))
  cat(strrep("-", 80), "\n")
  for (i in 1:nrow(by_exp_pot)) {
    cat(sprintf("%-10s %-15s %10.3f %10.3f %10.3f %10.3f %8d\n",
                paste0("ASPS_", by_exp_pot$experiment_number[i]),
                by_exp_pot$potency[i],
                by_exp_pot$Mean[i],
                by_exp_pot$SD[i],
                by_exp_pot$Min[i],
                by_exp_pot$Max[i],
                by_exp_pot$N_bags[i]))
  }
}


# ANOVA analysis ----------------------------------------------------------

cat("\n\n")
cat(strrep("#", 80), "\n")
cat("ANOVA ANALYSIS (Type III Sum of Squares)\n")
cat(strrep("#", 80), "\n")
cat("Design: potency * experiment_number\n")
cat("Experimental unit: BAG-LEVEL MEANS\n")
cat(strrep("#", 80), "\n")

# Set contrast options for Type III ANOVA
options(contrasts = c("contr.sum", "contr.poly"))

# Perform ANOVA for each response variable
for (var in response_vars) {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("ANOVA: ", toupper(var), "\n")
  cat(strrep("=", 80), "\n")
  
  # Two-way ANOVA with interaction
  formula_str <- paste(var, "~ potency * experiment_number")
  model <- aov(as.formula(formula_str), data = df_bags)
  anova_results <- Anova(model, type = "III")
  
  cat("\n")
  print(anova_results, digits = 6)
  cat("\n")
  
  # Interpret results
  p_potency <- anova_results["potency", "Pr(>F)"]
  p_experiment <- anova_results["experiment_number", "Pr(>F)"]
  p_interaction <- anova_results["potency:experiment_number", "Pr(>F)"]
  
  cat("INTERPRETATION:\n")
  cat(sprintf("  Main effect of potency:       p = %.6f  %s\n", 
              p_potency,
              ifelse(p_potency < 0.001, "***",
                     ifelse(p_potency < 0.01, "**",
                            ifelse(p_potency < 0.05, "*",
                                   ifelse(p_potency < 0.10, ".",
                                          "ns"))))))
  cat(sprintf("  Main effect of experiment:     p = %.6f  %s\n", 
              p_experiment,
              ifelse(p_experiment < 0.001, "***",
                     ifelse(p_experiment < 0.01, "**",
                            ifelse(p_experiment < 0.05, "*",
                                   ifelse(p_experiment < 0.10, ".",
                                          "ns"))))))
  cat(sprintf("  Interaction (potency x exp):   p = %.6f  %s\n", 
              p_interaction,
              ifelse(p_interaction < 0.001, "***",
                     ifelse(p_interaction < 0.01, "**",
                            ifelse(p_interaction < 0.05, "*",
                                   ifelse(p_interaction < 0.10, ".",
                                          "ns"))))))
  
  cat("\nSignificance codes: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10, ns p≥0.10\n")
}


# Summary ----------------------------------------------------------------

cat("\n\n")
cat(strrep("#", 80), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("#", 80), "\n")
cat("Dataset: JZ experiments (ASPS 6-10)\n")
cat("Experimental unit: Bag-level means (~16 seeds per bag)\n")
cat("Total bags analyzed:", nrow(df_bags), "\n")
cat("Response variables:", paste(response_vars, collapse = ", "), "\n")
cat("Design: 6 potencies × 5 experiments (unbalanced due to missing bags)\n")
cat(strrep("#", 80), "\n")
cat("\n")