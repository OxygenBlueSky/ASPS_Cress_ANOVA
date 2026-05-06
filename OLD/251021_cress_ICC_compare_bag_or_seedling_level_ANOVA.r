# ASPS Cress Length Analysis - Seedling-Level with ICC Quantification
# Compares bag-level vs seedling-level analysis
# Quantifies intraclass correlation and design effects
# Date: 2025-10-21 14:30
#
# Purpose: Demonstrate pseudoreplication effects when treating seedlings as independent
# WARNING: Seedling-level analysis inflates Type I error due to non-independence


# Libraries

library(readxl)
library(car)
library(here)
library(dplyr)
library(tidyr)
library(ICC)


# Determine export date

date <- Sys.Date() 
date2 <- gsub("-| |UTC", "", date)


# Read data

cat("\n")
cat(strrep("=", 80), "\n")
cat("READING DATA\n")
cat(strrep("=", 80), "\n")

filename <- "251021_cress_length_ASPS_1-10_alldata_decoded_no_dublets.xlsx"
df_raw <- read_excel(here(filename), sheet = "Sheet 1")

cat("Raw data loaded:\n")
cat("  Total observations (individual seeds):", nrow(df_raw), "\n")
cat("  Variables:", paste(colnames(df_raw), collapse = ", "), "\n")


# Parse experiment number and potency code

cat("\n")
cat(strrep("=", 80), "\n")
cat("PARSING EXPERIMENT AND POTENCY\n")
cat(strrep("=", 80), "\n")

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


# Calculate bag-level means for comparison

response_vars <- c("sprout_length", "root_length", "seedling_length", "root_sprout_ratio")

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


# Convert to factors for ANOVA

df_seedlings$experiment_number <- as.factor(df_seedlings$experiment_number)
df_seedlings$potency <- as.factor(df_seedlings$potency)
df_seedlings$experimenter <- as.factor(df_seedlings$experimenter)
df_seedlings$bag_id <- as.factor(df_seedlings$bag_id)

df_bags$experiment_number <- as.factor(df_bags$experiment_number)
df_bags$potency <- as.factor(df_bags$potency)
df_bags$experimenter <- as.factor(df_bags$experimenter)


# Define analysis groups

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


# Calculate Intraclass Correlation Coefficient (ICC)

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


# ANOVA Comparison: Bag-level vs Seedling-level

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
    
    # Check for spurious significance
    if (p_pot_seedlings < 0.05 && p_pot_bags >= 0.05) {
      cat("\n  ⚠️  ALERT: Potency effect significant at seedling-level but NOT at bag-level!\n")
      cat("      This is likely a FALSE POSITIVE due to pseudoreplication.\n")
    }
    if (p_int_seedlings < 0.05 && p_int_bags >= 0.05) {
      cat("\n  ⚠️  ALERT: Interaction significant at seedling-level but NOT at bag-level!\n")
      cat("      This is likely a FALSE POSITIVE due to pseudoreplication.\n")
    }
  }
}
