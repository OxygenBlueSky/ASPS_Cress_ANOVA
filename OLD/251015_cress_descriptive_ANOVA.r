# ASPS Cress Length Analysis - JZ Experiments 6-10
# Analysis of sprout_length, root_length, seedling_length, and root_sprout_ratio
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
library(openxlsx)
library(emmeans)


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
response_vars <- c("sprout_length", "root_length", "seedling_length", "root_sprout_ratio")

# Calculate means per bag
df_bags <- df_parsed %>%
  group_by(experiment_number, potency_code, potency, bag, exp_no, label) %>%
  summarise(
    n_seeds = n(),
    sprout_length = mean(sprout_length, na.rm = TRUE),
    root_length = mean(root_length, na.rm = TRUE),
    seedling_length = mean(seedling_length, na.rm = TRUE),
    root_sprout_ratio = mean(root_sprout_ratio, na.rm = TRUE),
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


# Export ANOVA summary to Excel ------------------------------------------

cat("\n\n")
cat(strrep("#", 80), "\n")
cat("EXPORTING ANOVA SUMMARY TO EXCEL\n")
cat(strrep("#", 80), "\n\n")

# Create new workbook
wb_anova <- createWorkbook()

# Define color styles
style_red <- createStyle(fgFill = "#FFB3BA")
style_orange <- createStyle(fgFill = "#FFDFBA")
style_lilac <- createStyle(fgFill = "#E0BBE4")
style_header <- createStyle(textDecoration = "bold", fgFill = "#D3D3D3")

# Add worksheet
addWorksheet(wb_anova, "JZ")

# Initialize results list
results_list <- list()

# Run ANOVA for all response variables and store results
for (var in response_vars) {
  
  formula_str <- paste(var, "~ potency * experiment_number")
  model <- aov(as.formula(formula_str), data = df_bags)
  anova_results <- Anova(model, type = "III")
  
  results_list[[var]] <- data.frame(
    Experiment = anova_results["experiment_number", "Pr(>F)"],
    Potency = anova_results["potency", "Pr(>F)"],
    Interaction = anova_results["potency:experiment_number", "Pr(>F)"],
    stringsAsFactors = FALSE
  )
}

# Combine into dataframe
results_df <- do.call(rbind, results_list)
results_df <- data.frame(
  Parameter = rownames(results_df),
  results_df,
  stringsAsFactors = FALSE
)

# Format numeric columns to 6 decimals
for (col in c("Experiment", "Potency", "Interaction")) {
  for (row in 1:nrow(results_df)) {
    cell_value <- results_df[row, col]
    if (!is.na(suppressWarnings(as.numeric(cell_value)))) {
      results_df[row, col] <- sprintf("%.6f", as.numeric(cell_value))
    }
  }
}

# Write data to worksheet
writeData(wb_anova, "JZ",
          "ANOVA Summary: JZ Experiments 6-10 (Cress Length)",
          startRow = 1, startCol = 1)
writeData(wb_anova, "JZ", results_df, startRow = 3, rowNames = FALSE)

# Apply header style
addStyle(wb_anova, "JZ", style_header,
         rows = 3, cols = 1:4, gridExpand = TRUE)

# Apply color coding based on p-values
for (row_idx in 1:nrow(results_df)) {
  for (col_idx in 2:4) {
    
    col_name <- colnames(results_df)[col_idx]
    cell_value <- results_df[row_idx, col_name]
    
    p_val <- suppressWarnings(as.numeric(cell_value))
    
    if (!is.na(p_val)) {
      excel_row <- row_idx + 3
      excel_col <- col_idx
      
      if (p_val < 0.01) {
        addStyle(wb_anova, "JZ", style_red,
                 rows = excel_row, cols = excel_col)
      } else if (p_val < 0.05) {
        addStyle(wb_anova, "JZ", style_orange,
                 rows = excel_row, cols = excel_col)
      } else if (p_val < 0.10) {
        addStyle(wb_anova, "JZ", style_lilac,
                 rows = excel_row, cols = excel_col)
      }
    }
  }
}

# Set column widths
setColWidths(wb_anova, "JZ", cols = 1:4, widths = c(25, 15, 15, 15))

# Add legend
legend_row <- nrow(results_df) + 5
writeData(wb_anova, "JZ", "Color Legend:",
          startRow = legend_row, startCol = 1)
writeData(wb_anova, "JZ", "Light lilac = p < 0.10",
          startRow = legend_row + 1, startCol = 1)
writeData(wb_anova, "JZ", "Light orange = p < 0.05",
          startRow = legend_row + 2, startCol = 1)
writeData(wb_anova, "JZ", "Light red = p < 0.01",
          startRow = legend_row + 3, startCol = 1)

addStyle(wb_anova, "JZ", style_lilac, rows = legend_row + 1, cols = 1)
addStyle(wb_anova, "JZ", style_orange, rows = legend_row + 2, cols = 1)
addStyle(wb_anova, "JZ", style_red, rows = legend_row + 3, cols = 1)

# Save workbook
output_anova <- paste0(date2, "_cress_ANOVA_summary.xlsx")
saveWorkbook(wb_anova, output_anova, overwrite = TRUE)

cat(sprintf("ANOVA summary exported to: %s\n", output_anova))
cat("Color-coded p-values: lilac (p<0.10), orange (p<0.05), red (p<0.01)\n\n")


# POST HOC TESTS: emmeans with interaction contrasts ---------------------

cat("\n\n")
cat(strrep("#", 80), "\n")
cat("POST HOC TESTS: emmeans pairwise comparisons with interaction contrasts\n")
cat(strrep("#", 80), "\n\n")

# Function to compute emmeans contrasts
compute_emmeans_contrasts <- function(data_subset, variable) {
  
  # Check number of experiments
  n_experiments <- length(unique(data_subset$experiment_number))
  
  # Fit two-way ANOVA model
  formula_str <- paste(variable, "~ potency * experiment_number")
  model <- aov(as.formula(formula_str), data = data_subset)
  
  # Get remedy names
  remedies <- levels(data_subset$potency)
  n_remedies <- length(remedies)
  
  # Initialize result matrices
  potency_matrix <- matrix(NA, nrow = n_remedies, ncol = n_remedies,
                           dimnames = list(remedies, remedies))
  interaction_matrix <- matrix(NA, nrow = n_remedies, ncol = n_remedies,
                               dimnames = list(remedies, remedies))
  
  # STEP 1: Main effect pairwise comparisons
  emm_potency <- emmeans(model, ~ potency)
  
  cat(sprintf("\n  ANOVA table for %s:\n", variable))
  print(anova(model))
  cat("\n")
  
  pairs_potency <- pairs(emm_potency, adjust = "none")
  pairs_summary <- summary(pairs_potency)
  
  # Fill potency matrix
  for (i in 1:nrow(pairs_summary)) {
    contrast_name <- as.character(pairs_summary$contrast[i])
    parts <- strsplit(contrast_name, " - ")[[1]]
    remedy1 <- trimws(parts[1])
    remedy2 <- trimws(parts[2])
    
    p_val <- pairs_summary$p.value[i]
    
    idx1 <- which(remedies == remedy1)
    idx2 <- which(remedies == remedy2)
    
    if (idx1 > idx2) {
      potency_matrix[idx1, idx2] <- p_val
    } else {
      potency_matrix[idx2, idx1] <- p_val
    }
  }
  
  # STEP 2: Interaction contrasts
  if (n_experiments > 1) {
    
    tryCatch({
      emm_by_exp <- emmeans(model, ~ potency | experiment_number)
      pairs_by_exp <- pairs(emm_by_exp)
      pairs_by_exp_summary <- summary(pairs_by_exp)
      
      for (i in 1:(n_remedies-1)) {
        for (j in (i+1):n_remedies) {
          remedy1 <- remedies[i]
          remedy2 <- remedies[j]
          
          comparison_name <- paste(remedy1, "-", remedy2)
          matching_rows <- grep(paste0("^", comparison_name, "$"), 
                                pairs_by_exp_summary$contrast, 
                                fixed = FALSE)
          
          if (length(matching_rows) >= 2) {
            tryCatch({
              specific_contrasts <- pairs_by_exp[matching_rows]
              pairs_of_contrasts <- pairs(specific_contrasts)
              joint_test <- test(pairs_of_contrasts, joint = TRUE)
              p_val_int <- joint_test$p.value
              
              interaction_matrix[j, i] <- p_val_int
              
            }, error = function(e) {
              interaction_matrix[j, i] <- "Error/pair"
            })
          } else {
            interaction_matrix[j, i] <- "Error/no of exp"
          }
        }
      }
      
    }, error = function(e) {
      interaction_matrix[lower.tri(interaction_matrix)] <- "Error/function"
    })
    
  } else {
    interaction_matrix[lower.tri(interaction_matrix)] <- "NA (single exp)"
  }
  
  return(list(
    potency = potency_matrix,
    interaction = interaction_matrix
  ))
}

# Create Excel workbook for post-hoc tests
wb_posthoc <- createWorkbook()

# Define styles
style_4dec <- createStyle(numFmt = "0.0000")
style_bold <- createStyle(textDecoration = "bold")

cat(sprintf("\n"))
cat(strrep("=", 80), "\n")
cat("Processing post-hoc tests: JZ Experiments 6-10 (Cress Length)\n")
cat(strrep("=", 80), "\n")

# Add worksheet
addWorksheet(wb_posthoc, "JZ")

current_row <- 1

# Write title
writeData(wb_posthoc, "JZ",
          "Post Hoc Tests: JZ Experiments 6-10 (Cress Length)",
          startRow = current_row, startCol = 1)
addStyle(wb_posthoc, "JZ", style_bold, rows = current_row, cols = 1)

current_row <- current_row + 2

# Loop through response variables
for (var in response_vars) {
  
  cat(sprintf("\n--- Parameter: %s ---\n", var))
  
  # Compute contrasts
  results <- compute_emmeans_contrasts(df_bags, var)
  
  potency_matrix <- results$potency
  interaction_matrix <- results$interaction
  
  remedies <- rownames(potency_matrix)
  n_remedies <- length(remedies)
  
  # Write parameter name
  writeData(wb_posthoc, "JZ", var, startRow = current_row, startCol = 1)
  
  # Write remedy column headers (spanning 2 columns each)
  for (j in 1:n_remedies) {
    writeData(wb_posthoc, "JZ", remedies[j],
              startRow = current_row, startCol = 1 + (j-1)*2 + 1)
    mergeCells(wb_posthoc, "JZ", 
               rows = current_row, 
               cols = (1 + (j-1)*2 + 1):(1 + (j-1)*2 + 2))
  }
  
  addStyle(wb_posthoc, "JZ", style_header,
           rows = current_row, cols = 1:(2*n_remedies + 1), gridExpand = TRUE)
  
  current_row <- current_row + 1
  
  # Write sub-headers: "potency" and "interaction"
  writeData(wb_posthoc, "JZ", "", startRow = current_row, startCol = 1)
  
  for (j in 1:n_remedies) {
    writeData(wb_posthoc, "JZ", "potency",
              startRow = current_row, startCol = 1 + (j-1)*2 + 1)
    writeData(wb_posthoc, "JZ", "interaction",
              startRow = current_row, startCol = 1 + (j-1)*2 + 2)
  }
  
  addStyle(wb_posthoc, "JZ", style_header,
           rows = current_row, cols = 1:(2*n_remedies + 1), gridExpand = TRUE)
  
  current_row <- current_row + 1
  
  # Fill data rows
  for (i in 1:n_remedies) {
    
    # Write row label
    writeData(wb_posthoc, "JZ", remedies[i],
              startRow = current_row, startCol = 1)
    
    # Fill lower triangle
    for (j in 1:n_remedies) {
      
      col_pot <- 1 + (j-1)*2 + 1
      col_int <- col_pot + 1
      
      if (j < i) {
        
        p_pot <- potency_matrix[i, j]
        p_int <- interaction_matrix[i, j]
        
        # Write potency p-value
        if (is.numeric(p_pot) && !is.na(p_pot)) {
          writeData(wb_posthoc, "JZ", p_pot,
                    startRow = current_row, startCol = col_pot)
          
          addStyle(wb_posthoc, "JZ", style_4dec,
                   rows = current_row, cols = col_pot)
          
          if (p_pot < 0.01) {
            addStyle(wb_posthoc, "JZ", style_red,
                     rows = current_row, cols = col_pot, stack = TRUE)
          } else if (p_pot < 0.05) {
            addStyle(wb_posthoc, "JZ", style_orange,
                     rows = current_row, cols = col_pot, stack = TRUE)
          } else if (p_pot < 0.10) {
            addStyle(wb_posthoc, "JZ", style_lilac,
                     rows = current_row, cols = col_pot, stack = TRUE)
          }
        } else if (!is.na(p_pot)) {
          writeData(wb_posthoc, "JZ", as.character(p_pot),
                    startRow = current_row, startCol = col_pot)
        }
        
        # Write interaction p-value
        if (is.numeric(p_int) && !is.na(p_int)) {
          writeData(wb_posthoc, "JZ", p_int,
                    startRow = current_row, startCol = col_int)
          
          addStyle(wb_posthoc, "JZ", style_4dec,
                   rows = current_row, cols = col_int)
          
          if (p_int < 0.01) {
            addStyle(wb_posthoc, "JZ", style_red,
                     rows = current_row, cols = col_int, stack = TRUE)
          } else if (p_int < 0.05) {
            addStyle(wb_posthoc, "JZ", style_orange,
                     rows = current_row, cols = col_int, stack = TRUE)
          } else if (p_int < 0.10) {
            addStyle(wb_posthoc, "JZ", style_lilac,
                     rows = current_row, cols = col_int, stack = TRUE)
          }
        } else if (!is.na(p_int)) {
          writeData(wb_posthoc, "JZ", as.character(p_int),
                    startRow = current_row, startCol = col_int)
        }
      }
    }
    
    current_row <- current_row + 1
  }
  
  current_row <- current_row + 2
}

# Set column widths
setColWidths(wb_posthoc, "JZ", 
             cols = 1:(2*n_remedies + 1), 
             widths = c(15, rep(10, 2*n_remedies)))

# Add legend
legend_row <- current_row + 1

writeData(wb_posthoc, "JZ", "Color Legend:",
          startRow = legend_row, startCol = 1)

writeData(wb_posthoc, "JZ", "Light lilac = p < 0.10",
          startRow = legend_row + 1, startCol = 1)
addStyle(wb_posthoc, "JZ", style_lilac, rows = legend_row + 1, cols = 1)

writeData(wb_posthoc, "JZ", "Light orange = p < 0.05",
          startRow = legend_row + 2, startCol = 1)
addStyle(wb_posthoc, "JZ", style_orange, rows = legend_row + 2, cols = 1)

writeData(wb_posthoc, "JZ", "Light red = p < 0.01",
          startRow = legend_row + 3, startCol = 1)
addStyle(wb_posthoc, "JZ", style_red, rows = legend_row + 3, cols = 1)

# Add interpretation note
writeData(wb_posthoc, "JZ",
          "Note: 'potency' = Main effect p-value (marginal comparison averaged over experiments)",
          startRow = legend_row + 5, startCol = 1)

writeData(wb_posthoc, "JZ",
          "      'interaction' = Does this comparison vary across experiments? (interaction test)",
          startRow = legend_row + 6, startCol = 1)

writeData(wb_posthoc, "JZ",
          "      If interaction p-value is significant, the main effect may be misleading.",
          startRow = legend_row + 7, startCol = 1)

# Save workbook
output_posthoc <- paste0(date2, "_cress_posthoc_emmeans.xlsx")
saveWorkbook(wb_posthoc, output_posthoc, overwrite = TRUE)

cat(sprintf("\n\n"))
cat(strrep("=", 80), "\n")
cat(sprintf("Post hoc tests exported to: %s\n", output_posthoc))
cat(strrep("=", 80), "\n")
cat("File contains JZ sheet with 4 parameters\n")
cat("Each parameter shows triangular table with remedy comparisons\n")
cat("\nTwo p-values per comparison:\n")
cat("  - 'potency' column: Main effect (are remedies different on average?)\n")
cat("  - 'interaction' column: Does this difference vary by experiment?\n")
cat("\nColor coding: lilac (p<0.10), orange (p<0.05), red (p<0.01)\n")
cat(strrep("=", 80), "\n\n")


# Summary -----------------------------------------------------------------

cat("\n\n")
cat(strrep("#", 80), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("#", 80), "\n")
cat("Dataset: JZ experiments (ASPS 6-10)\n")
cat("Experimental unit: Bag-level means (~16 seeds per bag)\n")
cat("Total bags analyzed:", nrow(df_bags), "\n")
cat("Response variables:", paste(response_vars, collapse = ", "), "\n")
cat("Design: 6 potencies × 5 experiments (unbalanced due to missing bags)\n")
cat("\nFiles exported:\n")
cat("  1. ANOVA summary:", output_anova, "\n")
cat("  2. Post-hoc tests:", output_posthoc, "\n")
cat(strrep("#", 80), "\n")
cat("\n")