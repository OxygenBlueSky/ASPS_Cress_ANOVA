# ASPS Cress Length Analysis - JZ Experiments 6-10
# Analysis of sprout_length, root_length, seedling_length, and root_sprout_ratio
# Date: 2025-10-15
# 
# Experimental unit: BAG (mean of ~16 seeds per bag)
# Design: potency (6 levels) x experiment (5 levels)
# ANOVA: Type III SS with interaction term

# Notes:
# - trace JZ missing bags
# ASPS Cress Length Analysis - Complete Dataset (ASPS 1-10)
# Analysis of sprout_length, root_length, seedling_length, and root_sprout_ratio
# Date: 2025-10-20
# 
# Experimental unit: BAG (mean of ~16 seeds per bag)
# Design: potency (6 levels) x experiment (10 levels)
# ANOVA: Type III SS with interaction term
#
# Three analysis groups: ALL (1-10), AS (1-5), JZ (6-10)


# No skweness correction


# Libraries

library(readxl)
library(car)
library(here)
library(dplyr)
library(tidyr)
library(openxlsx)
library(emmeans)
library(ggplot2)
library(gridExtra)
library(cowplot)


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

df_parsed <- df_raw %>%
  separate(exp_no, into = c("experiment_number", "potency_code"), 
           sep = "_", remove = FALSE) %>%
  mutate(
    experiment_number = as.integer(experiment_number),
    bag = as.integer(bag),
    experimenter = ifelse(experiment_number <= 5, "AS", "JZ")
  )

cat("Parsed structure:\n")
cat("  Experiments:", paste(sort(unique(df_parsed$experiment_number)), collapse = ", "), "\n")
cat("  AS experiments (1-5):", sum(df_parsed$experiment_number <= 5), "seeds\n")
cat("  JZ experiments (6-10):", sum(df_parsed$experiment_number > 5), "seeds\n")
cat("  Potency codes:", paste(sort(unique(df_parsed$potency_code)), collapse = ", "), "\n")
cat("  Decoded potencies:", paste(sort(unique(df_parsed$potency)), collapse = ", "), "\n")


# Calculate bag-level means

cat("\n")
cat(strrep("=", 80), "\n")
cat("CALCULATING BAG-LEVEL MEANS\n")
cat(strrep("=", 80), "\n")
cat("Experimental unit: BAG (averaging individual seed measurements)\n")
cat(strrep("=", 80), "\n")

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

cat("\nBag-level dataset created:\n")
cat("  Total bags:", nrow(df_bags), "\n")
cat("  Seeds per bag: min =", min(df_bags$n_seeds), 
    ", max =", max(df_bags$n_seeds),
    ", mean =", round(mean(df_bags$n_seeds), 1), "\n")

cat("\n")
cat(strrep("=", 80), "\n")
cat("DETAILED BAG INVENTORY BY EXPERIMENT AND POTENCY\n")
cat(strrep("=", 80), "\n")

bags_detail <- df_bags %>%
  group_by(experiment_number, experimenter, potency_code, potency) %>%
  summarise(
    n_bags = n(),
    bags_present = paste(sort(unique(bag)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(experiment_number, potency_code)

cat(sprintf("\n%-5s %-8s %-8s %-12s %8s   %s\n", 
            "Exp", "Exptr", "Pot Code", "Potency", "N Bags", "Bag Numbers Present"))
cat(strrep("-", 100), "\n")

for (i in 1:nrow(bags_detail)) {
  cat(sprintf("%-5s %-8s %-8s %-12s %8d   %s\n",
              bags_detail$experiment_number[i],
              bags_detail$experimenter[i],
              bags_detail$potency_code[i],
              bags_detail$potency[i],
              bags_detail$n_bags[i],
              bags_detail$bags_present[i]))
}

cat("\n")
cat("Summary by experimenter:\n")
experimenter_summary <- df_bags %>%
  group_by(experimenter) %>%
  summarise(
    n_bags = n(),
    n_experiments = n_distinct(experiment_number),
    .groups = "drop"
  )
print(as.data.frame(experimenter_summary), row.names = FALSE)
cat("\n")


# Convert to factors for ANOVA

df_bags$experiment_number <- as.factor(df_bags$experiment_number)
df_bags$potency <- as.factor(df_bags$potency)
df_bags$potency_code <- as.factor(df_bags$potency_code)
df_bags$experimenter <- as.factor(df_bags$experimenter)


# Define analysis groups

analysis_groups <- list(
  ALL = list(
    name = "ALL DATA (ASPS 1-10)",
    data = df_bags,
    sheet_name = "ALL"
  ),
  AS = list(
    name = "AS ONLY (ASPS 1-5)",
    data = df_bags %>% filter(experimenter == "AS"),
    sheet_name = "AS"
  ),
  JZ = list(
    name = "JZ ONLY (ASPS 6-10)",
    data = df_bags %>% filter(experimenter == "JZ"),
    sheet_name = "JZ"
  )
)


# Comprehensive descriptive statistics

cat("\n")
cat(strrep("#", 80), "\n")
cat("DESCRIPTIVE STATISTICS\n")
cat(strrep("#", 80), "\n")

for (group_key in names(analysis_groups)) {
  
  group <- analysis_groups[[group_key]]
  df_subset <- group$data
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat(group$name, "\n")
  cat(strrep("=", 80), "\n")
  cat("N bags:", nrow(df_subset), "\n")
  cat("Experiments:", paste(sort(unique(df_subset$experiment_number)), collapse = ", "), "\n")
  
  for (var in response_vars) {
    
    cat("\n--- ", toupper(var), " ---\n", sep = "")
    
    overall <- df_subset %>%
      summarise(
        Mean = mean(!!sym(var), na.rm = TRUE),
        SD = sd(!!sym(var), na.rm = TRUE),
        Min = min(!!sym(var), na.rm = TRUE),
        Max = max(!!sym(var), na.rm = TRUE),
        N_bags = n()
      )
    
    cat("\nOverall:\n")
    cat(sprintf("  Mean ± SD: %.3f ± %.3f\n", overall$Mean, overall$SD))
    cat(sprintf("  Range: %.3f - %.3f\n", overall$Min, overall$Max))
    
    by_potency <- df_subset %>%
      group_by(potency) %>%
      summarise(
        Mean = mean(!!sym(var), na.rm = TRUE),
        SD = sd(!!sym(var), na.rm = TRUE),
        N_bags = n(),
        .groups = "drop"
      ) %>%
      arrange(potency)
    
    cat("\nBy Potency:\n")
    cat(sprintf("%-15s %10s %10s %8s\n", "Potency", "Mean", "SD", "N_bags"))
    cat(strrep("-", 50), "\n")
    for (i in 1:nrow(by_potency)) {
      cat(sprintf("%-15s %10.3f %10.3f %8d\n",
                  by_potency$potency[i],
                  by_potency$Mean[i],
                  by_potency$SD[i],
                  by_potency$N_bags[i]))
    }
  }
}


# ANOVA analysis

cat("\n\n")
cat(strrep("#", 80), "\n")
cat("ANOVA ANALYSIS (Type III Sum of Squares)\n")
cat(strrep("#", 80), "\n")
cat("Design: potency * experiment_number\n")
cat("Experimental unit: BAG-LEVEL MEANS\n")
cat(strrep("#", 80), "\n")

options(contrasts = c("contr.sum", "contr.poly"))

anova_results_list <- list()

for (group_key in names(analysis_groups)) {
  
  group <- analysis_groups[[group_key]]
  df_subset <- group$data
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat(group$name, "\n")
  cat(strrep("=", 80), "\n")
  
  group_results <- list()
  
  for (var in response_vars) {
    
    cat("\n--- ", toupper(var), " ---\n", sep = "")
    
    formula_str <- paste(var, "~ potency * experiment_number")
    model <- aov(as.formula(formula_str), data = df_subset)
    anova_results <- Anova(model, type = "III")
    
    print(anova_results, digits = 6)
    
    p_potency <- anova_results["potency", "Pr(>F)"]
    p_experiment <- anova_results["experiment_number", "Pr(>F)"]
    p_interaction <- anova_results["potency:experiment_number", "Pr(>F)"]
    
    cat("\nInterpretation:\n")
    cat(sprintf("  Potency:       p = %.6f  %s\n", 
                p_potency, ifelse(p_potency < 0.001, "***",
                                  ifelse(p_potency < 0.01, "**",
                                         ifelse(p_potency < 0.05, "*",
                                                ifelse(p_potency < 0.10, ".", "ns"))))))
    cat(sprintf("  Experiment:    p = %.6f  %s\n", 
                p_experiment, ifelse(p_experiment < 0.001, "***",
                                     ifelse(p_experiment < 0.01, "**",
                                            ifelse(p_experiment < 0.05, "*",
                                                   ifelse(p_experiment < 0.10, ".", "ns"))))))
    cat(sprintf("  Interaction:   p = %.6f  %s\n", 
                p_interaction, ifelse(p_interaction < 0.001, "***",
                                      ifelse(p_interaction < 0.01, "**",
                                             ifelse(p_interaction < 0.05, "*",
                                                    ifelse(p_interaction < 0.10, ".", "ns"))))))
    
    group_results[[var]] <- data.frame(
      Experiment = p_experiment,
      Potency = p_potency,
      Interaction = p_interaction
    )
  }
  
  anova_results_list[[group_key]] <- group_results
}


# Export ANOVA summary to Excel

cat("\n\n")
cat(strrep("#", 80), "\n")
cat("EXPORTING ANOVA SUMMARY TO EXCEL\n")
cat(strrep("#", 80), "\n\n")

wb_anova <- createWorkbook()

style_red <- createStyle(fgFill = "#FFB3BA")
style_orange <- createStyle(fgFill = "#FFDFBA")
style_lilac <- createStyle(fgFill = "#E0BBE4")
style_header <- createStyle(textDecoration = "bold", fgFill = "#D3D3D3")

for (group_key in names(analysis_groups)) {
  
  group <- analysis_groups[[group_key]]
  sheet_name <- group$sheet_name
  
  addWorksheet(wb_anova, sheet_name)
  
  results_list <- anova_results_list[[group_key]]
  results_df <- do.call(rbind, results_list)
  results_df <- data.frame(
    Parameter = rownames(results_df),
    results_df,
    stringsAsFactors = FALSE
  )
  
  for (col in c("Experiment", "Potency", "Interaction")) {
    for (row in 1:nrow(results_df)) {
      cell_value <- results_df[row, col]
      if (!is.na(suppressWarnings(as.numeric(cell_value)))) {
        results_df[row, col] <- sprintf("%.6f", as.numeric(cell_value))
      }
    }
  }
  
  writeData(wb_anova, sheet_name,
            paste0("ANOVA Summary: ", group$name),
            startRow = 1, startCol = 1)
  writeData(wb_anova, sheet_name, results_df, startRow = 3, rowNames = FALSE)
  
  addStyle(wb_anova, sheet_name, style_header,
           rows = 3, cols = 1:4, gridExpand = TRUE)
  
  for (row_idx in 1:nrow(results_df)) {
    for (col_idx in 2:4) {
      cell_value <- results_df[row_idx, colnames(results_df)[col_idx]]
      p_val <- suppressWarnings(as.numeric(cell_value))
      
      if (!is.na(p_val)) {
        excel_row <- row_idx + 3
        excel_col <- col_idx
        
        if (p_val < 0.01) {
          addStyle(wb_anova, sheet_name, style_red,
                   rows = excel_row, cols = excel_col)
        } else if (p_val < 0.05) {
          addStyle(wb_anova, sheet_name, style_orange,
                   rows = excel_row, cols = excel_col)
        } else if (p_val < 0.10) {
          addStyle(wb_anova, sheet_name, style_lilac,
                   rows = excel_row, cols = excel_col)
        }
      }
    }
  }
  
  setColWidths(wb_anova, sheet_name, cols = 1:4, widths = c(25, 15, 15, 15))
  
  legend_row <- nrow(results_df) + 5
  writeData(wb_anova, sheet_name, "Color Legend:", startRow = legend_row, startCol = 1)
  writeData(wb_anova, sheet_name, "Light lilac = p < 0.10", startRow = legend_row + 1, startCol = 1)
  writeData(wb_anova, sheet_name, "Light orange = p < 0.05", startRow = legend_row + 2, startCol = 1)
  writeData(wb_anova, sheet_name, "Light red = p < 0.01", startRow = legend_row + 3, startCol = 1)
  
  addStyle(wb_anova, sheet_name, style_lilac, rows = legend_row + 1, cols = 1)
  addStyle(wb_anova, sheet_name, style_orange, rows = legend_row + 2, cols = 1)
  addStyle(wb_anova, sheet_name, style_red, rows = legend_row + 3, cols = 1)
}

output_anova <- paste0(date2, "_cress_ANOVA_summary_ALL_AS_JZ.xlsx")
saveWorkbook(wb_anova, output_anova, overwrite = TRUE)

cat(sprintf("ANOVA summary exported to: %s\n", output_anova))
cat("File contains 3 sheets: ALL, AS, JZ\n")
cat("Color-coded p-values: lilac (p<0.10), orange (p<0.05), red (p<0.01)\n\n")


# POST HOC TESTS: emmeans with interaction contrasts

cat("\n\n")
cat(strrep("#", 80), "\n")
cat("POST HOC TESTS: emmeans pairwise comparisons\n")
cat(strrep("#", 80), "\n\n")

compute_emmeans_contrasts <- function(data_subset, variable) {
  
  n_experiments <- length(unique(data_subset$experiment_number))
  
  formula_str <- paste(variable, "~ potency * experiment_number")
  model <- aov(as.formula(formula_str), data = data_subset)
  
  remedies <- levels(data_subset$potency)
  n_remedies <- length(remedies)
  
  potency_matrix <- matrix(NA, nrow = n_remedies, ncol = n_remedies,
                           dimnames = list(remedies, remedies))
  interaction_matrix <- matrix(NA, nrow = n_remedies, ncol = n_remedies,
                               dimnames = list(remedies, remedies))
  
  emm_potency <- emmeans(model, ~ potency)
  pairs_potency <- pairs(emm_potency, adjust = "none")
  pairs_summary <- summary(pairs_potency)
  
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
                                pairs_by_exp_summary$contrast, fixed = FALSE)
          
          if (length(matching_rows) >= 2) {
            tryCatch({
              specific_contrasts <- pairs_by_exp[matching_rows]
              pairs_of_contrasts <- pairs(specific_contrasts)
              joint_test <- test(pairs_of_contrasts, joint = TRUE)
              p_val_int <- joint_test$p.value
              interaction_matrix[j, i] <- p_val_int
            }, error = function(e) {
              interaction_matrix[j, i] <- "Error"
            })
          }
        }
      }
    }, error = function(e) {
      interaction_matrix[lower.tri(interaction_matrix)] <- "Error"
    })
  } else {
    interaction_matrix[lower.tri(interaction_matrix)] <- "NA"
  }
  
  return(list(potency = potency_matrix, interaction = interaction_matrix))
}

wb_posthoc <- createWorkbook()

style_4dec <- createStyle(numFmt = "0.0000")
style_bold <- createStyle(textDecoration = "bold")

for (group_key in names(analysis_groups)) {
  
  group <- analysis_groups[[group_key]]
  df_subset <- group$data
  sheet_name <- group$sheet_name
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("Processing:", group$name, "\n")
  cat(strrep("=", 80), "\n")
  
  addWorksheet(wb_posthoc, sheet_name)
  
  current_row <- 1
  writeData(wb_posthoc, sheet_name,
            paste0("Post Hoc Tests: ", group$name),
            startRow = current_row, startCol = 1)
  addStyle(wb_posthoc, sheet_name, style_bold, rows = current_row, cols = 1)
  current_row <- current_row + 2
  
  for (var in response_vars) {
    
    cat(sprintf("  %s\n", var))
    
    results <- compute_emmeans_contrasts(df_subset, var)
    potency_matrix <- results$potency
    interaction_matrix <- results$interaction
    
    remedies <- rownames(potency_matrix)
    n_remedies <- length(remedies)
    
    writeData(wb_posthoc, sheet_name, var, startRow = current_row, startCol = 1)
    
    for (j in 1:n_remedies) {
      writeData(wb_posthoc, sheet_name, remedies[j],
                startRow = current_row, startCol = 1 + (j-1)*2 + 1)
      mergeCells(wb_posthoc, sheet_name, 
                 rows = current_row, 
                 cols = (1 + (j-1)*2 + 1):(1 + (j-1)*2 + 2))
    }
    
    addStyle(wb_posthoc, sheet_name, style_header,
             rows = current_row, cols = 1:(2*n_remedies + 1), gridExpand = TRUE)
    current_row <- current_row + 1
    
    writeData(wb_posthoc, sheet_name, "", startRow = current_row, startCol = 1)
    for (j in 1:n_remedies) {
      writeData(wb_posthoc, sheet_name, "potency",
                startRow = current_row, startCol = 1 + (j-1)*2 + 1)
      writeData(wb_posthoc, sheet_name, "interaction",
                startRow = current_row, startCol = 1 + (j-1)*2 + 2)
    }
    
    addStyle(wb_posthoc, sheet_name, style_header,
             rows = current_row, cols = 1:(2*n_remedies + 1), gridExpand = TRUE)
    current_row <- current_row + 1
    
    for (i in 1:n_remedies) {
      writeData(wb_posthoc, sheet_name, remedies[i],
                startRow = current_row, startCol = 1)
      
      for (j in 1:n_remedies) {
        col_pot <- 1 + (j-1)*2 + 1
        col_int <- col_pot + 1
        
        if (j < i) {
          p_pot <- potency_matrix[i, j]
          p_int <- interaction_matrix[i, j]
          
          if (is.numeric(p_pot) && !is.na(p_pot)) {
            writeData(wb_posthoc, sheet_name, p_pot,
                      startRow = current_row, startCol = col_pot)
            addStyle(wb_posthoc, sheet_name, style_4dec,
                     rows = current_row, cols = col_pot)
            
            if (p_pot < 0.01) {
              addStyle(wb_posthoc, sheet_name, style_red,
                       rows = current_row, cols = col_pot, stack = TRUE)
            } else if (p_pot < 0.05) {
              addStyle(wb_posthoc, sheet_name, style_orange,
                       rows = current_row, cols = col_pot, stack = TRUE)
            } else if (p_pot < 0.10) {
              addStyle(wb_posthoc, sheet_name, style_lilac,
                       rows = current_row, cols = col_pot, stack = TRUE)
            }
          }
          
          if (is.numeric(p_int) && !is.na(p_int)) {
            writeData(wb_posthoc, sheet_name, p_int,
                      startRow = current_row, startCol = col_int)
            addStyle(wb_posthoc, sheet_name, style_4dec,
                     rows = current_row, cols = col_int)
            
            if (p_int < 0.01) {
              addStyle(wb_posthoc, sheet_name, style_red,
                       rows = current_row, cols = col_int, stack = TRUE)
            } else if (p_int < 0.05) {
              addStyle(wb_posthoc, sheet_name, style_orange,
                       rows = current_row, cols = col_int, stack = TRUE)
            } else if (p_int < 0.10) {
              addStyle(wb_posthoc, sheet_name, style_lilac,
                       rows = current_row, cols = col_int, stack = TRUE)
            }
          }
        }
      }
      current_row <- current_row + 1
    }
    current_row <- current_row + 2
  }
  
  setColWidths(wb_posthoc, sheet_name, 
               cols = 1:(2*n_remedies + 1), 
               widths = c(15, rep(10, 2*n_remedies)))
  
  legend_row <- current_row + 1
  writeData(wb_posthoc, sheet_name, "Color Legend:", startRow = legend_row, startCol = 1)
  writeData(wb_posthoc, sheet_name, "Light lilac = p < 0.10", startRow = legend_row + 1, startCol = 1)
  writeData(wb_posthoc, sheet_name, "Light orange = p < 0.05", startRow = legend_row + 2, startCol = 1)
  writeData(wb_posthoc, sheet_name, "Light red = p < 0.01", startRow = legend_row + 3, startCol = 1)
  
  addStyle(wb_posthoc, sheet_name, style_lilac, rows = legend_row + 1, cols = 1)
  addStyle(wb_posthoc, sheet_name, style_orange, rows = legend_row + 2, cols = 1)
  addStyle(wb_posthoc, sheet_name, style_red, rows = legend_row + 3, cols = 1)
}

output_posthoc <- paste0(date2, "_cress_posthoc_emmeans_ALL_AS_JZ.xlsx")
saveWorkbook(wb_posthoc, output_posthoc, overwrite = TRUE)

cat(sprintf("\nPost hoc tests exported to: %s\n", output_posthoc))
cat("File contains 3 sheets: ALL, AS, JZ\n\n")


# Plotting: Normalized data by experiment

cat("\n\n")
cat(strrep("#", 80), "\n")
cat("CREATING NORMALIZED PLOTS BY EXPERIMENT\n")
cat(strrep("#", 80), "\n\n")

for (group_key in names(analysis_groups)) {
  
  group <- analysis_groups[[group_key]]
  df_subset <- group$data
  group_label <- group$sheet_name
  
  cat(sprintf("\n%s:\n", group$name))
  
  for (var in response_vars) {
    
    cat(sprintf("  %s\n", var))
    
    lactose_grand_mean <- df_subset %>%
      filter(potency == "Lactose") %>%
      pull(!!sym(var)) %>%
      mean(na.rm = TRUE)
    
    df_normalized <- df_subset %>%
      mutate(normalized_value = !!sym(var) / lactose_grand_mean)
    
    exp_plots <- list()
    experiments <- sort(unique(df_normalized$experiment_number))
    
    for (exp in experiments) {
      exp_data <- df_normalized %>% filter(experiment_number == exp)
      
      p <- ggplot(exp_data, aes(x = potency, y = normalized_value, fill = potency)) +
        geom_boxplot(outlier.size = 1.5, width = 0.7) +
        geom_hline(yintercept = 1.0, linetype = "dashed", color = "red", linewidth = 0.7) +
        scale_fill_brewer(palette = "Dark2", name = "Potency") +
        labs(title = paste0("Exp ", exp), x = NULL, y = "Normalized value") +
        theme_bw() +
        theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_text(size = 9),
              legend.position = "none",
              panel.grid.major.x = element_blank())
      
      exp_plots[[as.character(exp)]] <- p
    }
    
    p_legend <- ggplot(df_normalized, aes(x = potency, y = normalized_value, fill = potency)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Dark2", name = "Potency") +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 9))
    
    legend <- cowplot::get_legend(p_legend)
    
    n_exp <- length(experiments)
    plot_width <- max(35, n_exp * 7)
    
    combined_plot <- gridExtra::grid.arrange(
      grobs = exp_plots,
      ncol = n_exp,
      nrow = 1,
      top = grid::textGrob(
        paste0(var, " (", group_label, ")\nNormalized to Lactose grand mean = ", 
               sprintf("%.3f", lactose_grand_mean)),
        gp = grid::gpar(fontsize = 14, fontface = "bold")
      ),
      bottom = legend
    )
    
    output_plot <- paste0(date2, "_ASPScress_normalized_by_exp_", group_label, "_", var, ".png")
    ggsave(
      filename = output_plot,
      plot = combined_plot,
      width = plot_width,
      height = 12,
      dpi = 300,
      units = "cm"
    )
  }
}

cat("\nAll plots by experiment created\n\n")


# Plotting: Normalized data by potency

cat("\n\n")
cat(strrep("#", 80), "\n")
cat("CREATING NORMALIZED PLOTS BY POTENCY\n")
cat(strrep("#", 80), "\n\n")

for (group_key in names(analysis_groups)) {
  
  group <- analysis_groups[[group_key]]
  df_subset <- group$data
  group_label <- group$sheet_name
  
  cat(sprintf("\n%s:\n", group$name))
  
  for (var in response_vars) {
    
    cat(sprintf("  %s\n", var))
    
    lactose_grand_mean <- df_subset %>%
      filter(potency == "Lactose") %>%
      pull(!!sym(var)) %>%
      mean(na.rm = TRUE)
    
    df_normalized <- df_subset %>%
      mutate(normalized_value = !!sym(var) / lactose_grand_mean)
    
    potency_plots <- list()
    potencies <- sort(unique(df_normalized$potency))
    
    for (pot in potencies) {
      pot_data <- df_normalized %>% filter(potency == pot)
      
      p <- ggplot(pot_data, aes(x = experiment_number, y = normalized_value, 
                                fill = experiment_number)) +
        geom_boxplot(outlier.size = 1.5, width = 0.7) +
        geom_hline(yintercept = 1.0, linetype = "dashed", color = "red", linewidth = 0.7) +
        scale_fill_brewer(palette = "Set2", name = "Experiment") +
        labs(title = pot, x = NULL, y = "Normalized value") +
        theme_bw() +
        theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_text(size = 9),
              legend.position = "none",
              panel.grid.major.x = element_blank())
      
      potency_plots[[pot]] <- p
    }
    
    p_legend <- ggplot(df_normalized, aes(x = experiment_number, y = normalized_value, 
                                          fill = experiment_number)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Set2", name = "Experiment") +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 9))
    
    legend <- cowplot::get_legend(p_legend)
    
    combined_plot <- gridExtra::grid.arrange(
      grobs = potency_plots,
      ncol = 6,
      nrow = 1,
      top = grid::textGrob(
        paste0(var, " by Potency (", group_label, ")\nNormalized to Lactose grand mean = ", 
               sprintf("%.3f", lactose_grand_mean)),
        gp = grid::gpar(fontsize = 14, fontface = "bold")
      ),
      bottom = legend
    )
    
    output_plot <- paste0(date2, "_ASPScress_normalized_by_potency_", group_label, "_", var, ".png")
    ggsave(
      filename = output_plot,
      plot = combined_plot,
      width = 42,
      height = 12,
      dpi = 300,
      units = "cm"
    )
  }
}

cat("\nAll plots by potency created\n\n")


# Summary

cat("\n\n")
cat(strrep("#", 80), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("#", 80), "\n")
cat("Dataset: ASPS 1-10 complete\n")
cat("Experimental unit: Bag-level means (~16 seeds per bag)\n")
cat("Total bags analyzed:\n")
for (group_key in names(analysis_groups)) {
  group <- analysis_groups[[group_key]]
  cat(sprintf("  %s: %d bags\n", group$sheet_name, nrow(group$data)))
}
cat("\nResponse variables:", paste(response_vars, collapse = ", "), "\n")
cat("Design: 6 potencies × experiments (varies by group)\n")
cat("\nFiles exported:\n")
cat(sprintf("  1. ANOVA summary: %s (3 sheets)\n", output_anova))
cat(sprintf("  2. Post-hoc tests: %s (3 sheets)\n", output_posthoc))
cat("  3. Normalized plots: 24 PNG files total\n")
cat("     - 12 plots by experiment (4 vars × 3 groups)\n")
cat("     - 12 plots by potency (4 vars × 3 groups)\n")
cat(strrep("#", 80), "\n")
cat("\n")