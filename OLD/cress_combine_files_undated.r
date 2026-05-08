# Combine ASPS 1-10 cress length data with potency decoding
# Date: 2025-01-20
# This script combines ASPS 1-5 data files with ASPS 6-10 data
# and adds potency information from the decoding table

# Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)


# Read decoding table
decode_potency <- function() {
  # Read CSV with proper skip
  decoding <- read.csv("ASPS1-10-decoding table.csv", skip = 3, header = TRUE,
                       stringsAsFactors = FALSE)
  
  # Clean column names
  names(decoding) <- c("Experiment_number", "Lactose", "Stannum", 
                       "Silicea", "Sulphur", "Ars.Album", "Mercury")
  
  # Remove any extra rows that might be empty
  decoding <- decoding[!is.na(decoding$Experiment_number) & decoding$Experiment_number != "", ]
  
  # Convert experiment number to numeric
  decoding$Experiment_number <- as.numeric(decoding$Experiment_number)
  
  return(decoding)
}


# Function to get potency name from experiment number and code letter
get_potency <- function(exp_num, code_letter, decoding_table) {
  # Handle NA or empty inputs
  if (is.na(exp_num) || is.na(code_letter) || code_letter == "") {
    return(NA)
  }
  
  # Find the row for this experiment
  exp_row <- decoding_table[decoding_table$Experiment_number == exp_num, ]
  
  if (nrow(exp_row) == 0) {
    return(NA)
  }
  
  # Ensure we have exactly one row
  if (nrow(exp_row) > 1) {
    warning(paste("Multiple rows found for experiment", exp_num, "- using first"))
    exp_row <- exp_row[1, ]
  }
  
  # Convert code to uppercase for matching
  code_letter <- toupper(as.character(code_letter))
  
  # Find which column contains this code letter
  remedy_cols <- c("Lactose", "Stannum", "Silicea", "Sulphur", "Ars.Album", "Mercury")
  
  for (remedy in remedy_cols) {
    # Get the value and ensure it's a single character string
    remedy_code <- as.character(exp_row[[remedy]][1])
    
    # Compare uppercase versions
    if (!is.na(remedy_code) && toupper(remedy_code) == code_letter) {
      return(remedy)
    }
  }
  
  return(NA)
}


# Read and process ASPS 1-5 files
process_asps_1_5 <- function() {
  
  # List of ASPS 1-5 files
  asps_files <- c(
    "20251003-ASPS1gerade_labeled.xlsx",
    "20251003-ASPS1ungerade_labeled.xlsx",
    "20251003-ASPS2ungerade_labeled.xlsx",
    "20251020-ASPS2gerade_labeled.xlsx",  # Second ASPS2 gerade file
    "20251003-ASPS3gerade_labeled.xlsx",
    "20251003-ASPS3ungerade_labeled.xlsx",
    "20251003-ASPS4gerade_labeled.xlsx",
    "20251003-ASPS4ungerade_labeled.xlsx",
    "20251003-ASPS5gerade_labeled.xlsx",
    "20251003-ASPS5ungerade_labeled.xlsx"
  )
  
  # Read decoding table
  decoding_table <- decode_potency()
  
  # Initialize empty dataframe
  all_data <- data.frame()
  
  # Process each file
  for (file in asps_files) {
    
    cat("Processing:", file, "\n")
    
    # Read Excel file
    df <- read_excel(file, sheet = 1)
    
    # Remove the unnamed column (column 2 which contains "skift")
    # The columns should be: Label, unnamed, exp no, code, bag no, LASPR, LAGES, LAWU, LAWUSPR
    if (ncol(df) >= 2) {
      # Identify the column with "skift" - it's typically the second column
      col_names <- names(df)
      if (col_names[2] == "...2" || col_names[2] == "" || is.na(col_names[2])) {
        df <- df[, -2]  # Remove the second column
      }
    }
    
    # Extract ASPS experiment number from Label
    # Label format: "ASPS1_ge_P4200280.jpg:1138-0702" -> extract "1"
    df$asps_exp_num <- as.numeric(str_extract(df$Label, "(?<=ASPS)\\d+"))
    
    # Create new label format: ASPS_[exp_num]_[code]_[bag_no]
    df$new_label <- paste0("ASPS_", df$asps_exp_num, "_", 
                           toupper(df$code), "_", df$`bag no`)
    
    # Create exp_no in format: [exp_num]_[code]
    df$exp_no <- paste0(df$asps_exp_num, "_", toupper(df$code))
    
    # Get potency for each row
    df$potency <- mapply(get_potency, 
                         df$asps_exp_num, 
                         df$code, 
                         MoreArgs = list(decoding_table = decoding_table))
    
    # Rename columns to match ASPS 6-10 format
    df_clean <- df %>%
      select(
        label = new_label,
        sprout_length = LASPR,
        seedling_length = LAGES,
        root_length = LAWU,
        root_sprout_ratio = LAWUSPR,
        exp_no = exp_no,
        bag = `bag no`,
        potency = potency
      )
    
    # Add to combined dataframe
    all_data <- rbind(all_data, df_clean)
  }
  
  # Add sequential count column
  all_data$count <- seq_len(nrow(all_data))
  
  # Add empty reference_cell column to match ASPS 6-10 format
  all_data$reference_cell <- NA
  
  # Reorder columns to match ASPS 6-10 format
  all_data <- all_data %>%
    select(reference_cell, count, label, sprout_length, seedling_length,
           root_length, root_sprout_ratio, exp_no, bag, potency)
  
  return(all_data)
}


# Read ASPS 6-10 data
read_asps_6_10 <- function() {
  df <- read_excel("only_combined_data_Kresselaenge_ASPS_6-10_SL.xlsx", sheet = 1)
  
  # Remove LASPR1/2 and LAGES1/2 columns as they're not in ASPS 1-5
  df <- df %>%
    select(-`LASPR1/2`, -`LAGES1/2`)
  
  return(df)
}


# Main execution
main <- function() {
  
  cat("Starting ASPS 1-10 data combination\n")
  cat("=====================================\n\n")
  
  # Process ASPS 1-5 files
  cat("Processing ASPS 1-5 files...\n")
  asps_1_5 <- process_asps_1_5()
  cat("ASPS 1-5 rows processed:", nrow(asps_1_5), "\n\n")
  
  # Read ASPS 6-10 data
  cat("Reading ASPS 6-10 data...\n")
  asps_6_10 <- read_asps_6_10()
  cat("ASPS 6-10 rows:", nrow(asps_6_10), "\n\n")
  
  # Combine datasets
  cat("Combining datasets...\n")
  combined_data <- rbind(asps_1_5, asps_6_10)
  
  # Sort by experiment number and code
  combined_data <- combined_data %>%
    arrange(exp_no, bag, count)
  
  cat("Total combined rows:", nrow(combined_data), "\n")
  cat("Experiments included:", paste(sort(unique(combined_data$exp_no)), collapse = ", "), "\n\n")
  
  # Check for any missing potencies
  missing_potencies <- sum(is.na(combined_data$potency))
  if (missing_potencies > 0) {
    cat("WARNING: Found", missing_potencies, "rows with missing potency values\n")
    # Note: This might happen if there are issues with the decoding table
    # or if codes don't match exactly (case sensitivity, typos, etc.)
  }
  
  # Save to Excel
  output_filename <- "cress_length_ASPS_1-10_alldata_decoded.xlsx"
  write.xlsx(combined_data, file = output_filename, rowNames = FALSE)
  
  cat("\nData saved to:", output_filename, "\n")
  
  # Summary statistics
  cat("\n=====================================\n")
  cat("Summary Statistics:\n")
  cat("=====================================\n")
  
  # Summary by experiment
  summary_exp <- combined_data %>%
    group_by(exp_no, potency) %>%
    summarise(
      n_observations = n(),
      mean_sprout = mean(sprout_length, na.rm = TRUE),
      mean_seedling = mean(seedling_length, na.rm = TRUE),
      mean_root = mean(root_length, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(summary_exp)
  
  return(combined_data)
}


# Run the main function
combined_data <- main()


# Data quality notes:
# 1. ASPS data includes both "gerade" (even) and "ungerade" (odd) numbered samples
# 2. ASPS2 has two files from different dates (20251003 and 20251020) - paul processed one later
# 3. The original "exp no" column in ASPS 1-5 files was not used as it doesn't match the 
#    ASPS numbering scheme. Instead, experiment number is extracted from the Label field
# 4. The unnamed column containing "skift" markers has been removed entirely
# 5. LASPR1/2 and LAGES1/2 columns are not present in ASPS 1-5 data and have been 
#    excluded from the final combined dataset for consistency