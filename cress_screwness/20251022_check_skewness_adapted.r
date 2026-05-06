# Check skewness mathematically and visually of seedling_length, sprout_length, and root_length data
# A value of zero indicates that there is no skewness in the distribution
# Determine optimal transformation (excluding data points leading to left skewness)
#
# Version: 2025-10-22 14:30
# Adapted from Paul's skewness script for cress length data

library(readxl)
library(here)
library(openxlsx)
library(moments) # skewness and kurtosis
library(ggplot2)

# Read data
fil <- "251021_cress_length_ASPS_1-10_alldata_decoded_no_dublets"
df <- read_excel(here(paste0(fil, ".xlsx")), sheet = "Sheet 1")


###############################################################################################
############# FOR seedling_length (LAGES) ######################
###############################################################################################

# Check histogram before correction and degree of skewness
# Negative skew indicates the tail is on the left side of the distribution
ggp <- ggplot(data=df, aes(seedling_length)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.2) +
  xlim(min(df$seedling_length, na.rm = TRUE), max(df$seedling_length, na.rm = TRUE)) +
  labs(caption=paste("Skewness = ", round(skewness(df$seedling_length, na.rm = TRUE),2)))

ggp +  xlab("seedling_length (cm)")

########################### Skewness correction total length
# Calculate skewness for different cutoff values for small seedling_length entries
for (i in 0:10) {
  skw <- skewness(df$seedling_length[df$seedling_length > i])
  Skw <- round(skw, digits = 2)
  cat(paste0("ommitted values below ", i, " --> skewness = ", Skw))
  cat("\n")
}

cutoffL <- 8.0 # enter left cutoff for skewness correction (org. 5.0)

############################################

# Print histogram with double cutoff
hist(df$seedling_length, 
     xlim = c(min(df$seedling_length, na.rm = TRUE), max(df$seedling_length, na.rm = TRUE)),
     main = paste0("skewness = ", round(skewness(df$seedling_length, na.rm = TRUE),2)))
print(paste0("cutoff values are (cutoff Left: ", cutoffL))
hist(df$seedling_length[df$seedling_length > cutoffL], 
     xlim = c(min(df$seedling_length, na.rm = TRUE), max(df$seedling_length, na.rm = TRUE)),
     main = paste0("skewness at ", 
                   cutoffL, " - ", "= ",
                   round(skewness(df$seedling_length[df$seedling_length > cutoffL]),2)))

# Adapt dataframe and include Tseedling_length with cutoff values
df$Tseedling_length <- df$seedling_length
df$Tseedling_length[df$seedling_length <= cutoffL] <- NA # render cutoff areas NA

# Check histogram Tseedling_length
ggp <- ggplot(data=df, aes(Tseedling_length)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.2) +
  xlim(min(df$seedling_length, na.rm = TRUE), max(df$seedling_length, na.rm = TRUE)) +
  labs(caption=paste("Skewness = ", round(skewness(df$Tseedling_length, na.rm = TRUE),2)))

ggp +  xlab("Tseedling_length (cm)")

colnames(df)[colnames(df) == 'Tseedling_length'] <- paste0("Tseedling_length-CutOffL", cutoffL)  # add cutoff values to Tseedling_length column name

###############################################################################################
############# FOR sprout_length (LASPR) ######################
###############################################################################################

# Check histogram before correction and degree of skewness
# Negative skew indicates the tail is on the left side of the distribution
ggp <- ggplot(data=df, aes(sprout_length)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.2) +
  xlim(min(df$sprout_length, na.rm = TRUE), max(df$sprout_length, na.rm = TRUE)) +
  labs(caption=paste("Skewness = ", round(skewness(df$sprout_length, na.rm = TRUE),2)))

ggp +  xlab("sprout_length (cm)")

########################### Skewness correction shoot length
# Calculate skewness for different cutoff values for small sprout_length entries
for (i in seq(0, 3.4, by = 0.2)) {
  skw <- skewness(df$sprout_length[df$sprout_length > i])
  Skw <- round(skw, digits = 2)
  cat(paste0("ommitted values below ", i, " --> skewness = ", Skw))
  cat("\n")
}

cutoffL <- 2.8 # enter left cutoff for skewness correction (org. 2.0)

############################################

# Print histogram with double cutoff
hist(df$sprout_length, 
     xlim = c(min(df$sprout_length, na.rm = TRUE), max(df$sprout_length, na.rm = TRUE)),
     main = paste0("skewness = ", round(skewness(df$sprout_length, na.rm = TRUE),2)))
print(paste0("cutoff values are (cutoff Left: ", cutoffL))
hist(df$sprout_length[df$sprout_length > cutoffL], 
     xlim = c(min(df$sprout_length, na.rm = TRUE), m"251021_cress_length_ASPS_1-10_alldata_decoded_no_dublets_skewness_corr.xlsx"ax(df$sprout_length, na.rm = TRUE)),
     main = paste0("skewness at ", 
                   cutoffL, "= ",
                   round(skewness(df$sprout_length[df$sprout_length > cutoffL]),2)))

# Adapt dataframe and include Tsprout_length with cutoff values
df$Tsprout_length <- df$sprout_length
df$Tsprout_length[df$sprout_length <= cutoffL] <- NA # render cutoff areas NA

# Check histogram Tsprout_length
ggp <- ggplot(data=df, aes(Tsprout_length)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.2) +
  xlim(min(df$sprout_length, na.rm = TRUE), max(df$sprout_length, na.rm = TRUE)) +
  labs(caption=paste("Skewness = ", round(skewness(df$Tsprout_length, na.rm = TRUE),2)))

ggp +  xlab("Tsprout_length (cm)")

colnames(df)[colnames(df) == 'Tsprout_length'] <- paste0("Tsprout_length-CutOffL", cutoffL)  # add cutoff values to Tsprout_length column name


###############################################################################################
############# FOR root_length (LAWU) ######################
###############################################################################################

# Check histogram before correction and degree of skewness
# Negative skew indicates the tail is on the left side of the distribution
ggp <- ggplot(data=df, aes(root_length)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.2) +
  xlim(min(df$root_length, na.rm = TRUE), max(df$root_length, na.rm = TRUE)) +
  labs(caption=paste("Skewness = ", round(skewness(df$root_length, na.rm = TRUE),2)))

ggp +  xlab("root_length (cm)")

########################### Skewness correction root length
# Calculate skewness for different cutoff values for small root_length entries
for (i in seq(0, 5, by = 0.2)) {
  skw <- skewness(df$root_length[df$root_length > i])
  Skw <- round(skw, digits = 2)
  cat(paste0("ommitted values below ", i, " --> skewness = ", Skw))
  cat("\n")
}

cutoffL <- 4.8 # enter left cutoff for skewness correction (org. 1.6)

############################################

# Print histogram with double cutoff
hist(df$root_length, 
     xlim = c(min(df$root_length, na.rm = TRUE), max(df$root_length, na.rm = TRUE)),
     main = paste0("skewness = ", round(skewness(df$root_length, na.rm = TRUE),2)))
print(paste0("cutoff values are (cutoff Left: ", cutoffL))
hist(df$root_length[df$root_length > cutoffL], 
     xlim = c(min(df$root_length, na.rm = TRUE), max(df$root_length, na.rm = TRUE)),
     main = paste0("skewness at ", 
                   cutoffL, "= ",
                   round(skewness(df$root_length[df$root_length > cutoffL]),2)))

# Adapt dataframe and include Troot_length with cutoff values
df$Troot_length <- df$root_length
df$Troot_length[df$root_length <= cutoffL] <- NA # render cutoff areas NA

# Check histogram Troot_length
ggp <- ggplot(data=df, aes(Troot_length)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.2) +
  xlim(min(df$root_length, na.rm = TRUE), max(df$root_length, na.rm = TRUE)) +
  labs(caption=paste("Skewness = ", round(skewness(df$Troot_length, na.rm = TRUE),2)))

ggp +  xlab("Troot_length (cm)")

colnames(df)[colnames(df) == 'Troot_length'] <- paste0("Troot_length-CutOffL", cutoffL)  # add cutoff values to Troot_length column name

###############################################################################################################

# Export new datafile
write.xlsx(df, file = here(paste0(fil, "_skewness_corr.xlsx")))