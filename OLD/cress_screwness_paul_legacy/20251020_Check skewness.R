#check skewness mathematically and visually of LAGES, LASPR, and LAWU data
#A value of zero indicates that there is no skewness in the distribution
#determine optimal transformation (excluding data points leading to left skewness)

library(readxl)
library(openxlsx)
library(moments) #skewness and kurtosis
library(ggplot2)

setwd("/home/paul/Ongoing Projects/School project/Cress length/20250915_SNC2/")
list.files()
fil <- "20251015-1-198 Length_Eval"           # data file with LAGES data
df <- read_xlsx(paste0(fil, ".xlsx"))  # get the data


###############################################################################################
############# FOR LAGES ######################
###############################################################################################

#check histogram before correction and degree of skewness
#Negative skew indicates the tail is on the left side of the distribution
ggp <- ggplot(data=df, aes(LAGES)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.2) +
  xlim(min(df$LAGES), max(df$LAGES)) +
  labs(caption=paste("Skewness = ", round(skewness(df$LAGES, na.rm = TRUE),2)))

ggp +  xlab("LAGES (cm)")
  
########################### Skewness correction total length
#calculate skewness for different cutoff values for small LAGES entries
for (i in 0:8) {
  skw <- skewness(df$LAGES[df$LAGES > i])
  Skw <- round(skw, digits = 2)
  cat(paste0("ommitted values below ", i, " --> skewness = ", Skw))
  cat("\n")
}

cutoffL <- 5.0 #enter left cutoff for skewness correction

############################################

#print histogram with double cutoff
hist(df$LAGES, 
     xlim = c(min(df$LAGES), max(df$LAGES)),
     main = paste0("skewness = ", round(skewness(df$LAGES),2)))
print(paste0("cutoff values are (cutoff Left: ", cutoffL))
hist(df$LAGES[df$LAGES > cutoffL], 
     xlim = c(min(df$LAGES), max(df$LAGES)),
     main = paste0("skewness at ", 
                   cutoffL, " - ", "= ",
                   round(skewness(df$LAGES[df$LAGES > cutoffL]),2)))
     
#adapt dataframe and include TLAGES with cutoff values
df$TLAGES <- df$LAGES
df$TLAGES[df$LAGES <= cutoffL] <- NA #render cutoff areas NA

#check histogram TLAGES
ggp <- ggplot(data=df, aes(TLAGES)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.2) +
  xlim(min(df$LAGES), max(df$LAGES)) +
  labs(caption=paste("Skewness = ", round(skewness(df$TLAGES, na.rm = TRUE),2)))

ggp +  xlab("TLAGES (cm)")

colnames(df)[colnames(df) == 'TLAGES'] <- paste0("TLAGES-CutOffL", cutoffL)  # add cutoff values to TLAGES column name

###############################################################################################
############# FOR LASPR ######################
###############################################################################################

#check histogram before correction and degree of skewness
#Negative skew indicates the tail is on the left side of the distribution
ggp <- ggplot(data=df, aes(LASPR)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.2) +
  xlim(min(df$LASPR), max(df$LASPR)) +
  labs(caption=paste("Skewness = ", round(skewness(df$LASPR, na.rm = TRUE),2)))

ggp +  xlab("LASPR (cm)")

########################### Skewness correction shoot length
#calculate skewness for different cutoff values for small LASPR entries
for (i in seq(0, 3, by = 0.2)) {
  skw <- skewness(df$LASPR[df$LASPR > i])
  Skw <- round(skw, digits = 2)
  cat(paste0("ommitted values below ", i, " --> skewness = ", Skw))
  cat("\n")
}

cutoffL <- 2.0 #enter left cutoff for skewness correction

############################################

#print histogram with double cutoff
hist(df$LASPR, 
     xlim = c(min(df$LASPR), max(df$LASPR)),
     main = paste0("skewness = ", round(skewness(df$LASPR),2)))
print(paste0("cutoff values are (cutoff Left: ", cutoffL))
hist(df$LASPR[df$LASPR > cutoffL], 
     xlim = c(min(df$LASPR), max(df$LASPR)),
     main = paste0("skewness at ", 
                   cutoffL, "= ",
                   round(skewness(df$LASPR[df$LASPR > cutoffL]),2)))

#adapt dataframe and include TLASPR with cutoff values
df$TLASPR <- df$LASPR
df$TLASPR[df$LASPR <= cutoffL] <- NA #render cutoff areas NA

#check histogram TLASPR
ggp <- ggplot(data=df, aes(TLASPR)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.2) +
  xlim(min(df$LASPR), max(df$LASPR)) +
  labs(caption=paste("Skewness = ", round(skewness(df$TLASPR, na.rm = TRUE),2)))

ggp +  xlab("TLASPR (cm)")

colnames(df)[colnames(df) == 'TLASPR'] <- paste0("TLASPR-CutOffL", cutoffL)  # add cutoff values to TLASPR column name


###############################################################################################
############# FOR LAWU ######################
###############################################################################################

#check histogram before correction and degree of skewness
#Negative skew indicates the tail is on the left side of the distribution
ggp <- ggplot(data=df, aes(LAWU)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.2) +
  xlim(min(df$LAWU), max(df$LAWU)) +
  labs(caption=paste("Skewness = ", round(skewness(df$LAWU, na.rm = TRUE),2)))

ggp +  xlab("LAWU (cm)")

########################### Skewness correction root length
#calculate skewness for different cutoff values for small LAWU entries
for (i in seq(0, 3, by = 0.2)) {
  skw <- skewness(df$LAWU[df$LAWU > i])
  Skw <- round(skw, digits = 2)
  cat(paste0("ommitted values below ", i, " --> skewness = ", Skw))
  cat("\n")
}

cutoffL <- 1.6 #enter left cutoff for skewness correction

############################################

#print histogram with double cutoff
hist(df$LAWU, 
     xlim = c(min(df$LAWU), max(df$LAWU)),
     main = paste0("skewness = ", round(skewness(df$LAWU),2)))
print(paste0("cutoff values are (cutoff Left: ", cutoffL))
hist(df$LAWU[df$LAWU > cutoffL], 
     xlim = c(min(df$LAWU), max(df$LAWU)),
     main = paste0("skewness at ", 
                   cutoffL, "= ",
                   round(skewness(df$LAWU[df$LAWU > cutoffL]),2)))

#adapt dataframe and include TLAWU with cutoff values
df$TLAWU <- df$LAWU
df$TLAWU[df$LAWU <= cutoffL] <- NA #render cutoff areas NA

#check histogram TLAWU
ggp <- ggplot(data=df, aes(TLAWU)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.2) +
  xlim(min(df$LAWU), max(df$LAWU)) +
  labs(caption=paste("Skewness = ", round(skewness(df$TLAWU, na.rm = TRUE),2)))

ggp +  xlab("TLAWU (cm)")

colnames(df)[colnames(df) == 'TLAWU'] <- paste0("TLAWU-CutOffL", cutoffL)  # add cutoff values to TLAWU column name

###############################################################################################################

#export new datafile
write.xlsx(df, file = paste0(fil, "-SkewnessCorr.xlsx"))
