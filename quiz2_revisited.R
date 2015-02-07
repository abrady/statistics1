# quiz 2 revisited
# Working memory training is a rapidly growing market with potential to further expand in the 
# future. Several computerized software programs promoting cognitive improvements have been 
# developed in recent years, with controversial results and implications. In a distinct literature,
# aerobic exercise has been shown to broadly enhance cognitive functions, in humans and animals. 
# We are attempting to bring together these two trends of research, leading to an emerging third
# approach: designed sports training. Specifically designed sports – wrestling, fencing, martial
# arts – which tax working memory by incorporating motion in three-dimensional space, are an optimal
# way to combine the benefits of traditional cognitive training and aerobic exercise into a single 
# activity. Then, write an R script to answer the following questions, based on simulated data 
# available here. All answers should be rounded to 2 significant digits.

# Check your working directory
getwd()
# If necessary, set your working directory
setwd("~/ocw/statistics1");


# Load packages
library(psych)

# 2 decimals
# options(digits=2)

# Read data into a dataframe called PE (physical endurance)
data <- read.table("Stats1.13.HW.02.txt", header = T)

cat('\nHow many rows of data are in the data file? ',nrow(data))
cat('\nnmean(SR): ',mean(data$SR))
cat('\nvar(SR): ',var(data$SR))
