# quiz 3 revisited

# Check your working directory
getwd()
# If necessary, set your working directory
setwd("~/ocw/statistics1");


# Load packages
library(psych)

# 2 decimals
# options(digits=2)

# Read data into a dataframe called PE (physical endurance)
data <- read.table("Stats1.13.HW.03.txt", header = T)
