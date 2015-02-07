# Cognitive training is a rapidly growing market with potential to further expand in the future. 
# Several computerized software programs promoting cognitive improvements have been developed in 
# recent years, with controversial results and implications. In a distinct literature, aerobic exercise
# has been shown to broadly enhance cognitive functions, in humans and animals. My research group is 
# attempting to bring together these two trends of research, leading to an emerging third approach: 
# designed sport training. Specifically designed sports are an optimal way to combine the benefits of
# traditional cognitive training and aerobic exercise into a single activity. 
# So, suppose we conducted a training experiment in which subjects were randomly assigned to one of two conditions:
# Designed sport training (des).
# and Aerobic training (aer).
# 
# Also, assume that we measured both verbal and spatial reasoning before and after training, 
# using four separate measures: • S1 •  S2 •	V1 •	V2. 
# Simulated data are available here. 
# Save the file to your computer and read it into R to complete the assignment and answer the following questions.
# https://spark-public.s3.amazonaws.com/stats1/datafiles/Stats1.13.HW.03.txt

# Statistics One, 2013, Lab 4

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlation analyses
#   Conduct regression analyses, unstandardized
#   Conduct regression analyses, standardized

# Example
#   A correlational study investigating predictors of physcial endurance in adults
#     Outcome variable (Y) is physical endurance
#     Predictors (X) are age and number of years actively engaged in exercise/sports
#     Sample size is N = 200

# Check your working directory
getwd()
# If necessary, set your working directory
setwd("~/ocw/statistics1");


# Load packages
library(psych)

# 2 decimals
options(digits=2)

# Read data into a dataframe called PE (physical endurance)
PE <- read.table("Stats1.13.HW.03.txt", header = T)

# If you want to view the data
#View(PE)
#edit(PE)

# Summary statistics
describe(PE)
describeBy(PE, PE$cond) # break it down by type

# Correlation analysis 
cor(PE[3:ncol(PE)]) # matrix of correlation for all data

# let's see how 
des = subset(PE, PE$cond=='des')

c = cor(PE$S1.pre, PE$S2.pre)
cat('Q1: cor between s1 and s2 is', c,'\n')
cat('Q2: What is the correlation between V1 and V2 pre-training:', cor(PE$V1.pre,PE$V2.pre),'\n')

# Q3: With respect to the measurement of two distinct constructs, 
# spatial reasoning and verbal reasoning, the pattern of correlations pre-training reveals:
#cat('Q3: cor of spatial with verbal:',cor(PE$V1.pre,PE$S1.pre), cor(PE$V2.pre,PE$S2.pre),'\n')
cat('Q3: cor of spatial with verbal:',cor(PE$V1.pre+PE$V2.pre,PE$S1.pre+PE$S2.pre),'\n')


# Q4: Correlations from the control group could be used to estimate test/retest reliability. 
# If so, which test is most reliable?
cat('Q4: ')
aer = subset(PE,PE$cond=="aer")
cat('cor of S1', cor(aer$S1.pre,aer$S1.post),',')
cat('cor of S2', cor(aer$S2.pre,aer$S2.post),',')
cat('cor of V1', cor(aer$V1.pre,aer$V1.post),',')
cat('cor of V2', cor(aer$V2.pre,aer$V2.post),',')

#cat('cor of S1',cor(subset(PE,data$cond=='aer',select='S1.pre'), subset(PE,data$cond=='aer',select='S1.post')))
#cat(', cor of S2',cor(subset(PE,data$cond=='aer',select='S2.pre'), subset(PE,data$cond=='aer',select='S2.post')))
#cat(', cor of V1',cor(subset(PE,data$cond=='aer',select='V1.pre'), subset(PE,data$cond=='aer',select='V1.post')))
#cat(', cor of V2',cor(subset(PE,data$cond=='aer',select='V2.pre'), subset(PE,data$cond=='aer',select='V2.post')))

#  Does there appear to be a correlation between spatial reasoning before training and the amount of 
# improvement in spatial reasoning?
cat('\nQ5:')
PE$spacial.pre = (PE$S1.pre + PE$S2.pre)/2
PE$spacial.post = (PE$S1.post + PE$S2.post)/2
PE$spacial.improve = (PE$spacial.post - PE$spacial.pre)
cat('correlation is ',cor(PE$spacial.pre, PE$spacial.improve))

cat('\nQ6:')
PE$spacial.pre = (PE$V1.pre + PE$V2.pre)/2
PE$spacial.post = (PE$V1.post + PE$V2.post)/2
PE$spacial.improve = (PE$spacial.post - PE$spacial.pre)
cat('correlation is ',cor(PE$spacial.pre, PE$spacial.improve))

# Which group exhibited more improvement in spatial reasoning?
cat('\nQ7: des')
improve = function(ds) {
  pre = subset(ds, select=c(S1.pre,S2.pre));
  post = subset(ds, select=c(S1.post,S2.post))
  (post[1] + post[2])/2 - (pre[1] + pre[2])/2
}
des = subset(PE,PE$cond=='des')
aer = subset(PE,PE$cond=='aer')
des$spacial.improve = improve(des)
aer$spacial.improve = improve(aer)

cat('Q8: Color scatterplot matrix, colored and ordered by magnitude of r')
base <- subset(PE, select=c('S1.pre','S2.pre','V1.pre','V2.pre'))
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r) 
cpairs(base, base.order, panel.colors = base.color, gap = .5,
       main = "Variables Ordered and Colored by Correlation")

cat('Q9: Color scatterplot matrix, colored and ordered by magnitude of r')
base <- subset(PE, select=c('S1.post','S2.post','V1.post','V2.post'))
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r) 
cpairs(base, base.order, panel.colors = base.color, gap = .5,
       main = "Variables Ordered and Colored by Correlation")

cat('Q10: major change from pre to post')
base <- subset(PE, select=c('S1.post','S2.post','V1.post','V2.post','S1.pre','S2.pre','V1.pre','V2.pre'))
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r) 
cpairs(base, base.order, panel.colors = base.color, gap = .5,
       main = "Variables Ordered and Colored by Correlation")
