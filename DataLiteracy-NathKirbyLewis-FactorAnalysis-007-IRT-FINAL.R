# ---
# title: "Item Response Theory Analysis of the Data Literacy Scale"
# author: "Joseph Kirby, Jerome Lewis, and Ravi Nath"
# date: "8/24/2023"

# IRT Analysis

# Set up Data
rm(list=ls())

# Load pacakges
library(mirt)
library(GAIPE) # for confidence intervals

data <- read.csv(file.choose(), header = TRUE, sep=",")                                                      #data <- read.csv("DataForIRTAnalysis-V1.csv", header = TRUE, sep=",")

# Use the 12 question survey and review each factor individually, using mirt package and graded response model (grm)

# Select Factor for Analysis
# Factor 1
dfF1 <- subset(data, select = c(Q01:Q04,Category))
# Factor 2
dfF2 <- subset(data, select = c(Q06,Q07,Q09,Q10,Category))
# Factor 3
dfF3 <- subset(data, select = c(Q11:Q14,Category))

##################
#FACTOR 1
##################
model1 <- mirt(data = dfF1[,1:4],verbose=TRUE, model = 1, itemtype = "graded",SE=TRUE)
options(digits=2)
#summary(model1)                                                                 
M2(model1,type="C2", calcnull=FALSE, SE=TRUE,printSE=TRUE)
coef(model1,IRTpars=T,se=TRUE,printSE=TRUE)
plot(model1, type = "trace")
# Item generalized S-X2 and RMSEA Indexes
itemfit(model1,empirical.CI = .95,printSE=TRUE)
# confidence intervals for items
CI.RMSEA(rmsea=.034, df=26, N=796, clevel = 0.95) # Item 1
CI.RMSEA(rmsea=.026, df=21,  N=796, clevel = 0.95) # Item 2
CI.RMSEA(rmsea=.015, df=31,  N=796, clevel = 0.95) # Item 3
CI.RMSEA(rmsea=.00, df=23,  N=796, clevel = 0.95) # Item 4

##################
#FACTOR 2
##################
model2 <- mirt(data = dfF2[,1:4],verbose=TRUE, model = 1, itemtype = "graded",SE=TRUE)
options(digits=2)
#summary(model2)                                                                 
M2(model2,type="C2", SE=TRUE,printSE=TRUE)                                       # Optionally put in ,CI=.95
coef(model2,IRTpars=T,se=TRUE,printSE=TRUE)
plot(model2, type = "trace")
# Item generalized S-X2 and RMSEA Indexes
itemfit(model2,empirical.CI = .95,printSE=TRUE)
# confidence intervals for items
CI.RMSEA(rmsea=.00, df=19, N=796, clevel = 0.95) # Item 6
CI.RMSEA(rmsea=.019, df=31,  N=796, clevel = 0.95) # Item 7
CI.RMSEA(rmsea=.036, df=17,  N=796, clevel = 0.95) # Item 9
CI.RMSEA(rmsea=.034, df=18,  N=796, clevel = 0.95) # Item 10

##################
#FACTOR 3
##################
model3 <- mirt(data = dfF3[,1:4],verbose=TRUE, model = 1, itemtype = "graded",SE=TRUE)
options(digits=2)
#summary(model3)                                                                 # Shows how it loads on the factor. 5 did not doo well;./
M2(model3,type="C2", SE=TRUE,printSE=TRUE)
coef(model3,IRTpars=T,se=TRUE,printSE=TRUE)
plot(model3, type = "trace")
# Item generalized S-X2 and RMSEA Indexes
itemfit(model3,empirical.CI = .95,printSE=TRUE)
# confidence intervals for items
CI.RMSEA(rmsea=.033, df=34, N=796, clevel = 0.95) # Item 11
CI.RMSEA(rmsea=.027, df=38,  N=796, clevel = 0.95) # Item 12
CI.RMSEA(rmsea=.015, df=29,  N=796, clevel = 0.95) # Item 13
CI.RMSEA(rmsea=.019, df=31,  N=796, clevel = 0.95) # Item 14

