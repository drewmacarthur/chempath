# Import libraries and data -----------------------------------------------

#import libraries
library(quantreg)
library(ggplot2)

#import datasets to be used

tsh_ft4_male <- read.csv("TSH_FT4_male.csv", header = TRUE,
                         sep = ",", dec = ".")

tsh_ft4_female <- read.csv("TSH_FT4_female.csv", header = TRUE,
                         sep = ",", dec = ".")

tsh_ft3_male <- read.csv("TSH_FT3_male.csv", header = TRUE,
                         sep = ",", dec = ".")

tsh_ft3_female <- read.csv("TSH_FT3_female.csv", header = TRUE,
                         sep = ",", dec = ".")

#check if numeric
str(tsh_ft4_female) #is numeric

# Inspect data ------------------------------------------------------------


#plot original data first

plot(tsh_ft4_female$FT4_result, tsh_ft4_female$TSH_result, 
     main = "FT4 plotted against TSH for females in TAD", 
     xlab = "FT4 (pmol/L)",
     ylab = "TSH (mIU/L)")

#plot FT4 against -log TSH 

plot(tsh_ft4_female$FT4_result, -log10(tsh_ft4_female$TSH_result), 
     main = "FT4 plotted against logarithmic TSH for females in TAD", 
     xlab = "FT4 (pmol/L)",
     ylab = "-log TSH (mIU/L)")

#run the quantile regression model for TSH/FT4 in males
#x is the predictor, FT4, and y is the response, TSH

ft4_female_model <- rq(TSH_result ~ FT4_result, data = tsh_ft4_female, tau = 0.5)
summary(ft4_female_model)

#plot the model

ggplot(data = tsh_ft4_female, mapping = aes(FT4_result, TSH_result)) +
  geom_point() + geom_abline(intercept = coef(ft4_female_model)[1], 
                     slope = coef(ft4_female_model)[2])

# Working with logarithmic TSH --------------------------------------------
#create log TSH column in the df

new_tsh <- log10(tsh_ft4_female$TSH_result)
tsh_ft4_female$logTSH <- new_tsh

#troubleshooting na, nan, inf values error in rq
str(tsh_ft4_female)
is.na(tsh_ft4_female)
tsh_ft4_female$logTSH <- as.numeric(as.character(tsh_ft4_female$logTSH))
inf_values <- is.infinite(tsh_ft4_female$logTSH)
summary(inf_values)

#remove inf values
tsh_ft4_female$logTSH <- ifelse(is.finite(tsh_ft4_female$logTSH), 
                                tsh_ft4_female$logTSH, NA)

tsh_ft4_female_filt <- tsh_ft4_female[!grepl("NA", tsh_ft4_female$logTSH),]

#run the model
ft4_female_model <- rq(FT4_result ~ logTSH, data = tsh_ft4_female, tau = 0.5)
summary(ft4_female_model)

#plot the model
ggplot(data = tsh_ft4_female, mapping = aes(logTSH, FT4_result)) +
  geom_point() + geom_abline(intercept = coef(ft4_female_model)[1], 
                             slope = coef(ft4_female_model)[2])
