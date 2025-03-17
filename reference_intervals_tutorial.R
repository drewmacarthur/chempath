#Tutorial for reference interval determination with Reflim and RefineR
#Author: Drew MacArthur
#Date: 19 Feb 2025
#Updated: 17 March 2025
#Example dataset used: TAD CHEM HbA1c 

# Importing ---------------------------------------------------------------

#install the reflimR and refineR with install.packages function
install.packages("reflimR")
install.packages("refineR")

#import the libraries/packages to work with in our script
library(refineR)
library(reflimR)

#check your working directory (source folder)
getwd()

#change the working directory if necessary to your own file path
setwd("C:\Users\drewm\Documents\Coding")
#or go to Session -> Set Working Directory


#import your data set as a new variable called hba1c_data
hba1c_data <- read.csv(file = "TAD_HBA1C_DATA.csv", 
                       header = TRUE, sep = ",", dec = ".")

#view the first few data points to check
print(head(hba1c_data))


# Data clean-up -----------------------------------------------------------

#clean up the data set before analysing:

#remove "NULL" data points from hba1c levels, by creating new variable

hba1c_filtered <- hba1c_data[!grepl("NULL", hba1c_data$PERCENTAGE_HBA1C_C045),]


#extract column of interest (hba1c levels)
hba1c_levels <- hba1c_filtered[, "PERCENTAGE_HBA1C_C045"]


#make the data numeric
hba1c_levels_ <- as.numeric(hba1c_levels)


# Running the models ------------------------------------------------------

#run refineR estimation algorithm

#create the fit
fit_hba1c_refiner <- findRI(Data = hba1c_levels_)

#print the results
print(fit_hba1c_refiner)

#compute reference intervals from the model
getRI(fit_hba1c_refiner)

#plot the fit

plot(x = fit_hba1c_refiner, showPathol = TRUE, xlab = "HbA1c (%)",
     ylab = "Frequency", 
     title = "Estimated RI for TAD HbA1c 2016 - 2020")


#run the reflim model

reflim(hba1c_levels_, main = "Estimated RI for TAD HbA1c 2016 - 2020", 
       xlab = "HbA1c (%)")

# Additional information --------------------------------------------------

#some other options for the data

#A: separate the data into male and female

hba1c_females <- hba1c_filtered[grepl("F", hba1c_filtered$GENDER),]
hba1c_males <- hba1c_filtered[grepl("M", hba1c_filtered$GENDER),]
#these can then be used to create different models

#B: separate the data by age using age or birth date columns:

#convert date of birth column to date-time format, if needed:

#check column class types
str(hba1c_filtered)

#convert to date time if needed
hba1c_filtered[["DATE_OF_BIRTH"]] <- as.POSIXct(hba1c_filtered[["DATE_OF_BIRTH"]],
                                              format = "%d/%m/%Y")

#create different variables for different age range
#find adults over 60
hba1c_over_60 <- hba1c_filtered[hba1c_filtered$DATE_OF_BIRTH < 1964-02-19,]

#find adults under 60 but over 18
hba1c_under_60 <- hba1c_filtered[hba1c_filtered$DATE_OF_BIRTH > 1964-02-19 &
                                   hba1c_filtered$DATE_OF_BIRTH < 2007-02-19,]


# Run the model for >60 years ---------------------------------------------

#you will still need to extract only the hba1c levels again after filtering:

#extract column of interest (hba1c levels)
hba1c_levels_over60 <- hba1c_over_60[, "PERCENTAGE_HBA1C_C045"]


#make the data numeric
hba1c_levels_over60 <- as.numeric(hba1c_levels_over60)


#run refineR estimation algorithm

#create the fit
fit_over60 <- findRI(Data = hba1c_levels_over60)

#print the results
print(fit_over60)

#compute reference intervals from the model
getRI(fit_over60)

#plot the fit

plot(x = fit_over60, showPathol = TRUE, xlab = "HbA1c (%)",
     ylab = "Frequency", 
     title = "Estimated RI for TAD HbA1c 2016 - 2020 (>60 years of age)")


