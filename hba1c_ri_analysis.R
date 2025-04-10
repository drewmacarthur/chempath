# Importing ---------------------------------------------------------------

library(refineR)
library(reflimR)

hba1c_data <- read.csv(file = "TAD_HBA1C_DATA.csv", 
                       header = TRUE, sep = ",", dec = ".")

# Data clean-up and characteristics -----------------------------------------------------------

#remove NULL results
hba1c_filtered <- hba1c_data[!grepl("NULL", hba1c_data$PERCENTAGE_HBA1C_C045),]

str(hba1c_filtered$PERCENTAGE_HBA1C_C045)

#make the data numeric
hba1c_filtered$HBA1C <- as.numeric(hba1c_filtered$PERCENTAGE_HBA1C_C045)

#remove unknown age and gender
hba1c <- hba1c_filtered[!grepl("Unknown", hba1c_filtered$AGE_DISPLAY),]
hba1c <- hba1c[!grepl("U", hba1c$GENDER),]

#plot histogram of data first
hist(hba1c$HBA1C,
     xlab = "HbA1c (%)",
     col = "steelblue",
     xlim = c(0,20),
     ylim = c(0,50000),
     breaks = 50,
     main = "TAD CHEM HbA1c 2016 - 2020")

summary(hba1c$HBA1C, na.rm = TRUE)
sd(hba1c$HBA1C, na.rm = TRUE)

#create new column with numeric ages
library(lubridate)
library(maggritr)
library(dplyr)

hba1c <- hba1c %>% 
  mutate(AGE = time_length(period(AGE_DISPLAY), unit = "years"))

#get characteristics
summary(hba1c$AGE, na.rm = TRUE)
sd(hba1c$AGE, na.rm = TRUE)
min(hba1c$AGE)

hist(hba1c$AGE,
     xlab = "Age (years)",
     col = "mediumpurple",
     xlim = c(0,100),
     ylim = c(0,25000),
     breaks = 30,
     main = "Age for TAD CHEM HbA1c 2016 - 2020")

#get hba1c status info
summary(hba1c$HBA1C <= 6)
summary(hba1c$HBA1C > 8)

#get gender info

hba1c_male <- hba1c[grepl("M", hba1c$GENDER),]
hba1c_female <- hba1c[grepl("F", hba1c$GENDER),]
#male age
summary(hba1c_male$AGE, na.rm = TRUE)
sd(hba1c_male$AGE, na.rm = TRUE)
#male hba1c
summary(hba1c_male$HBA1C, na.rm =TRUE)
sd(hba1c_male$HBA1C, na.rm = TRUE)
#hba1c status male
summary(hba1c_male$HBA1C <= 6)
summary(hba1c_male$HBA1C > 8)
#female age
summary(hba1c_female$AGE, na.rm = TRUE)
sd(hba1c_female$AGE, na.rm = TRUE)
#female hba1c
summary(hba1c_female$HBA1C, na.rm =TRUE)
sd(hba1c_female$HBA1C, na.rm = TRUE)

#separate by age
hba1c_old <- hba1c[hba1c$AGE > 60,]
hba1c_young <- hba1c[hba1c$AGE <= 60 & hba1c$AGE >= 18,]

#young age and hba1c
summary(hba1c_young$AGE, na.rm = TRUE)
sd(hba1c_young$AGE, na.rm = TRUE)

summary(hba1c_young$HBA1C, na.rm =TRUE)
sd(hba1c_young$HBA1C, na.rm = TRUE)

#old age and hba1c
summary(hba1c_old$AGE, na.rm = TRUE)
sd(hba1c_old$AGE, na.rm = TRUE)

summary(hba1c_old$HBA1C, na.rm =TRUE)
sd(hba1c_old$HBA1C, na.rm = TRUE)

#hba1c status for young and old
summary(hba1c_young$HBA1C <= 6)
summary(hba1c_young$HBA1C > 8)

summary(hba1c_old$HBA1C <= 6)
summary(hba1c_old$HBA1C > 8)

# REFINER ------------------------------------------------------

fit_all <- findRI(Data = hba1c$HBA1C)

print(fit_all)

getRI(fit_all)

plot(x = fit_all, showPathol = TRUE,
     xlab = "HbA1c (%)",
     ylab = "Frequency", 
     title = "Estimated RI for all HbA1c data")

#male vs female
fit_male <- findRI(Data = hba1c_male$HBA1C)

print(fit_male)

getRI(fit_male)

plot(x = fit_male, showPathol = TRUE,
     xlab = "HbA1c (%)",
     ylab = "Frequency", 
     title = "Estimated RI for HbA1c in males")

fit_female <- findRI(Data = hba1c_female$HBA1C)

print(fit_female)

getRI(fit_female)

plot(x = fit_female, showPathol = TRUE,
     xlab = "HbA1c (%)",
     ylab = "Frequency", 
     title = "Estimated RI for HbA1c in females")

#young vs old
fit_young <- findRI(Data = hba1c_young$HBA1C)

print(fit_young)

getRI(fit_young)

plot(x = fit_young, showPathol = TRUE,
     xlab = "HbA1c (%)",
     ylab = "Frequency", 
     title = "Estimated RI for HbA1c in adults 18 - 60 years")

fit_old <- findRI(Data = hba1c_old$HBA1C)

print(fit_old)

getRI(fit_old)

plot(x = fit_old, showPathol = TRUE,
     xlab = "HbA1c (%)",
     ylab = "Frequency", 
     title = "Estimated RI for HbA1c in adults > 60 years")

#redo with two parameter box cox transformation to see if it changes

fit_all <- findRI(Data = hba1c$HBA1C, model = "modBoxCox")

print(fit_all)

getRI(fit_all)

plot(x = fit_all, showPathol = TRUE, color = "steelblue",
     xlab = "HbA1c (%)",
     ylab = "Frequency", 
     title = "Estimated RI for all HbA1c data")
#no change

# REFLIM ------------------------------------------------------------------

#all data
reflim(hba1c$HBA1C, main = "Estimated RI for all HbA1c data", 
       xlab = "HbA1c (%)")

#male vs female
reflim(hba1c_male$HBA1C, main = "Estimated RI for HbA1c in males", 
       xlab = "HbA1c (%)")
reflim(hba1c_female$HBA1C, main = "Estimated RI for HbA1c in females", 
       xlab = "HbA1c (%)")

#young vs old
reflim(hba1c_young$HBA1C, main = "Estimated RI for HbA1c in adults 18 - 60 years", 
       xlab = "HbA1c (%)")
reflim(hba1c_old$HBA1C, main = "Estimated RI for HbA1c in adults > 60 years", 
       xlab = "HbA1c (%)")

# Additional information --------------------------------------------------

#compare means between genders and ages

t.test(log(hba1c_female$HBA1C[hba1c_female$HBA1C > 1]),
           log(hba1c_male$HBA1C[hba1c_male$HBA1C > 1]),
           var.equal = FALSE)

t.test(hba1c_young$HBA1C[hba1c_young$HBA1C > 1], 
       hba1c_old$HBA1C[hba1c_old$HBA1C > 1],
       var.equal = FALSE)
