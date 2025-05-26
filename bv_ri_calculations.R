#Population-based reference intervals based on biological variation
#Author: Drew MacArthur
#Date: 26 May 2025

#Calculations based on Coskun et al (2024): https://doi.org/10.1093/clinchem/hvae109

# RI function for normally-distributed data ----------------------------

#normal data distribution (numeric), 
#where x = a single data column/list/vector, 
#a is within-subject biological variation (%),
#b is between-subject biological variation (%),
#and c is analytical variation (%)

RI_normal <- function(x, a, b, c){
  
  PSP = mean(x)
  n = length(x)
  CV_I = (a/100) * mean(x)
  CV_G = (b/100) * mean(x)
  CV_A = (c/100) * mean(x)
  
  CV_pop = sqrt(((n + 1)/n) * (CV_I^2 + CV_G^2 + CV_A^2))
  
  TV_PSP = 1.96 * CV_pop
  
  pop_RI_U = PSP + ((PSP * TV_PSP)/100)
  
  pop_RI_L = PSP - ((PSP * TV_PSP)/100)
  
  summary_stats <- matrix(c("Normal", n, PSP, pop_RI_L, pop_RI_U), nrow = 5, byrow = FALSE)
  
  rownames(summary_stats) <- c("Distribution", "n subjects", "PSP (mean)", "Lower reference limit", 
                               "Upper reference limit")
  colnames(summary_stats) <- c("RI summary")
  
  RI_summary <- as.table(summary_stats)
  
  return(RI_summary)
  
}


# RI function for Ln-transformed data ------------------------------------

#not normal distribution (numeric), ln-transformed
#where x is the data in a single column/list/vector,
#a is within-subject biological variation (%),
#b is between-subject biological variation (%),
#and c is analytical variation (%)

RI_transf <- function(x, a, b, c){
  
  u = mean(x)
  n = length(x)
  sigma = sd(x)
  
  CV_I = (a/100) * mean(x)
  CV_G = (b/100) * mean(x)
  CV_A = (c/100) * mean(x)
  
  SD_I = log(CV_I^2 + 1)
  SD_G = log(CV_G^2 + 1)
  SD_A = log(CV_A^2 + 1)
  
  SD = sqrt(((n + 1)/n) * (SD_I + SD_G + SD_A))
  
  PSP = exp(u + 0.5*sigma^2)
  
  pop_RI_U = exp((u + 0.5*sigma^2) + 1.96*SD)
  
  pop_RI_L = exp((u + 0.5*sigma^2) - 1.96*SD)
  
  summary_stats <- matrix(c("Ln-transformed", n, PSP, pop_RI_L, pop_RI_U), nrow = 5, byrow = FALSE)
  
  rownames(summary_stats) <- c("Distribution", "n subjects", "PSP (mean)", "Lower reference limit", 
                               "Upper reference limit")
  
  colnames(summary_stats) <- c("RI summary")
  
  RI_summary <- as.table(summary_stats)
  
  return(RI_summary)
  
}



# Using the functions on a data set ---------------------------------------


#TSH as an example

#import the dataset
TSH_data <- read.csv("C:\\Users\\drewm\\Documents\\Coding\\Thyroid function data\\TAD FINAL PROCESSED DATASETS\\TSH_FT4_ALL.csv")

#make TSH numeric
TSH_data$TSH <- as.numeric(TSH_data$TSH)

#get samples within "normal" if needed
TSH <- TSH_data[TSH_data$TSH < 4.78 & TSH_data$TSH > 0.55,]

#visualise the distribution for normality
hist(TSH$TSH,
     xlim = c(0,10),
     breaks = 50)

#OR check if distribution is normal using Kolmogorov-Smirnov test
ks.test(TSH$TSH, "pnorm") #not normal

#transform the data using natural logarithm
ln_TSH <- log(TSH$TSH)

hist(ln_TSH,
     breaks = 40)


#get random sample of 16 reference individuals from the population
#Coskun et al recommends minimum 16 samples

test <- sample(ln_TSH, 16)

#perform the calculation
#within-subject BV: 19.3% (Westgard)
#between-subject BV: 24.6% (Westgard)
#analytical variation: 3.5% (random example)

RI_transf(test, 19.3, 24.6, 3.5)



#on normally distributed data use RI_normal:

RI_normal(x, a, b, c)

#where x = a single data column/list/vector, 
#a = within-subject biological variation (%),
#b = between-subject biological variation (%),
#c = analytical variation (%)






