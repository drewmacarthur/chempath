#Example of seasonal variation analysis
#Author: Drew MacArthur
#Date: 19/05/2025
#Data used: Thyroid function dataset

# Import and clean-up data ------------------------------------------------

#import the csv data file
KZN_data <- read.csv("TFT_FINAL_PROCESSED.csv")

#separate rows based on "TestName" column
KZN_TSH <- KZN_data[grepl("TSH", KZN_data$TestName),]
KZN_FT4 <- KZN_data[grepl("FT4", KZN_data$TestName),]

#create "CollectionDate" column from "DateTimeReceived"
KZN_TSH$CollectionDate <- as.Date(KZN_TSH$DateTimeReceived, format = "%Y-%m-%d")
KZN_FT4$CollectionDate <- as.Date(KZN_FT4$DateTimeReceived, format = "%Y-%m-%d")

#Create columns with numbered week and month based on CollectionDate

library(lubridate)
library(magrittr)
library(dplyr)


KZN_TSH <- KZN_TSH %>% 
  mutate(WEEK = week(CollectionDate))

KZN_TSH <- KZN_TSH %>% 
  mutate(MONTH = month(CollectionDate))

#repeat for FT4 dataset
KZN_FT4 <- KZN_FT4 %>% 
  mutate(WEEK = week(CollectionDate))

KZN_FT4 <- KZN_FT4 %>% 
  mutate(MONTH = month(CollectionDate))


# Seasonal variation analysis ---------------------------------------------

#create list of months and weeks

week_list <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
               21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
               41,42,43,44,45,46,47,48,49,50,51,52)

month_list <- c(1,2,3,4,5,6,7,8,9,10,11,12)

#get weekly averages and populate them into a list

#create empty list for the averages
weekly_tsh_kzn <- list()

#create a for loop to iterate every week in our week_list
for (i in week_list){
  week <- KZN_TSH[KZN_TSH$WEEK == i,]
  mean_x <- mean(week$TSH, na.rm =TRUE)
  weekly_tsh_kzn <- append(weekly_tsh_kzn, mean_x)
}

#create a dataframe to store our averages
weekly_averages <- data.frame(row.names = week_list)

#add our list of averages as a column to the df
weekly_averages$TSH <- weekly_tsh_kzn


#repeat for FT4 and add to the df

weekly_ft4_kzn <- list()

for (i in week_list){
  week <- KZN_FT4[KZN_FT4$WEEK == i,]
  mean_x <- mean(week$FT4, na.rm =TRUE)
  weekly_ft4_kzn <- append(weekly_ft4_kzn, mean_x)
}

weekly_averages$FT4 <- weekly_ft4_kzn

#export the df as a csv to store the data

#flatten the df to matrix form (list columns can't be exported)
weekly_averages_flattened <- apply(weekly_averages,2,as.character)
#export to file path
write.csv(weekly_averages_flattened, file = "C:\\Users\\drewm\\Documents\\Coding\\Thyroid function data\\KZN_weekly_averages.csv")

#-----------------
#repeat the same for monthly averages

monthly_tsh_kzn <- list()

for (i in month_list){
  month <- KZN_TSH[KZN_TSH$MONTH == i,]
  mean_x <- mean(month$TSH, na.rm =TRUE)
  monthly_tsh_kzn <- append(monthly_tsh_kzn, mean_x)
}

monthly_averages <- data.frame(row.names = month_list)

monthly_averages$TSH <- monthly_tsh_kzn

#---------------
#example to get male vs female averages

#tsh male

weekly_tsh_male <- list()

TSH_male <- KZN_TSH[KZN_TSH$Gender == "M",]

for (i in week_list){
  week <- TSH_male[TSH_male$WEEK == i,]
  mean_x <- mean(week$TSH, na.rm =TRUE)
  weekly_tsh_male <- append(weekly_tsh_male, mean_x)
}

#tsh female

weekly_tsh_female <- list()

TSH_female <- KZN_TSH[KZN_TSH$Gender == "F",]

for (i in week_list){
  week <- TSH_female[TSH_female$WEEK == i,]
  mean_x <- mean(week$TSH, na.rm =TRUE)
  weekly_tsh_female <- append(weekly_tsh_female, mean_x)
}



