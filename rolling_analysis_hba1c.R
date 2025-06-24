#Rolling analysis on laboratory data
#Example using TAD HbA1c data (2016 - 2020)
#Author: Drew MacArthur
#Date: 24 June 2025


# Data importing ----------------------------------------------------------



data <- read.csv("TAD_HBA1C_DATA.csv") #import file

#create a new date column in date format from dates (check format in dataset first)

data$DATE <- as.Date(data$DATE_TESTED, 
                     tryFormats = "%Y-%m-%d")

data$DATE[is.na(data$DATE)] <- as.Date(data$DATE_TESTED[is.na(data$DATE)],
                                       format = "%Y/%m/%d") #data had two different formats
#check if all in date format
summary(is.na(data$DATE))

#**also check if data column is numeric if necessary
summary(is.numeric(data$PERCENTAGE_HBA1C_C045))




# Rolling analysis --------------------------------------------------------


library(zoo)
library(xts)
library(dplyr)
library(lubridate)

#convert to data to xts format, ordered by DATE column we created

data_xts <- xts(data$PERCENTAGE_HBA1C_C045, order.by = data$DATE)

#rolling analysis using 1000 samples:

#get rolling averages using rollmean

averages <- rollmean(data_xts, 1000, align = "right", fill = NA)

#get rolling sd and quantiles using rollapply

SD <- rollapply(data_xts, 1000, (sd), align = "right", fill = NA)

quantiles <- rollapply(data_xts, 1000, (quantile), align = "right", fill = NA)

#populate this data into a data frame

df <- fortify.zoo(cbind.xts(averages, SD, quantiles))

#create year column so we can separate data for each year

df$year <- year(data$DATE) #could be useful to do months as well

df <- na.omit(df) #remove NA row values




# Plot the data -----------------------------------------------------------



#plot mean +- SD for all the years

plot(x = df$Index,
     y = df$averages,
     type = "l",
     lty = 1,
     lwd = 2,
     col = "slateblue",
     main = "Rolling mean +- SD for HbA1c (2016 - 2020)",
     ylab = "HbA1c (%)",
     xlab = "Time")
lines(df$averages - df$SD, col = "purple4", lwd = 2, lty = 1)
lines(df$averages + df$SD, col = "purple4", lwd = 2, lty = 1)


#plot median and quartiles for all years

plot(x = df$Index,
     y = df$quantiles.2,
     type = "l",
     lty = 1,
     lwd = 2,
     col = "steelblue",
     main = "Rolling median, Q1 and Q3 for HbA1c (2016 - 2020)",
     ylab = "HbA1c (%)",
     xlab = "Time")
lines(df$quantiles.1, col = "blue4", lwd = 2, lty = 1)
lines(df$quantiles.3, col = "blue4", lwd = 2, lty = 1)


#plot for only one specific year

plot(x = df$Index[df$year == 2016],
     y = df$quantiles.2[df$year == 2016],
     type = "l",
     lty = 1,
     lwd = 2,
     col = "tomato",
     main = "Rolling median, Q1 and Q3 for HbA1c (2016)",
     ylab = "HbA1c (%)",
     xlab = "Time")
lines(df$quantiles.1[df$year == 2016], col = "tomato4", lwd = 2, lty = 1)
lines(df$quantiles.3[df$year == 2016], col = "tomato4", lwd = 2, lty = 1)




# Export rolling data -----------------------------------------------------


#create mean +- SD columns 

df$mean_minus_SD <- df$averages - df$SD
df$mean_plus_SD <- df$averages + df$SD

#export the entire dataset

write.csv(all_data, file = "C:/Users/drewm/Documents/Coding/HBA1C_rolling_data_all.csv",
          row.names = TRUE)


#export each year separately

years <- unique(df$year)


for (i in years){
  dataset <- df[df$year == i,]
  
  write.csv(dataset, file = sprintf("HBA1C_rolling_%s.csv", i),
            row.names = TRUE)
}


