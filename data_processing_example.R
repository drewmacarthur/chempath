#Example on how to import data from multiple Excel documents
#Data in this example: TAD (Gauteng) dataset
#Author: Drew MacArthur
#Date: 26 May 2025


#first save each Excel sheet as csv i.e. one csv for each month

#set your working directory to the folder with the csv's 
#(Session -> Set Working Directory -> Choose Directory)

#import all the files into R:

#e.g.
jan <- read.csv("jan_tft.csv")

#Repeat for each file, then combine them using rbind() 
#e.g.
all_data <- rbind(jan,feb,march,etc) #columns must be the same, in the same order

#OR

#use the below "for loop":

months <- list("jan", "feb", "march", "april", "may", "june", "july", "aug", "sept",
               "oct", "nov", "dec") #create your list of months to iterate over
  
TFT_data <- data.frame() #create empty data frame to append each time in our loop

for (i in months){ #looping over every month in our list
  
  i <- read.csv(sprintf("%s_tft.csv", i)) #edit according to your own file names*
  TFT_data <- rbind(TFT_data, i) #adding each iteration of "i" to our data frame
  
} 
#* "sprintf" function inserts the variable "i" into the file name wherever it sees "%s".

#make sure the csv files have the same column names in the same order, otherwise you will
#get an error

#--> View your data frame to ensure it looks right. 

#---------------

#data filtering and cleaning up

#convert test columns to numeric ("<", ">" or empty values will be set to NA)

TFT_data$TSH <- as.numeric(TFT_data$TSH) #"$" indexes a column in your data frame
TFT_data$FT4 <- as.numeric(TFT_data$FT4)
TFT_data$FT3 <- as.numeric(TFT_data$FT3)

#separate by category (e.g. male and female) if you want:

TFT_male <- TFT_data[TFT_data$Sex == "M",] #[] brackets index the data based on a condition
TFT_female <- TFT_data[TFT_data$Sex == "F",]


#export the data-sets as csv (which can be analysed in Excel)

write.csv(TFT_data, file = "C:\\Users\\drewm\\Documents\\Coding\\Thyroid function data\\TFT_data.csv")

write.csv(TFT_male, file = "C:\\Users\\drewm\\Documents\\Coding\\Thyroid function data\\TFT_male.csv")

write.csv(TFT_female, file = "C:\\Users\\drewm\\Documents\\Coding\\Thyroid function data\\TFT_female.csv")

