#Tutorial on plotting in R
#Including histograms, scatter plots and ggplot package
#Author: Drew MacArthur
#Date: 29 May 2025


# How to plot histograms --------------------------------------------------

#first check and set working directory
getwd() #check

setwd("File path") #set working directory

#import your data as data frame using read.csv()

data <- read.csv("File name.csv")

#create a basic histogram using hist()

hist(data$column, #specify the column of your data frame
     main = "Histogram title",
     col = "mediumpurple", #you can search online for a list of colours e.g. "steelblue"
     xlab = "X label", #name your x axis
     xlim = c(0,15),#set the limit of the x-axis
     breaks = 150, #no. of breaks changes no. of bars in the histogram
     ylim = c(0,2000)) #set the limit of the y-axis

#create a histogram with a density curve

hist(data$column,
     main = "Title",
     col = "mediumpurple",
     xlab = "X label",
     xlim = c(0,15),
     breaks = 150,
     prob = TRUE) #setting prob = TRUE adds a density curve
lines(x = density(x = data$column), col = "black", lwd = 2) #add your line for the density curve
#lwd is the line width and can be adjusted



# How to create scatter plots  --------------------------------------------

#let's plot using the built-in R function plot()

plot(data$column1, #choose your x-values
     data$column2, #choose your y-values
     main = "Plot title", #name your plot
     xlab = "X-axis", #name the x-axis
     ylab = "Y-axis") #name the y-axis

#you can also use variables for the x and y, like a list/vector instead of a data frame column


#you can add data transformations within the plot

plot(log(data$column1), #plot the natural logarithm of x (log () in R)
     data$column2,
     main = "Title", 
     xlab = "X-axis",
     ylab = "Y-axis")


plot(-log10(data$column1), #plot -log10 of x
     data$column2,
     main = "Title", 
     xlab = "X-axis",
     ylab = "Y-axis")


#Add a regression line and text to a scatter plot

#you can create a regression model first or put the same code into the abline() function

#e.g. create a linear regression using lm()
regression_model <- lm(data$column2 ~ data$column1, data = data)
#always write the regression formula in the format y ~ x



#create scatter plot with this regression line
plot(data$column1, 
     data$column2,
     main = "Title",
     xlab = "X-axis",
     ylab = "Y-axis")
abline(regression_model, col = "red") #abline adds a line based on a regression model
text(x=1, y=60, "Linear regression", col='red') #add text to label the line
#x and y is the text position based on the plot's axes



#you can also directly create a model inside the abline() function:

abline(lm(data$column2 ~ data$column1, data = data), col = "blue" )
text(x=1, y=50, "Linear regression", col='blue')

#you can multiple regression lines by adding another abline() underneath

#you can also change line thickness using lty

abline(regression_model, lty = 1, col = "red")

#you can add a plain line
abline(v = 5,lty = 5, col = "black") #add a vertical line at x = 5


# Create plots with ggplot ------------------------------------------------

#plot with the useful package ggplot2

#first install and import the package
install.packages(ggplot2)
library(ggplot2)



#you can create plots inside a variable to store them

plot1 <- ggplot() + #always start with ggplot() and then add your elements using +
  
  geom_point(data = data, aes(x = data$column1, y = data$column2, color = "blue")) +
  #aes is our mapping i.e. our model formula
  #color can be a colour or, a category if we plot more than one line
  #geom_point() is a scatter plot
  labs(x = "X label", y = "Y label", title = "Plot title") + #add your labels
  #add a regression line using geom_abline()
  #incorporate your previous model's values as the intercept and slope
  geom_abline(intercept = coef(regression_model)[1], slope = coef(regression_model)[2],
              lty = 5, color = "black") +
  #add a y-axis limit
  ylim(0,60)


plot1 #view the plot




#we can create categories and have a legend for the categories: 

plot2 <- ggplot() +
  geom_point(data = data, aes(x = data$column1, y = data$column2, color = "Category 1"), 
           show.legend = TRUE) + #e.g. category could be gender
  #show.legend must be true
  geom_point(data = data, aes(x = data$column1, y = data$column2,color = "Category 2"),
             show.legend = TRUE) + #add your second scatter
  
  labs(x = "X label", y = "Y label", title = "Title") +
  #now we add the information for our legend
  scale_color_manual(name ="Category name", #legend title
                     values = c("Category 1" = "mediumpurple4", "Category 2"="steelblue")) +
  #category names must be identical to those written in the geom_point color
  
plot2 #view the plot







