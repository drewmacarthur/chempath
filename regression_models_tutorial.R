#Basic regression model analysis in R
#Including linear, non-linear and quantile regression models
#Author: Drew MacArthur
#Date: 30 May 2025

#Examples for a theoretical data frame "df"

# Linear regression -------------------------------------------------------

linear_model <- lm(df$column2 ~ df$column1, data = df) #always use y ~ x formula format

summary(linear_model) #get the model information

plot(linear_model,
     main = "Linear regression",
     xlab = "X-axis",
     ylab = "Y-axis") #plot the regression line



# Quantile regression -----------------------------------------------------

#Quantile regression estimates quantiles of y based on x, instead of mean

install.packages(quantreg) #install quantreg for this analysis

library(quantreg) #import the package 

quantile_model <- rq(df$column2 ~ df$column1, data = df, tau = 0.5) #create a model
#rq is the quantile regression function
#tau is the quantile, e.g. 0.5 estimates the median of the data

summary(quantile_model)

plot(quantile_model,
     main = "Quantile regression",
     xlab = "X-axis",
     ylab = "Y-axis") #plot the regression line



# Multiple regression -----------------------------------------------------

#create a regression model with more than one variable of x

multiple_model <- ln(df$column3 ~ df$column1 * df$column2, data = df)

#create your formula using y ~ x * x, or any other operation, like x + x

summary(multiple_model)

plot(multiple_model,
     main = "Multiple regression",
     xlab = "X-axis",
     ylab = "Y-axis") #plot the regression line


# Generalised linear models ---------------------------------------------

#glm creates generalised linear models, based on an error function (family)

#logistic regression
logistic_model <- glm(df$column2 ~ df$column1, data = df, family = binomial)

#poisson regression
poisson_model <- glm(df$column2 ~ df$column1, data = df, family = poisson)

#gaussian regression
gaussian_model <- glm(df$column2 ~ df$column1, data = df, family = gaussian)




# Compare model performance -----------------------------------------------


#you can compare two or more models on their performance, based on the same data set

install.packages(performance) #install the package needed

library(performance) #import the package

#assess performance of a single model

performance(linear_model)

#compare the two models, linear_model and quantile_model, to see which performs better

compare_performance(linear_model, quantile_model)

#it generates AIC and BIC scores. Models with lower AIC score perform better. 

model_performance.lm(linear_model) #used for lm regression models


#check package documentation for more use cases. 






