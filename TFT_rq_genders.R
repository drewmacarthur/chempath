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

#combine male and female datasets (for comparison purposes later)
tsh_ft4 <- rbind(tsh_ft4_male, tsh_ft4_female)
tsh_ft3 <- rbind(tsh_ft3_male, tsh_ft3_female)


#plot original data first
plot(tsh_ft4_female$TSH_result, tsh_ft4_female$FT4_result, 
     main = "FT4 plotted against TSH for females in TAD", 
     xlab = "TSH (mIU/L)",
     ylab = "FT4 (pmol/L)")
plot(tsh_ft4_male$TSH_result, tsh_ft4_male$FT4_result, 
     main = "FT4 plotted against TSH for males in TAD", 
     xlab = "TSH (mIU/L)",
     ylab = "FT4 (pmol/L)")

#plot FT4 against -log TSH 
plot(-log10(tsh_ft4_female$TSH_result),tsh_ft4_female$FT4_result, 
     main = "FT4 plotted against logarithmic TSH for females in TAD", 
     xlab = "-logTSH (mIU/L)",
     ylab = "FT4 (pmol/L)")
plot(-log10(tsh_ft4_male$TSH_result), tsh_ft4_male$FT4_result, 
     main = "FT4 plotted against logarithmic TSH for males in TAD", 
     xlab = "-logTSH (mIU/L)",
     ylab = "FT4 (pmol/L)")


#repeat for ft3
plot(tsh_ft3_female$TSH_result, tsh_ft3_female$FT3_result, 
     main = "FT3 plotted against TSH for females in TAD", 
     xlab = "TSH (mIU/L)",
     ylab = "FT3 (pmol/L)")
plot(tsh_ft3_male$TSH_result, tsh_ft3_male$FT3_result, 
     main = "FT3 plotted against TSH for males in TAD", 
     xlab = "TSH (mIU/L)",
     ylab = "FT3 (pmol/L)")

#plot FT4 against log TSH 
plot(log10(tsh_ft3_female$TSH_result),tsh_ft3_female$FT3_result, 
     main = "FT3 plotted against logarithmic TSH for females in TAD", 
     xlab = "logTSH (mIU/L)",
     ylab = "FT3 (pmol/L)")
plot(log10(tsh_ft3_male$TSH_result), tsh_ft3_male$FT3_result, 
     main = "FT3 plotted against logarithmic TSH for males in TAD", 
     xlab = "logTSH (mIU/L)",
     ylab = "FT3 (pmol/L)")

#create log data if necessary
new_ft4_f <- log10(tsh_ft4_female$TSH_result)
tsh_ft4_female$logTSH <- new_ft4_f

new_ft4_m <- log10(tsh_ft4_male$TSH_result)
tsh_ft4_male$logTSH <- new_ft4_m

new_ft3_f <- log10(tsh_ft3_female$TSH_result)
tsh_ft3_female$logTSH <- new_ft3_f

new_ft3_m <- log10(tsh_ft3_male$TSH_result)
tsh_ft3_male$logTSH <- new_ft3_m

#replace inf values with NA
tsh_ft4_female$logTSH <- ifelse(is.finite(tsh_ft4_female$logTSH), 
                             tsh_ft4_female$logTSH, NA)
ft4_female <- tsh_ft4_female[!grepl("NA", tsh_ft4_female$logTSH),]

tsh_ft4_male$logTSH <- ifelse(is.finite(tsh_ft4_male$logTSH), 
                             tsh_ft4_male$logTSH, NA)
ft4_male <- tsh_ft4_male[!grepl("NA", tsh_ft4_male$logTSH),]

#repeat for ft3
tsh_ft3_female$logTSH <- ifelse(is.finite(tsh_ft3_female$logTSH), 
                                tsh_ft3_female$logTSH, NA)
ft3_female <- tsh_ft3_female[!grepl("NA", tsh_ft3_female$logTSH),]

tsh_ft3_male$logTSH <- ifelse(is.finite(tsh_ft3_male$logTSH), 
                              tsh_ft3_male$logTSH, NA)
ft3_male <- tsh_ft3_male[!grepl("NA", tsh_ft3_male$logTSH),]

# Regression analysis -----------------------------------------------------

#perform quantile regression and plot lines
plot(ft4_female$logTSH, ft4_female$FT4_result,
     main = "logTSH vs FT4 female",
     xlab = "logTSH (mIU/L)",
     ylab = "FT4 (pmol/L)")
abline(lm(FT4_result~logTSH, data = ft4_female), col = "red")
text(x=1, y=60, "Simple regression", col='red')
abline(rq(FT4_result~logTSH, data = ft4_female, tau = 0.5), col = "blue" )
text(x=1, y=50, "Quantile regression", col='blue')

plot(ft4_male$logTSH, ft4_male$FT4_result,
     main = "logTSH vs FT4 male",
     xlab = "logTSH (mIU/L)",
     ylab = "FT4 (pmol/L)")
abline(lm(FT4_result~logTSH, data = ft4_male), col = "red")
text(x=1, y=35, "Simple regression", col='red')
abline(rq(FT4_result~logTSH, data = ft4_male, tau = 0.5), col = "blue" )
text(x=1, y=30, "Quantile regression", col='blue')

#plot both male and female data on one graph
male_female <- ggplot() +
  geom_point(data = ft4_female, aes(x= logTSH, y= FT4_result,color = "Female"), show.legend = TRUE) +
  geom_point(data = ft4_male, aes(x = logTSH, y = FT4_result,color = "Male"), show.legend = TRUE) +
  labs(x = "log TSH (mIU/L)", y = "FT4 (pmol/L)", title = "logTSH vs FT4") +
  scale_color_manual(name="Gender",
                     values=c("Female" = "mediumpurple4", "Male" = "skyblue"))

male_female

#create rq models to input to graph
rq_female <- rq(FT4_result~logTSH, data = ft4_female, tau = 0.5)
rq_male <- rq(FT4_result~logTSH, data = ft4_male, tau = 0.5)

#ft3 models
rq_ft3_f <- rq(FT3_result~logTSH, data = ft3_female, tau = 0.5)
rq_ft3_m <- rq(FT3_result~logTSH, data = ft3_male, tau = 0.5)

#plot the data with rq lines
ft4_model_plot <- ggplot() +
  geom_point(data = ft4_female, aes(x= logTSH, y= FT4_result,color = "Female"), show.legend = TRUE) +
  geom_point(data = ft4_male, aes(x = logTSH, y = FT4_result,color = "Male"), show.legend = TRUE) +
  labs(x = "log TSH (mIU/L)", y = "FT4 (pmol/L)", title = "logTSH vs FT4") +
  scale_color_manual(name="Gender",
                     values=c("Female" = "mediumpurple4", "Male" = "skyblue")) +
  geom_abline(intercept = coef(rq_female)[1], slope = coef(rq_female)[2],lty=5, color = "black") +
  geom_abline(intercept = coef(rq_male)[1], slope = coef(rq_male)[2],lty=2, color = "black")

ft4_model_plot

#compare female and male models for ft4
summary(rq_female)
summary(rq_male)
compare_performance(rq_female, rq_male)

#perform t-test on two regression models

slope_A <- #input regression slope
slope_B <- #input regression slope
se_A <- #input slope standard error
se_B <- #input slope standard error
nA <- #n samples for A
nB <- #n samples for B

t_stat <- (slope_A - slope_B) / sqrt(se_A^2 + se_B^2)
df <- ((se_A^2 + se_B^2)^2) / (((se_A^2)^2 / (nA - 2)) + ((se_B^2)^2 / (nB - 2)))
p_value <- 2 * pt(-abs(t_stat), df)
t_stat
p_value


#compare lines with model for all of the data

ft4_model_plot <- ggplot() +
  geom_point(data = ft4_female, aes(x= logTSH, y= FT4_result,color = "Female"), show.legend = TRUE) +
  geom_point(data = ft4_male, aes(x = logTSH, y = FT4_result,color = "Male"), show.legend = TRUE) +
  labs(x = "log TSH (mIU/L)", y = "FT4 (pmol/L)", title = "logTSH vs FT4") +
  geom_abline(intercept = 12.77737, slope = -1.50695, lty=5, color="black") +
  scale_color_manual(name="Gender",
                     values=c("Female" = "mediumpurple4", "Male" = "skyblue")) +
  geom_abline(intercept = coef(rq_female)[1], slope = coef(rq_female)[2],lty=1, color = "mediumpurple3") +
  geom_abline(intercept = coef(rq_male)[1], slope = coef(rq_male)[2],lty=1, color = "skyblue3") +
  ylim(0,30)

ft4_model_plot

# FT3 analysis ------------------------------------------------------------

#perform quantile regression and plot lines

plot(ft3_female$logTSH, ft3_female$FT3_result,
     main = "logTSH vs FT3 female",
     xlab = "logTSH (mIU/L)",
     ylab = "FT3 (pmol/L)")
abline(lm(FT3_result~logTSH, data = ft3_female), col = "red")
text(x=1, y=35, "Simple regression", col='red')
abline(rq(FT3_result~logTSH, data = ft3_female, tau = 0.5), col = "blue" )
text(x=1, y=30, "Quantile regression", col='blue')

plot(ft3_male$logTSH, ft3_male$FT3_result,
     main = "logTSH vs FT3 male",
     xlab = "logTSH (mIU/L)",
     ylab = "FT3 (pmol/L)")
abline(lm(FT3_result~logTSH, data = ft3_male), col = "red")
text(x=1, y=12, "Simple regression", col='red')
abline(rq(FT3_result~logTSH, data = ft3_male, tau = 0.5), col = "blue" )
text(x=1, y=10, "Quantile regression", col='blue')

#create rq models to input to graph
rq_ft3_f <- rq(FT3_result~logTSH, data = ft3_female, tau = 0.5)
rq_ft3_m <- rq(FT3_result~logTSH, data = ft3_male, tau = 0.5)

#plot the data with rq lines
ft3_model_plot <- ggplot() +
  geom_point(data = ft3_female, aes(x= logTSH, y= FT3_result,color = "Female"), show.legend = TRUE) +
  geom_point(data = ft3_male, aes(x = logTSH, y = FT3_result,color = "Male"), show.legend = TRUE) +
  labs(x = "log TSH (mIU/L)", y = "FT3 (pmol/L)", title = "logTSH vs FT3") +
  scale_color_manual(name="Gender",
                     values=c("Female" = "mediumpurple4", "Male" = "skyblue")) +
  geom_abline(intercept = coef(rq_ft3_f)[1], slope = coef(rq_ft3_f)[2],lty=1, color = "mediumpurple2") +
  geom_abline(intercept = coef(rq_ft3_m)[1], slope = coef(rq_ft3_m)[2],lty=1, color = "skyblue3") +
  geom_abline(intercept = 4.36083, slope = -0.28662, lty=5, color="black") +
  ylim(0,15)

ft3_model_plot

#compare female and male models for ft3
summary(rq_ft3_f)
summary(rq_ft3_m)

#compare using t-test
slope_A <- #input regression slope
slope_B <- #input regression slope
se_A <- #input slope standard error
se_B <- #input slope standard error
nA <- #n samples for A
nB <- #n samples for B
  
t_stat <- (slope_A - slope_B) / sqrt(se_A^2 + se_B^2)
df <- ((se_A^2 + se_B^2)^2) / (((se_A^2)^2 / (nA - 2)) + ((se_B^2)^2 / (nB - 2)))
p_value <- 2 * pt(-abs(t_stat), df)
t_stat
p_value


# Partitioning ------------------------------------------------------------

#partition based on TSH levels
hypo_female <- ft4_female[ft4_female$TSH_result > 4.78,]
eu_female <- ft4_female[ft4_female$TSH_result <= 4.78 & ft4_female$TSH_result >= 0.55,]
hyper_female <- ft4_female[ft4_female$TSH_result < 0.55,]

hypo_male <- ft4_male[ft4_male$TSH_result > 4.78,]
eu_male <- ft4_male[ft4_male$TSH_result <= 4.78 & ft4_male$TSH_result >= 0.55,]
hyper_male <- ft4_male[ft4_male$TSH_result < 0.55,]

#final model partitioning for report
abnormal_female <- ft4_female[ft4_female$TSH_result < 0.55 | ft4_female$TSH_result > 4.78,]
normal_female <- ft4_female[ft4_female$TSH_result <= 4.78 & ft4_female$TSH_result >= 0.55,]

low_tsh_male <- ft4_male[ft4_male$TSH_result <= 4.78,]
high_tsh_male <- ft4_male[ft4_male$TSH_result > 4.78,]

#create the models
abnormal_f_model <- rq(FT4_result~logTSH, data = abnormal_female, tau=0.5)
normal_f_model <- rq(FT4_result~logTSH, data = normal_female, tau=0.5)

low_tsh_male_model <- rq(FT4_result~logTSH, data = low_tsh_male, tau=0.5)
high_tsh_male_model <- rq(FT4_result~logTSH, data = high_tsh_male, tau=0.5)

all_normal <- rbind(ft4_male, normal_female)
summary(rq(FT4_result~logTSH, data=all_normal, tau=0.5))

summary(abnormal_f_model)
summary(normal_f_model)

summary(low_tsh_male_model)
summary(high_tsh_male_model)

male_model <- rq(FT4_result~logTSH, data = ft4_male)
summary(male_model)

female_model <- rq(FT4_result~logTSH, data = ft4_female, tau=0.5)

library(performance)
compare_performance(female_model, abnormal_f_model, normal_f_model)

#perform t test to compare female normal and abnormal models
#compare slopes
slope_N <- -0.76
slope_A <- -1.97
se_N <- 0.04043
se_A <- 0.08466

t_stat <- (slope_N - slope_A) / sqrt(se_N^2 + se_A^2)
df <- ((se_N^2 + se_A^2)^2) / (((se_N^2)^2 / (4497 - 2)) + ((se_A^2)^2 / (1063 - 2)))
p_value <- 2 * pt(-abs(t_stat), df)
t_stat
p_value

#compare intercepts
slope_N <- -0.74
slope_A <- -0.76
se_N <- 0.13452
se_A <- 0.14639

t_stat <- (slope_N - slope_A) / sqrt(se_N^2 + se_A^2)
df <- ((se_N^2 + se_A^2)^2) / (((se_N^2)^2 / (1735 - 2)) + ((se_A^2)^2 / (4497 - 2)))
p_value <- 2 * pt(-abs(t_stat), df)
t_stat
p_value

#plot the optimum models on one graph
final_model_plot <- ggplot() +
  labs(x = "log TSH (mIU/L)", y = "FT4 (pmol/L)") +
  geom_abline(intercept = 12.63, slope = -0.75,lty=1, color = "black") +
  geom_abline(intercept = 12.9, slope = -1.97, lty=5, color="black") +
  ylim(5,20)
final_model_plot

#create plot for each gender with 3 different regression lines
#male

plot(ft4_male$logTSH, ft4_male$FT4_result,
     main = "Male population",
     xlab = "logTSH (mIU/L)",
     ylab = "FT4 (pmol/L)")
abline(rq(FT4_result~logTSH, data = hypo_male), lty = 1,lwd=2, col = "mediumpurple4")
text(x=1.0, y=35, "TSH > 4.78", col='mediumpurple4')
text(x=1.0, y=30, "[FT4] = -4.47log[TSH] + 15.35", col='mediumpurple4')
abline(rq(FT4_result~logTSH, data = eu_male, tau = 0.5), lty=1,lwd=2, col = "steelblue" )
text(x=-0.2, y=35, "0.55 < TSH < 4.78", col='steelblue')
text(x=-0.2, y=30, "[FT4] = -0.37log[TSH] + 12.65", col='steelblue')
abline(rq(FT4_result~logTSH, data = hyper_male, tau = 0.5), lty=1,lwd=2, col = "seagreen")
text(x=-1.5, y=35, "TSH < 0.55", col = "seagreen")
text(x=-1.5, y=30, "[FT4] = -0.70log[TSH] + 12.60", col = "seagreen")
abline(v = 0.26,lty = 5, col = "black")
abline(v = -0.68,lty = 5, col = "black")


#create models separately to get details
rq_male_hypo <- rq(FT4_result~logTSH, data = hypo_male)
rq_male_eu <- rq(FT4_result~logTSH, data = eu_male)
rq_male_hyper <- rq(FT4_result~logTSH, data = hyper_male)
rq_all <- rq(FT4_result~logTSH, data = ft4_male)

summary(rq_all)
summary(rq_male_hypo)
summary(rq_male_eu)
summary(rq_male_hyper)

#female
plot(ft4_female$logTSH, ft4_female$FT4_result,
     main = "Female population",
     xlab = "logTSH (mIU/L)",
     ylab = "FT4 (pmol/L)")
abline(rq(FT4_result~logTSH, data = hypo_female), lty = 1,lwd=2, col = "mediumpurple4")
text(x=1.0, y=55, "TSH > 4.78", col='mediumpurple4')
text(x=1.0, y=50, "[FT4] = -3.88log[TSH] + 14.90", col='mediumpurple4')
abline(rq(FT4_result~logTSH, data = eu_female, tau = 0.5), lty=1,lwd=2, col = "steelblue" )
text(x=-0.2, y=55, "0.55 < TSH < 4.78", col='steelblue')
text(x=-0.2, y=50, "[FT4] = -0.75log[TSH] + 12.62", col='steelblue')
abline(rq(FT4_result~logTSH, data = hyper_female, tau = 0.5), lty=1,lwd=2, col = "seagreen")
text(x=-1.5, y=55, "TSH < 0.55", col = "seagreen")
text(x=-1.5, y=50, "[FT4] = -3.08log[TSH] + 11.97", col = "seagreen")
abline(v = 0.26,lty = 5, col = "black")
abline(v = -0.68,lty = 5, col = "black")


#create models separately to get details
rq_female_hypo <- rq(FT4_result~logTSH, data = hypo_female)
rq_female_eu <- rq(FT4_result~logTSH, data = eu_female)
rq_female_hyper <- rq(FT4_result~logTSH, data = hyper_female)
rq_all_female <- rq(FT4_result~logTSH, data = ft4_female)
#print results
summary(rq_all_female)
summary(rq_female_hypo)
summary(rq_female_eu)
summary(rq_female_hyper)

