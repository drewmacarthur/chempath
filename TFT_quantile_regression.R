#Author: Drew MacArthur, updated 16/04/2025
#Analysis of TAD TFT (thyroid function) data for TSH-FT4 and TSH-FT3 relationship

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

#combine male and female datasets

tsh_ft4 <- rbind(tsh_ft4_male, tsh_ft4_female)
  
tsh_ft3 <- rbind(tsh_ft3_male, tsh_ft3_female)

#plot the data for each
plot(tsh_ft4$FT4_result, tsh_ft4$TSH_result, 
     main = "FT4 plotted against TSH", 
     xlab = "FT4 (pmol/L)",
     ylab = "TSH (mIU/L)")
plot(tsh_ft3$FT3_result, tsh_ft3$TSH_result, 
     main = "FT3 plotted against TSH",
     xlab = "FT3 (pmol/L)",
     ylab = "TSH (mIU/L)")

#plot against log TSH
plot(tsh_ft4$FT4_result, log10(tsh_ft4$TSH_result), 
     main = "FT4 plotted against logarithmic TSH", 
     xlab = "FT4 (pmol/L)",
     ylab = "logTSH (mIU/L)")
plot(tsh_ft3$FT3_result, log10(tsh_ft3$TSH_result), 
     main = "FT3 plotted against logarithmic TSH",
     xlab = "FT3 (pmol/L)",
     ylab = "logTSH (mIU/L)")

# fixing log TSH ----------------------------------------------------------

#create log10TSH column in each df, and fix inf values
new_tsh_ft4 <- log10(tsh_ft4$TSH_result)
tsh_ft4$logTSH <- new_tsh_ft4

new_tsh_ft3 <- log10(tsh_ft3$TSH_result)
tsh_ft3$logTSH <- new_tsh_ft3

#replace inf values with NA
tsh_ft4$logTSH <- ifelse(is.finite(tsh_ft4$logTSH), 
                                tsh_ft4$logTSH, NA)
tsh_ft4_filt <- tsh_ft4[!grepl("NA", tsh_ft4$logTSH),]

tsh_ft3$logTSH <- ifelse(is.finite(tsh_ft3$logTSH), 
                             tsh_ft3$logTSH, NA)
tsh_ft3_filt <- tsh_ft3[!grepl("NA", tsh_ft3$logTSH),]

# modelling ---------------------------------------------------------------
#regular linear regression
#create models for each relationship to compare on one plot
ft4_lin <- lm(FT4_result~TSH_result, data = tsh_ft4_filt)
ft4_log_lin <- lm(FT4_result~neg_logTSH, data = tsh_ft4_filt)

ft3_lin <- lm(FT3_result~TSH_result, data = tsh_ft3_filt)
ft3_log_lin <- lm(FT3_result~neg_logTSH, data = tsh_ft3_filt)

#view model summary
summary(ft4_lin)
summary(ft4_log_lin)
summary(ft3_lin)
summary(ft3_log_lin)

#compare model performance
compare_performance(ft4_lin, ft4_log_lin)
compare_performance(ft3_lin, ft3_log_lin)

#plot the linear vs quantile regression
plot(tsh_ft4_filt$logTSH, tsh_ft4_filt$FT4_result,
     main = "logTSH vs FT4",
     xlab = "logTSH (mIU/L)",
     ylab = "FT4 (pmol/L)")
abline(ft4_log_lin, col = "red")
text(x=1, y=60, "Simple regression", col='red')
abline(rq(FT4_result~logTSH, data = tsh_ft4_filt, tau = 0.5), col = "blue" )
text(x=1, y=50, "Quantile regression", col='blue')

plot(tsh_ft3_filt$logTSH, tsh_ft3_filt$FT3_result,
     main = "logTSH vs FT3",
     xlab = "logTSH (mIU/L)",
     ylab = "FT3 (pmol/L)")
abline(ft3_log_lin, col = "red")
text(x=1, y=40, "Simple regression", col='red')
abline(rq(FT3_result~logTSH, data = tsh_ft3_filt, tau = 0.5), col = "blue" )
text(x=1, y=30, "Quantile regression", col='blue')

#compare normal regression with quantile regression
compare_performance(ft4_log_lin,rq(FT4_result~logTSH, data = tsh_ft4_filt, tau = 0.5))
compare_performance(ft3_log_lin,rq(FT3_result~logTSH, data = tsh_ft3_filt, tau = 0.5))

#get info on the equation of the line for quantile regression
summary(rq(FT4_result~logTSH, data = tsh_ft4_filt, tau = 0.5))
summary(rq(FT3_result~logTSH, data = tsh_ft3_filt, tau = 0.5))

#compare ft4 model with ft3 model:
compare_performance(rq(FT4_result~logTSH, data = tsh_ft4_filt, tau = 0.5),
                    rq(FT3_result~logTSH, data = tsh_ft3_filt, tau = 0.5))

# TFT status partitioning -------------------------------------------------
#start with FT4-TSH only for simplicity and separate data based on tsh values 
#normal tsh range: 0.55 - 4.78 mIU/L

hypo_ft4 <- tsh_ft4_filt[tsh_ft4_filt$TSH_result > 4.78,]
eu_ft4 <- tsh_ft4_filt[tsh_ft4$TSH_result <= 4.78 & tsh_ft4_filt$TSH_result >= 0.55,]
hyper_ft4 <- tsh_ft4_filt[tsh_ft4_filt$TSH_result < 0.55,]

#plot the data for each
plot(hypo_ft4$logTSH, hypo_ft4$FT4_result, 
     main = "FT4 vs -logTSH for hypothyroid range", 
     xlab = "logTSH (mIU/L)",
     ylab = "FT4 (pmol/L)")
plot(hyper_ft4$logTSH, hyper_ft4$FT4_result, 
     main = "F4 vs -log TSH for hyperthyroid range",
     xlab = "log TSH (mIU/L)",
     ylab = "FT4 (pmol/L)")
plot(eu_ft4$logTSH, eu_ft4$FT4_result, 
     main = "F4 vs -log TSH for euthyroid range",
     xlab = "log TSH (mIU/L)",
     ylab = "FT4 (pmol/L)")

#create linear quantile model for each and plot on all of the data
rq_hypo <- rq(FT4_result~logTSH, data = hypo_ft4, tau=0.5)
rq_eu <- rq(FT4_result~logTSH, data = eu_ft4, tau=0.5)
rq_hyper <- rq(FT4_result~logTSH, data = hyper_ft4, tau=0.5)
rq_all <- rq(FT4_result~logTSH, data = tsh_ft4_filt, tau=0.5)

ft4_model_plot <- ggplot() +
  geom_point(data = hypo_ft4, aes(x= logTSH, y= FT4_result,color = "Hypothyroid"), show.legend = TRUE) +
  geom_point(data = eu_ft4, aes(x = logTSH, y = FT4_result,color = "Euthyroid"), show.legend = TRUE) +
  geom_point(data = hyper_ft4, aes(x=logTSH, y=FT4_result,color="Hyperthyroid"), show.legend=TRUE) +
  labs(x = "log TSH (mIU/L)", y = "FT4 (pmol/L)", title = "logTSH vs FT4") +
  scale_color_manual(name="TSH range",
                     values=c("Hypothyroid" = "mediumpurple4", "Euthyroid"="steelblue","Hyperthyroid"="seagreen4")) +
  geom_abline(intercept = coef(rq_hypo)[1], slope = coef(rq_hypo)[2],lty=5, color = "black") +
  geom_abline(intercept = coef(rq_eu)[1], slope = coef(rq_eu)[2],lty=5, color = "black") +
  geom_abline(intercept = coef(rq_hyper)[1], slope=coef(rq_hyper)[2],lty=5, color="black") +
  geom_abline(intercept = coef(rq_all)[1],slope=coef(rq_all)[2],lty=1, color="black") +
  ylim(0,60)

ft4_model_plot

#built in R plot method
plot(tsh_ft4_filt$logTSH, tsh_ft4_filt$FT4_result,
     main = "logTSH vs FT4",
     xlab = "logTSH (mIU/L)",
     ylab = "FT4 (pmol/L)")
abline(rq(FT4_result~logTSH, data = hypo_ft4), lty = 1,lwd=2, col = "mediumpurple4")
text(x=1.5, y=55, "TSH > 4.78", col='mediumpurple4')
text(x=1.5, y=50, "[FT4] = -3.94log[TSH] + 14.95", col='mediumpurple4')
abline(rq(FT4_result~logTSH, data = eu_ft4, tau = 0.5), lty=1,lwd=2, col = "steelblue" )
text(x=-0.2, y=55, "0.55 < TSH < 4.78", col='steelblue')
text(x=-0.2, y=50, "[FT4] = -0.67log[TSH] + 12.63", col='steelblue')
abline(rq(FT4_result~logTSH, data = hyper_ft4, tau = 0.5), lty=1,lwd=2, col = "seagreen")
text(x=-1.5, y=55, "TSH < 0.55", col = "seagreen")
text(x=-1.5, y=50, "[FT4] = -2.97log[TSH] + 11.97", col = "seagreen")
abline(v = 0.26,lty = 5, col = "black")
abline(v = -0.68,lty = 5, col = "black")

#get summary of the model parameters
summary(rq_all)
summary(rq_hypo)
summary(rq_eu)
summary(rq_hyper)

# TFT partitioned analysis for FT3 ----------------------------------------
#partition the data
hypo_ft3 <- tsh_ft3_filt[tsh_ft3_filt$TSH_result > 4.78,]
eu_ft3 <- tsh_ft3_filt[tsh_ft3$TSH_result <= 4.78 & tsh_ft3_filt$TSH_result >= 0.55,]
hyper_ft3 <- tsh_ft3_filt[tsh_ft3_filt$TSH_result < 0.55,]

#plot the data for each
plot(hypo_ft3$logTSH, hypo_ft3$FT3_result, 
     main = "FT3 vs -logTSH for hypothyroid range", 
     xlab = "logTSH (mIU/L)",
     ylab = "FT3 (pmol/L)")
plot(hyper_ft3$logTSH, hyper_ft3$FT3_result, 
     main = "FT3 vs -log TSH for hyperthyroid range",
     xlab = "log TSH (mIU/L)",
     ylab = "FT3 (pmol/L)")
plot(eu_ft3$logTSH, eu_ft3$FT3_result, 
     main = "FT3 vs -log TSH for euthyroid range",
     xlab = "log TSH (mIU/L)",
     ylab = "FT3 (pmol/L)")

#create linear quantile model for each and plot on all of the data

plot(tsh_ft3_filt$logTSH, tsh_ft3_filt$FT3_result,
     main = "logTSH vs FT3",
     xlab = "logTSH (mIU/L)",
     ylab = "FT3 (pmol/L)")
abline(rq(FT3_result~neg_logTSH, data = hypo_ft3), lty = 1, col = "red")
text(x=-1.5, y=40, "hypothyroid", col='red')
abline(rq(FT3_result~neg_logTSH, data = eu_ft3, tau = 0.5), lty=1, col = "blue" )
text(x=-0.2, y=40, "euthyroid", col='blue')
abline(rq(FT3_result~neg_logTSH, data = hyper_ft3, tau = 0.5), lty=3, col = "green")
text(x=1.5, y=40, "hyperthyroid", col = "green")
abline(v = 0.26,lty = 5, col = "black")
abline(v = -0.68,lty = 5, col = "black")

#get model info for equations of each line
summary(rq(FT3_result~neg_logTSH, data = hypo_ft3))
summary(rq(FT3_result~neg_logTSH, data = eu_ft3, tau = 0.5))
summary(rq(FT3_result~neg_logTSH, data = hyper_ft3, tau = 0.5))

#analysis of population characteristics

#create histogram of the tft data for the population

hist(tsh_ft4_filt$TSH_result,
     main = "TSH",
     col = "mediumpurple",
     xlab = "TSH (mIU/L)",
     xlim = c(0,15),
     breaks = 150,
     prob = TRUE)
lines(x = density(x =tsh_ft4_filt$TSH_result), col = "black", lwd = 2)

hist(tsh_ft3_filt$FT3_result,
     main = "FT3",
     col = "steelblue",
     xlab = "FT3 (pmol/L)",
     prob = TRUE,
     xlim = c(1,8),
     breaks = 200)
lines(x = density(x = tsh_ft3_filt$FT3_result), col = "black", lwd = 2)

hist(tsh_ft4_filt$FT4_result,
     main = "FT4",
     col = "skyblue",
     xlab = "FT4 (pmol/L)",
     xlim = c(0,25),
     breaks = 100,
     prob = TRUE)
lines(x = density(x = tsh_ft4_filt$FT4_result), col = "black", lwd = 2)

#compare means between groups

t.test(ft4_female$FT4_result, ft4_male$FT4_result, var.equal = FALSE)

t.test(ft3_female$FT3_result, ft3_male$FT3_result, var.equal = FALSE)
