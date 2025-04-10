
#=============================================================================================================================
#STRIPE 2024 Zebra Mussel THg
#=============================================================================================================================
library(ggplot2)
library(readr)
library(dplyr)
library(lattice)
library(lubridate)
library(tidyverse)
library(ggpubr)
library(ggsignif)

#pull up data
setwd("~/Desktop/USGS/Data/")

zmthg <- read_csv("STRIPE_24_zmthg_averaged.csv")


setwd("~/Desktop/stripe_2024/data")
meta <- read_csv("stripe_24_meta.csv")

#JOIN DATASETS

joined_zmthg <- merge(zmthg, meta ,by= "BarcodeID" )

joined_zmthg$date <- mdy(joined_zmthg$date)


#dry mass and wet mass

setwd("~/Desktop/stripe_2024/data")

tares_ww <- read_csv("stripe24_tares_ww.csv")
dry <- read_csv("stripe24_dry_mass.csv")

wdtare <- merge(tares_ww, dry ,by= "BarcodeID" )


#adding columns for tared wet mass, tared dry mass, % moisture
wt <- wdtare$wetmass.tared.g <- (wdtare$wet.mass.g - wdtare$tare.mass.g)
dt <- wdtare$drymass.tared.g <- (wdtare$dry.mass.g - wdtare$tare.mass.g)
wdtare$pct_moisture <- ((wt-dt)/wt)

write.csv(wdtare,"stripe24_pct_moist.csv", row.names = FALSE)

#CALCULATING WET THG NG/G
wet_zmthg <- merge(joined_zmthg, wdtare ,by= "BarcodeID" )

# Cw = Cd X [(100 - %H) / 100]

cd = wet_zmthg$thg.dry.ng.g
h = wet_zmthg$pct_moisture

wet_zmthg$thg.wet.ng.g <- NA

wet_zmthg$thg.wet.ng.g <-   cd * ((100 - h) / 100)

ymd(wet_zmthg$date)

#establish turnover date
turnover <- as.numeric(as.Date("2024-10-23"))

#dry THg vs time
dryc_v_date <- ggplot(wet_zmthg, aes(date, thg.dry.ng.g, color= size))+
  geom_point(data= wet_zmthg, 
             aes(x=date , 
                 y= thg.dry.ng.g ))+
  labs(title = "ZM THg Dry Concentration 2024", 
       x = "Date", y = "THg Dry ng/g")+
  geom_smooth()+
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)
dryc_v_date

#wet weight vs time
wetc_v_date <- ggplot(wet_zmthg, aes(date, thg.wet.ng.g, color= size))+
  geom_point(data= wet_zmthg, 
             aes(x=date , 
                 y= thg.wet.ng.g, color= size))
wetc_v_date+ 
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)+
  geom_smooth()+ 
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)+
  labs(title = "ZM THg Wet Concentration 2024", 
       x = "Date", y = "THg Wet ng/g")

# making csv for wet thg concentration

getwd()
write.csv(wet_zmthg,"wet_zmthg.csv", 
          row.names = FALSE)

#annotate("text", label = "Turnover",
# x = turnover,
# y = 115)

#geom_label(x= turnover, y=115, label="Turnover")

#manual colors

wetc_v_date <- ggplot(wet_zmthg, aes(date, thg.wet.ng.g))+
  geom_point(data= wet_zmthg, 
             aes(x=date , 
                 y= thg.wet.ng.g), color= wet_zmthg$size)+ 
  scale_color_manual(values=c("#E69F00", "#56B4E9"))


wetc_v_date+ 
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)+
  geom_smooth()+
  geom_label(x= turnover, y=115, label="Turnover")

#filter by size
#6-13 mm

wet_noqa %>% filter(size == "6-13 mm") -> small

small_wetc <- ggplot(small, aes(date, thg.wet.ng.g))+
  geom_point(data= wet_noqa, 
             aes(x=date , 
                 y= thg.wet.ng.g))

small_wetc+ 
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)+
  geom_smooth()+
  labs(title = "6-13 mm", 
       x = "Date", y = "THg Wet ng/g")+
  geom_label(x= turnover, y=121, label="Turnover")+
  labs(title = "6-13 mm", 
       x = "Date", y = "THg Wet ng/g")


#13-20

turnover <- as.numeric(as.Date("2024-10-23"))

wet_noqa %>% filter(size == "13-20 mm") -> large

large_wetc <- ggplot(large, aes(date, thg.wet.ng.g))+
  geom_point(data= large, 
             aes(x=date , 
                 y= thg.wet.ng.g))
large_wetc+ 
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1)+
  geom_smooth()+
  labs(title = "13-20 mm", 
       x = "Date", y = "THg Wet ng/g")+
  geom_label(x= turnover, y=121, label="Turnover")

#fitting a curve

#plotting setup
par(mfrow = c(3, 2), mar = c(2, 2, 1, 1)) # set up 6 subplots

#setup
RMSE <- data.frame("kth_order" = NA, 
                   "RMSE" = NA) # empty data frame to store RMSE
vals <- list("x" <- seq(min(wet_zmthg$date), 
                        max(wet_zmthg$thg.wet.ng.g), 
                        by = 0.01)) # set up vector used for prediction

#=============================================================================================================================
                                        ### Polynomial Modeling all Datapoints ####
#=============================================================================================================================
  
#make row in data for yday

wet_zmthg$daynum<- yday(wet_zmthg$date)


m1 <- lm(thg.wet.ng.g ~ poly(daynum ,2), data = wet_zmthg)
m2 <- lm(thg.wet.ng.g ~ daynum + I(daynum^2), data = wet_zmthg)




m1

m2

predict(m1, newdata = list("x" = 0.5)) ####### throwing error. problem?#####

#plotting setup
par(mfrow = c(3, 2), mar = c(2, 2, 1, 1)) # set up 6 subplots

#setup
RMSE <- data.frame("kth_order" = NA, "RMSE" = NA) # empty data frame to store RMSE
#vals <- list("daynum" <- seq(min(wet_zmthg$daynum), 
#  max(wet_zmthg$thg.wet.ng.g), 
# by = -1)) # set up vector used for prediction

vals <- list("daynum" <- seq(min(wet_zmthg$thg.wet.ng.g), 
                             max(wet_zmthg$daynum), 
                             by = 1)) # set up vector used for prediction

k <- c(1, 2, 3, 5, 9, 12) # k-th order

for (i in 1:length(k)) {
  # build models
  model <- lm(thg.wet.ng.g ~ poly(daynum, k[i]), data = wet_zmthg)
  
  # calculate RMSE and store it for further usage
  RMSE[i, 1] <- k[i] # store k-th order
  RMSE[i, 2] <- sqrt(sum((fitted(model) - wet_zmthg$thg.wet.ng.g)^2) / length(wet_zmthg$thg.wet.ng.g)) # calculate RMSE
  
  # predict
  predictions <- predict(model, newdata = vals)
  # plot
  plot(wet_zmthg$daynum, wet_zmthg$thg.wet.ng.g, pch = 16, col = "blue",
       ylim = c(min(wet_zmthg$thg.wet.ng.g) * 1.3, max(wet_zmthg$thg.wet.ng.g) * 1.3))
  lines(vals[[1]], predictions, lwd = 2, col = "red")
  text(x = 250, y = 140, paste0("k = ", k[i], ", RMSE = ", round(RMSE[i, 2], 3))) # annotate the plot
}



par(mfrow = c(3, 2), mar = c(2, 2, 1, 1)) # set up 6 subplots

#setup
RMSE <- data.frame("kth_order" = NA, "RMSE" = NA) # empty data frame to store RMSE
#vals <- list("daynum" <- seq(min(wet_zmthg$daynum), 
#  max(wet_zmthg$thg.wet.ng.g), 
# by = -1)) # set up vector used for prediction

vals <- list("daynum" <- seq(min(wet_zmthg$thg.wet.ng.g), 
                             max(wet_zmthg$daynum), 
                             by = 1)) # set up vector used for prediction

k <- c(1, 2, 3, 5, 9, 12) # k-th order

for (i in 1:length(k)) {
  # build models
  model <- lm(thg.wet.ng.g ~ poly(daynum, k[i]), data = wet_zmthg)
  
  # calculate RMSE and store it for further usage
  RMSE[i, 1] <- k[i] # store k-th order
  RMSE[i, 2] <- sqrt(sum((fitted(model) - wet_zmthg$thg.wet.ng.g)^2) / length(wet_zmthg$thg.wet.ng.g)) # calculate RMSE
  
  # predict
  predictions <- predict(model, newdata = vals)
  # plot
  plot(wet_zmthg$daynum, wet_zmthg$thg.wet.ng.g, pch = 16, col = "blue",
       ylim = c(min(wet_zmthg$thg.wet.ng.g) * 1.3, max(wet_zmthg$thg.wet.ng.g) * 1.3))
  lines(vals[[1]], predictions, lwd = 2, col = "red")
  text(x = 250, y = 140, paste0("k = ", k[i], ", RMSE = ", round(RMSE[i, 2], 3))) # annotate the plot
}


#optimize k value

library(caTools)
set.seed(100) # set seed for reproducibility


# define split vector
split <- sample.split(wet_zmthg$thg.wet.ng.g, SplitRatio = 0.65)

# split data set
train_set <- wet_zmthg[split == TRUE, ]
val_set <- wet_zmthg[split == FALSE, ]

# check dimensions of the training and validation set
dim(train_set)
dim(val_set)

# setup
RMSE <- data.frame("kth_order" = NA, "RMSE_train" = NA, "RMSE_val" = NA) # empty data frame to store RMSE
vals <- list("x" <- seq(min(train_set$thg.wet.ng.g), max(train_set$daynum), by = 1)) # set up vector used for prediction

# run  loop
k <- seq(1, 12) # k-th order

for (i in k) {
  # build models
  model <- lm(thg.wet.ng.g ~ poly(daynum, k[i]), data = train_set)
  
  # calculate RMSE and store it for further usage
  RMSE[i, 1] <- k[i] # store k-th order
  RMSE[i, 2] <- sqrt(sum((fitted(model) - train_set$thg.wet.ng.g)^2) / length(train_set$thg.wet.ng.g)) # calculate RMSE of the training set
  
  # predict
  predictions <- predict(model, newdata = val_set)
  RMSE[i, 3] <- sqrt(sum((predictions - val_set$thg.wet.ng.g)^2) / length(val_set$thg.wet.ng.g)) # calculate RMSE of the validation set
}

# plot RMSE for training and validation set
plot(RMSE[, 1], RMSE[, 2],
     xlab = "k-th order",
     ylab = "RMSE",
     ylim = c(min(RMSE[, c(2, 3)]), max(RMSE[, c(2, 3)])),
     type = "b",
     col = "blue",
     pch = 16
)
lines(RMSE[, 3],
      type = "b",
      col = "red",
      pch = 16
)
legend("topright",
       legend = c("training set", "validation set"),
       lty = c(1, 1),
       col = c("blue", "red")
)

best_order <- RMSE[which.min(RMSE[, 3]), 1]

final_model <- lm(thg.wet.ng.g ~ poly(daynum, best_order), data = train_set)

# predictions
vals <- list("x" <- seq(min(wet_zmthg$thg.wet.ng.g), max(wet_zmthg$daynum), by = 1))
predictions <- predict(final_model, newdata = vals, interval = "confidence", level = 0.95)

# plot data
plot(wet_zmthg$daynum, wet_zmthg$thg.wet.ng.g, pch = 16, ylab = "", xlab = "")

# plot data generation function
lines(vals[[1]], sin(2 * pi * vals[[1]]), lwd = 3, col = "green")

# plot fit and confidence levels
lines(vals[[1]], predictions[, "fit"], lwd = 2, col = "blue")
lines(vals[[1]], predictions[, "upr"], lwd = 2, lty = 2, col = "blue")
lines(vals[[1]], predictions[, "lwr"], lwd = 2, lty = 2, col = "blue")

legend("topright",
       legend = c("observed data", "prediction", "signal"),
       lty = c(NA, 1, 1),
       pch = c(16, NA, NA),
       cex = 0.75,
       col = c(1, "blue", "green"),
       lwd = c(NA, 2, 2)
)

#=============================================================================================================================
                                        ### Modeling 6-13mm Subclass ####
#=============================================================================================================================
#try running again, throwing error!

wet_zmthg %>% filter(size == "6-13 mm") -> tiny #filter for 6-13mm  size class

par(mfrow = c(3, 2), mar = c(2, 2, 1, 1)) # set up 6 subplots

# split data set
split <- sample.split(tiny$thg.wet.ng.g, SplitRatio = 0.65) #split data

train_set <- tiny[split == TRUE, ]
val_set <-tiny[split == FALSE, ]

# check dimensions of the training and validation set
dim(train_set)
dim(val_set)

# setup
RMSE <- data.frame("kth_order" = NA, "RMSE_train" = NA, "RMSE_val" = NA) # empty data frame to store RMSE
vals <- list("x" <- seq(min(train_set$thg.wet.ng.g), max(train_set$daynum), by = 1)) # set up vector used for prediction

# run  loop
k <- seq(1, 10) # k-th order

for (i in k) {
  # build models
  model <- lm(thg.wet.ng.g ~ poly(daynum, k[i]), data = train_set)
  
  # calculate RMSE and store it for further usage
  RMSE[i, 1] <- k[i] # store k-th order
  RMSE[i, 2] <- sqrt(sum((fitted(model) - train_set$thg.wet.ng.g)^2) / length(train_set$thg.wet.ng.g)) # calculate RMSE of the training set
  
  # predict
  predictions <- predict(model, newdata = val_set)
  RMSE[i, 3] <- sqrt(sum((predictions - val_set$thg.wet.ng.g)^2) / length(val_set$thg.wet.ng.g)) # calculate RMSE of the validation set
}

# plot RMSE for training and validation set
plot(RMSE[, 1], RMSE[, 2],
     xlab = "k-th order",
     ylab = "RMSE",
     ylim = c(min(RMSE[, c(2, 3)]), max(RMSE[, c(2, 3)])),
     type = "b",
     col = "blue",
     pch = 16
)
lines(RMSE[, 3],
      type = "b",
      col = "red",
      pch = 16
)
legend("topright",
       legend = c("training set", "validation set"),
       lty = c(1, 1),
       col = c("blue", "red")
)

best_order <- RMSE[which.min(RMSE[, 3]), 1]

final_model <- lm(thg.wet.ng.g ~ poly(daynum, best_order), data = train_set)

# predictions
vals <- list("x" <- seq(min(tiny$thg.wet.ng.g), max(tiny$daynum), by = 1))
predictions <- predict(final_model, newdata = vals, interval = "confidence", level = 0.95)

# plot data
plot(tiny$daynum, tiny$thg.wet.ng.g, pch = 16, ylab = "", xlab = "")

# plot data generation function
lines(vals[[1]], sin(2 * pi * vals[[1]]), lwd = 3, col = "green")

# plot fit and confidence levels
lines(vals[[1]], predictions[, "fit"], lwd = 2, col = "blue")
lines(vals[[1]], predictions[, "upr"], lwd = 2, lty = 2, col = "blue")
lines(vals[[1]], predictions[, "lwr"], lwd = 2, lty = 2, col = "blue")

legend("topright",
       legend = c("observed data", "prediction", "signal"),
       lty = c(NA, 1, 1),
       pch = c(16, NA, NA),
       cex = 0.75,
       col = c(1, "blue", "green"),
       lwd = c(NA, 2, 2)
)

# plotting RMSE for different k values vs different polynomials

#plotting setup
par(mfrow = c(3, 2), mar = c(2, 2, 1, 1)) # set up 6 subplots

#setup
RMSE <- data.frame("kth_order" = NA, "RMSE" = NA) # empty data frame to store RMSE


vals <- list("daynum" <- seq(min(tiny$thg.wet.ng.g), 
                             max(tiny$daynum), 
                             by = 1)) # set up vector used for prediction

k <- c(1, 2, 3, 5, 9, 10) # k-th order

for (i in 1:length(k)) {
  # build models
  model <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, k[i]), data = tiny)
  
  # calculate RMSE and store it for further usage
  RMSE[i, 1] <- k[i] # store k-th order
  RMSE[i, 2] <- sqrt(sum((fitted(model) - tiny$thg.wet.ng.g)^2) / length(tiny$thg.wet.ng.g)) # calculate RMSE
  
  # predict
  predictions <- predict(model, newdata = vals)
  # plot
  plot(tiny$daynum, tiny$thg.wet.ng.g, pch = 16, col = "blue",
       ylim = c(min(tiny$thg.wet.ng.g) * 1.3, max(tiny$thg.wet.ng.g) * 1.3))
  lines(vals[[1]], predictions, lwd = 2, col = "red")
  text(x = 250, y = 140, paste0("k = ", k[i], ", RMSE = ", round(RMSE[i, 2], 3))) # annotate the plot
}

length(tiny$daynum)
length(tiny$thg.wet.ng.g)

# Generate predictions with 95% confidence intervals
predictions <- predict(tmodel5, newdata = data.frame(x = tiny$daynum), interval = "confidence", level = 0.95)

# View the first few predictions
head(predictions)

# Create a data frame for plotting
plot_tiny <- data.frame(x = tiny$daynum, y = tiny$thg.wet.ng.g, 
                        fit = predictions[, "fit"], 
                        lwr = predictions[, "lwr"], 
                        upr = predictions[, "upr"])

# Plot the data with the polynomial fit and confidence intervals
ggplot(plot_data, aes(x = tiny$daynum, y = tiny$thg.wet.ng.g)) +
  geom_point() +  # original data points
  geom_line(aes(y = fit), color = "blue") +  # fitted polynomial line
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +  # confidence interval
  labs(title = "Polynomial Model with Confidence Intervals")

#============================================================================================================================
                               #modeling 13-20 mm subclass #
#============================================================================================================================
wet_zmthg %>% filter(size == "13-20 mm") -> big


bm1 <- lm(thg.wet.ng.g ~ poly(daynum ,2), data = big)
bm2 <- lm(thg.wet.ng.g ~ daynum + I(daynum^2), data = big)

bm1
bm2

predict(m1, newdata = list("x" = 0.5))

#plotting setup
par(mfrow = c(3, 2), mar = c(2, 2, 1, 1)) # set up 6 subplots

#setup
bRMSE <- data.frame("kth_order" = NA, "bRMSE" = NA) # empty data frame to store bRMSE
#bvals <- list("daynum" <- seq(min(wet_zmthg$daynum), 
#  max(wet_zmthg$thg.wet.ng.g), 
# by = -1)) # set up vector used for prediction

bvals <- list("daynum" <- seq(min(big$thg.wet.ng.g), 
                             max(big$daynum), 
                             by = 1)) # set up vector used for prediction

k <- c(3, 4, 5, 6, 7, 8) # k-th order

for (i in 1:length(k)) {
  # build bmodels
  bmodel <- lm(thg.wet.ng.g ~ poly(daynum, k[i]), data = big)
  
  # calculate bRMSE and store it for further usage
  bRMSE[i, 1] <- k[i] # store k-th order
  bRMSE[i, 2] <- sqrt(sum((fitted(bmodel) - big$thg.wet.ng.g)^2) / length(big$thg.wet.ng.g)) # calculate bRMSE
  
  # predict
  bpredictions <- predict(bmodel, newdata = bvals)
  # plot
  plot(big$daynum, big$thg.wet.ng.g, pch = 16, col = "orange",
       ylim = c(min(big$thg.wet.ng.g) * 1.3, max(big$thg.wet.ng.g) * 1.3))
  lines(bvals[[1]], bpredictions, lwd = 2, col = "red")
  text(x = 250, y = 140, paste0("k = ", k[i], ", bRMSE = ", round(bRMSE[i, 2], 3))) # annotate the plot
}

par(mfrow = c(3, 2), mar = c(2, 2, 1, 1)) # set up 6 subplots


bRMSE <- data.frame("kth_order" = NA, "bRMSE" = NA) # empty data frame to store bRMSE
#bvals <- list("daynum" <- seq(min(wet_zmthg$daynum), 
#  max(wet_zmthg$thg.wet.ng.g), 
# by = -1)) # set up vector used for prediction

bvals <- list("daynum" <- seq(min(big$thg.wet.ng.g), 
                             max(big$daynum), 
                             by = 1)) # set up vector used for prediction


library(caTools)
set.seed(100) # set seed for reproducibility


# define split vector
bsplit <- sample.split(big$thg.wet.ng.g, SplitRatio = 0.65)


# split data set
btrain_set <- big[bsplit == TRUE, ]
bval_set <- big[bsplit == FALSE, ]

# check dimensions of the training and validation set
dim(btrain_set)
dim(bval_set)

# setup
bRMSE <- data.frame("kth_order" = NA, "bRMSE_train" = NA, "bRMSE_val" = NA) # empty data frame to store bRMSE
bvals <- list("x" <- seq(min(btrain_set$thg.wet.ng.g), max(btrain_set$daynum), by = 1)) # set up vector used for prediction

# run  loop
k <- seq(1, 12) # k-th order

for (i in k) {
  # build bmodels
  bmodel <- lm(thg.wet.ng.g ~ poly(daynum, k[i]), data = btrain_set)
  
  
  # calculate bRMSE and store it for further usage
  bRMSE[i, 1] <- k[i] # store k-th order
  bRMSE[i, 2] <- sqrt(sum((fitted(bmodel) - btrain_set$thg.wet.ng.g)^2) / length(btrain_set$thg.wet.ng.g)) # calculate bRMSE of the training set
  
  # predict
  bpredictions <- predict(bmodel, newdata = bval_set)
  bRMSE[i, 3] <- sqrt(sum((bpredictions - bval_set$thg.wet.ng.g)^2) / length(bval_set$thg.wet.ng.g)) # calculate bRMSE of the validation set
}


# plot bRMSE for training and validation set
plot(bRMSE[, 1], bRMSE[, 2],
     xlab = "k-th order",
     ylab = "bRMSE",
     ylim = c(min(bRMSE[, c(2, 3)]), max(bRMSE[, c(2, 3)])),
     type = "b",
     col = "blue",
     pch = 16
)
lines(bRMSE[, 3],
      type = "b",
      col = "red",
      pch = 16
)
legend("topright",
       legend = c("training set", "validation set"),
       lty = c(1, 1),
       col = c("blue", "red")
)

best_order <- bRMSE[which.min(bRMSE[, 3]), 1]

final_bmodel <- lm(thg.wet.ng.g ~ poly(daynum, best_order), data = btrain_set)  

# bpredictions
bvals <- list("x" <- seq(min(big$thg.wet.ng.g), max(big$daynum), by = 1))
bpredictions <- predict(final_bmodel, newdata = bvals, interval = "confidence", level = 0.95)

# plot data
plot(big$daynum, big$thg.wet.ng.g, pch = 16, ylab = "", xlab = "", col = "orange")

# plot data generation function
lines(bvals[[1]], sin(2 * pi * bvals[[1]]), lwd = 3, col = "green")


# plot fit and confidence levels
lines(bvals[[1]], bpredictions[, "fit"], lwd = 2, col = "orange")
lines(bvals[[1]], bpredictions[, "upr"], lwd = 2, lty = 2, col = "orange")
lines(bvals[[1]], bpredictions[, "lwr"], lwd = 2, lty = 2, col = "orange")


#==============================================================================================================================
                                                  #Evaluating model for 13-20 mm
#==============================================================================================================================
library(ISLR)
library(dplyr)
library(ggplot2)

#models
bmodel1 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 1), data = big)
bmodel2 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 2), data = big)
bmodel3 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 3), data = big)
bmodel4 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 4), data = big)
bmodel5 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 5), data = big)
bmodel6 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 6), data = big)
bmodel7 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 7), data = big)

#ANOVA
print(anova(bmodel1,bmodel2,bmodel3,bmodel4,bmodel5, bmodel6, bmodel7))

#BIC
BIC_bs <- print(BIC(bmodel1,bmodel2,bmodel3,bmodel4,bmodel5, bmodel6, bmodel7))

orders_b <- c(1,2,3,4,5,6,7)
plot(BIC_bs$BIC ~ orders_b)

BIC_b4 <-BIC(bmodel4)
print(BIC_b4)

#confidence interval for best model

# Generate predictions with 95% confidence intervals
predictions <- predict(bmodel4, newdata = data.frame(x = x), 
                       interval = "confidence", level = 0.95)

head(predictions)

#plotting with CI

# Create a data frame for plotting
plot_data <- data.frame(x = big$daynum, y = big$thg.wet.ng.g, 
                        fit = predictions[, "fit"], 
                        lwr = predictions[, "lwr"], 
                        upr = predictions[, "upr"])

# Plot the data with the polynomial fit and confidence intervals
bigp <- ggplot(plot_data, aes(x = big$daynum, y = big$thg.wet.ng.g)) +
  geom_point() +  # original data points
  geom_line(aes(y = fit), color = "blue") +  # fitted polynomial line
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +  # confidence interval
  labs(title = "Seasonal variation in Zebra Mussel THg (13-20mm)" )+
  xlab("Day of Year")+
  ylab("Total Hg concentration (ng/g)")+
  annotate(geom="text", x=300, y=100, label="k=4",
           color="black")+
  annotate("text", x = 300, y = 90, 
           label = paste("BIC = ", round(BIC_b4, 2)), 
           color = "black")
  
  
bigp
# plot model
par(mfrow=c(1,1))

plot(big$thg.wet.ng.g~ big$daynum, col= "orange")+
  lines(sort(big$daynum),
        fitted(bmodel4)[order(big$daynum)],
        col = "orange")+
text(x = 285, y = 100, paste0("k = ", 4, ", BIC = ", signif(BIC_b4, 4)))



#==============================================================================================================================
                                                  #Evaluating model for 6-13 mm
#==============================================================================================================================

#BIC
tmodel1 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, degree =1, raw= T), data = tiny) #n272.3234
BICt1 <- BIC(tmodel1)

tmodel2 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, 2), data = tiny)
BICt2 <- BIC(tmodel2)

tmodel3 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, 3), data = tiny)
BICt3 <- BIC(tmodel3)

tmodel4 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, 4), data = tiny)
BICt4 <- BIC(tmodel4)

tmodel5 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, 5), data = tiny)
BICt5 <- BIC(tmodel5)

tmodel6 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, 6), data = tiny)
BICt6 <- BIC(tmodel6)

tmodel7 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, 7), data = tiny)
BICt7 <- BIC(tmodel7)

orders_t <- c(1,2,3,4,5,6,7)
BICts <- c(BICt1, BICt2, BICt3, BICt4, BICt5, BICt6, BICt7)

print(BICts)
plot(BICts ~ orders_t)
#ANOVA
print(anova(tmodel1,tmodel2,tmodel3,tmodel4,tmodel5, tmodel6, tmodel7))


an_t5 <- print(anova(tmodel5))


#AIC
library(AICcmodavg)

#define list of models
models <- list(tmodel1, tmodel2, tmodel3, tmodel4, tmodel5, tmodel6, tmodel7)

#specify model names
mod.names <- c('first_order', 'second_order',
               'third_order', 'fourth_order',
               'fifth_order', 'sixth_order',
               'seventh_order')

aictab(cand.set = models, modnames = mod.names)

# plot model

plot(tiny$thg.wet.ng.g~ tiny$daynum, col= "blue")+
  lines(sort(tiny$daynum),
        fitted(tmodel5)[order(tiny$daynum)],
        col = "blue")+
text(x = 285, y = 110, paste0("k = ", 5, ", BIC = ", signif(BICt5 , 4)))+
text(x = 285, y = 105, paste0("Pr > F =", 3.05*10^-8) )


# Create a data frame for plotting
plot_data <- data.frame(x = tiny$daynum, y = tiny$thg.wet.ng.g, 
                        fit = predictions[, "fit"], 
                        lwr = predictions[, "lwr"], 
                        upr = predictions[, "upr"])

# Plot the data with the polynomial fit and confidence intervals
bigp <- ggplot(plot_data, aes(x = tiny$daynum, y = tiny$thg.wet.ng.g)) +
  geom_point() +  # original data points
  geom_line(aes(y = fit), color = "blue") +  # fitted polynomial line
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +  # confidence interval
  labs(title = "Seasonal variation in Zebra Mussel THg (6-13mm)" )+
  xlab("Day of Year")+
  ylab("Total Hg concentration (ng/g)")+
  annotate(geom="text", x=300, y=100, label="k=5",
           color="black")+
  annotate("text", x = 300, y = 95, 
           label = paste("BIC = ", round(BICt5, 2)), 
           color = "black")
  
  
text(x = 285, y = 100, paste0("k = ", 4, ", BIC = ", signif(BIC_t5, 4)))
bigp




#================================================================================================================

#Bar Plot by season tiny

#================================================================================================================

tiny <- tiny %>%
  mutate(season = case_when(between(date, as.Date("2024-06-01"), 
                                    as.Date("2024-09-01")) ~ "summer", 
                            between(date, as.Date("2024-9-01"), 
                                    as.Date("2024-10-26")) ~ "early_fall", 
                            TRUE ~ "late_fall")) 

season_box <- boxplot(thg.wet.ng.g ~ season, data = tiny, 
                        xlab = "Season",
                        ylab = "Total Hg (ng/g)", 
                        main = "Total Hg in ZM",
                        notch = FALSE, 
                        varwidth = TRUE, 
                        col = c("white") 
                        
                      #names = c("0","1", "2", "3", "4", "5") 
)

one.way <- aov(thg.wet.ng.g ~ season, data = tiny)

summary(one.way)

#test if they're normally distributed
shapiro.test(tiny$thg.wet.ng.g[tiny$season == "summer"])
shapiro.test(tiny$thg.wet.ng.g[tiny$season == "early_fall"])
shapiro.test(tiny$thg.wet.ng.g[tiny$season == "late_fall"])

#late fall is not normally distributed so using Kruskal-Wallis

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(thg.wet.ng.g ~ season, data = tiny)

# Display result
kruskal_result

#p-value is less than p<0.05, there is statistical significance. using Dunn's test for pairwise

# load the FSA package
library(FSA)

# Perform Dunn's Test
dunnTest(thg.wet.ng.g ~ season, data = tiny, method = "bonferroni")


#==============================================
#plotting both models together
#==============================================

#confidence interval for best model

# Generate predictions with 95% confidence intervals
predictions <- predict(bmodel4, newdata = data.frame(x = x), 
                       interval = "confidence", level = 0.95)

head(predictions)

#plotting with CI

# Create a data frame for plotting
plot_data <- data.frame(x = big$daynum, y = big$thg.wet.ng.g, 
                        fit = predictions[, "fit"], 
                        lwr = predictions[, "lwr"], 
                        upr = predictions[, "upr"])

# Plot the data with the polynomial fit and confidence intervals
bigp <- ggplot(plot_data, aes(x = big$daynum, y = big$thg.wet.ng.g)) +
  geom_point() +  # original data points
  geom_line(aes(y = fit), color = "blue") +  # fitted polynomial line
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +  # confidence interval
  labs(title = "Seasonal variation in Zebra Mussel THg (13-20mm)" )+
  xlab("Day of Year")+
  ylab("Total Hg concentration (ng/g)")+
  annotate(geom="text", x=300, y=100, label="k=4",
           color="black")+
  annotate("text", x = 300, y = 90, 
           label = paste("BIC = ", round(BIC_b4, 2)), 
           color = "black")


bigp

