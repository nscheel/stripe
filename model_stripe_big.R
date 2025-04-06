#big
wet_zmthg %>% filter(size == "13-20 mm") -> big


m1 <- lm(thg.wet.ng.g ~ poly(daynum ,2), data = big)
m2 <- lm(thg.wet.ng.g ~ daynum + I(daynum^2), data = big)

m1
m2

predict(m1, newdata = list("x" = 0.5))

#plotting setup
par(mfrow = c(3, 2), mar = c(2, 2, 1, 1)) # set up 6 subplots

#setup
RMSE <- data.frame("kth_order" = NA, "RMSE" = NA) # empty data frame to store RMSE
#vals <- list("daynum" <- seq(min(wet_zmthg$daynum), 
#  max(wet_zmthg$thg.wet.ng.g), 
# by = -1)) # set up vector used for prediction

vals <- list("daynum" <- seq(min(big$thg.wet.ng.g), 
                             max(big$daynum), 
                             by = 1)) # set up vector used for prediction

k <- c(1, 2, 3, 5, 9, 12) # k-th order

for (i in 1:length(k)) {
  # build models
  model <- lm(thg.wet.ng.g ~ poly(daynum, k[i]), data = big)
  
  # calculate RMSE and store it for further usage
  RMSE[i, 1] <- k[i] # store k-th order
  RMSE[i, 2] <- sqrt(sum((fitted(model) - big$thg.wet.ng.g)^2) / length(big$thg.wet.ng.g)) # calculate RMSE
  
  # predict
  predictions <- predict(model, newdata = vals)
  # plot
  plot(big$daynum, big$thg.wet.ng.g, pch = 16, col = "blue",
       ylim = c(min(big$thg.wet.ng.g) * 1.3, max(big$thg.wet.ng.g) * 1.3))
  lines(vals[[1]], predictions, lwd = 2, col = "red")
  text(x = 250, y = 140, paste0("k = ", k[i], ", RMSE = ", round(RMSE[i, 2], 3))) # annotate the plot
}

par(mfrow = c(3, 2), mar = c(2, 2, 1, 1)) # set up 6 subplots


RMSE <- data.frame("kth_order" = NA, "RMSE" = NA) # empty data frame to store RMSE
#vals <- list("daynum" <- seq(min(wet_zmthg$daynum), 
#  max(wet_zmthg$thg.wet.ng.g), 
# by = -1)) # set up vector used for prediction

vals <- list("daynum" <- seq(min(big$thg.wet.ng.g), 
                             max(big$daynum), 
                             by = 1)) # set up vector used for prediction


library(caTools)
set.seed(100) # set seed for reproducibility


# define split vector
split <- sample.split(big$thg.wet.ng.g, SplitRatio = 0.65)


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
vals <- list("x" <- seq(min(big$thg.wet.ng.g), max(big$daynum), by = 1))
bigpredictions <- predict(final_model, newdata = vals, interval = "confidence", level = 0.95)

# plot data
plot(big$daynum, big$thg.wet.ng.g, pch = 16, ylab = "", xlab = "")

# plot data generation function
lines(vals[[1]], sin(2 * pi * vals[[1]]), lwd = 3, col = "green")


# plot fit and confidence levels
lines(vals[[1]], bigpredictions[, "fit"], lwd = 2, col = "orange")
lines(vals[[1]], bigpredictions[, "upr"], lwd = 2, lty = 2, col = "orange")
lines(vals[[1]], bigpredictions[, "lwr"], lwd = 2, lty = 2, col = "orange")

