# YSI

library(ggplot2)
library(tidyverse)
library(dplyr)

getwd()
setwd("Desktop/stripe_data/data")
ysi <- read.csv("stripe_ysi_2023.csv")

ysi$location <- sub(" ", "_", ysi$location)

#Depth profile 
temp <- ggplot(ysi, aes(temp.c, depth)) +
  geom_line(orientation = "y", data= ysi,
             aes(x=temp.c, y=depth,
            color= location),
            size=1, alpha=0.5)
  
temp + scale_y_reverse()

warner <- filter(ysi,location=='Warner_Beach')
warnertemp <- ggplot(warner, aes(temp.c, depth)) +
  geom_line(orientation="y", data= warner,
            aes(x=temp.c, y=depth,
                color= location),
            size=1, alpha=0.5)+
  geom_point(data= warner,
             aes(x=temp.c, y=depth,
                 color = location,
                 shape = location,
                 size=1, alpha=0.5))
warnertemp + scale_y_reverse()

