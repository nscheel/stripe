#water level

library(ggplot2)
library(readr)
library(dplyr)
library(lattice)
library(lubridate)
library(tidyverse)

setwd("Desktop/ntl_data")
waterlvl <- read_csv("waterlvl.csv")

### NOTE THIS DATA CAME FROM THE USGS BY WAY OF 
### DANE COUNTRY LAND AND WATER RESOURCES DEPT, NOT LTER
### I JUST GROUPED IT WITH LTER FOLDER FOR CONVENIENCE

waterlvl$date <- mdy(waterlvl$date)

#plot
waterlvl_v_date <- ggplot(waterlvl, aes(date, level))
waterlvl_v_date+ 
  geom_point(data= waterlvl, 
            aes(x=date, 
                y= level)) +
  labs(title = "Lake Mendota Water Level", 
       x = "Date", y = "Water Level (ft)")

#filter out weird points (for now)
waterlvl %>% filter(level > 848) -> cleaned

waterlvl_v_date_cleaned <- ggplot(cleaned, aes(date, level))
waterlvl_v_date_cleaned + 
  geom_line(linewidth= 1, data= cleaned, 
             aes(x=date, 
                 y= level)) +
  labs(title = "Lake Mendota Water Level", 
       x = "Date", y = "Water Level (ft)")

