#Mendota Ice

library(ggplot2)
library(readr)
library(dplyr)
library(lattice)
library(lubridate)
library(tidyverse)

setwd("Desktop/ntl_data")
ice <- read_csv("icedata.csv")

#just lake mendota
ice %>% filter(lakeid== "ME") -> ice_me

#filter years
#ice_me$year4 <-as.numeric(ice_me$year4)



ice_me %>% filter(year4 > 2015) -> zm_ice

ice_v_date <- ggplot(zm_ice, aes(sample_date, totice))
ice_v_date + 
  geom_point(data= zm_ice, 
            aes(x=sampledate , 
                y= totice ))

#barplot
barplot(zm_ice$totice, names.arg=zm_ice$year4 , xlab = "Year", ylab = "Total Ice (cm)", main ="Ice Thickness")



