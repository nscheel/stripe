#near shore profile

#loading packages
library(ggplot2)
library(tidyverse)

#reading in
getwd()
setwd("Desktop/stripe_2024/data/")
profile <- read.csv("profilejul17.csv")

#temp
temp <- ggplot(profile, aes(Temp, depth))
temp + 
  geom_path()+
  scale_y_reverse()+
  facet_wrap(~ Date,ncol=5, scales="fixed")

#oxygen

do <- ggplot(profile, aes(DO.L, depth))
do + 
  geom_path()+
  scale_y_reverse()+
  facet_wrap(~ Date,ncol=5, scales="fixed")

#map

sf <- ggplot()+ 
  geom_sf(data = mendota_outline, linewidth = 1, col = "gray40")+ 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(panel.background = element_blank()) 

sites <- data.frame(
  sitename = c("CFL", "Anoxia"),
  lat = c(43.0775215, 43.07985),
  lon = c(-89.4029731, -89.40304))

anoxia_site <- sf+  geom_point(
  data= sites, 
  aes(x=lon, y=lat),
  size=1, alpha=100,
  stroke= 1,
  color = "red")+ 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) +
  theme(
    panel.background = element_rect(fill = "gray88",
                                    colour = "gray88",
                                    size = 0.5, linetype = "solid"))+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
anoxia_site

#geom_label(
  #size=3, nudge_x = 0,nudge_y = 0, angle= 0,
  #data= anoxia, aes(x= lon, y=lat, label=sitename,))+


  
