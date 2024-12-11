#proposed site map
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(RColorBrewer)
library(tidyverse)
library(mapdata)
library(RColorBrewer)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(tidyr)
library(ggsignif)
library(dplyr)
library(tidyverse)
library(ggpubr)

#creating a sample data.frame with your lat/lon points
lon <- c(-89.50,-89.35)
lat <- c(43.05, 43.16)
coordinates <- as.data.frame(cbind(lon,lat))
mendota_outline <- st_read("/Users/nick/Desktop/stripe_data/mendota_sf/805400.shp")

getwd()
setwd("Desktop/stripe_data")
plan <- read.csv("plan.csv")

ggplot() + 
  geom_sf(data = mendota_outline, linewidth = 1, col = "black")+ 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(panel.background = element_blank())

ggplot() +
  geom_sf(data = mendota_outline, linewidth = 0.5, col = "black")+
  geom_point(data= plan, aes(x=lon, y=lat, color= zebra_mussels), size=3, alpha=0.5)+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"))+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_colour_manual(values = c("purple","red"), name="Zebra Mussels")
ggsave("siteplan.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')




