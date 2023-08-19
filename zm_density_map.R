##Zebra Mussel Density Map

#install packages
install.packages("RColorBrewer")

#loading packages
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(RColorBrewer)
library(tidyverse)

# creating a sample data.frame with your lat/lon points
lon <- c(-89.50,-89.35)
lat <- c(43.05, 43.16)
coordinates <- as.data.frame(cbind(lon,lat))

#loading in site coordinates
getwd()
setwd("Desktop/stripe_data")
sites <- read.csv("stripe_coordinates.csv")
sites <- replace(sites, sites=='', NA)
sites <- sites[!duplicated(sites), ]


#getting the zm density map
register_google(key = "AIzaSyBVVexkM5Z51lP4uDzVjEnxwrCm_JMUriE")
map_mendota <- get_map(location = c(lon = mean(coordinates$lon), lat = mean(coordinates$lat)), zoom = 12,
                       maptype = "satellite", scale = 2)
ggmap(map_mendota)

ZM_score_map <- ggmap(map_mendota)
map_of_stripe_sites <- ZM_score_map + geom_point(data=sites, aes(x=lon, y=lat, color= zm_density), size=1, alpha=0.5) + scale_color_gradient(low = "white", high="red", name = "ZM Score") 

map_of_stripe_sites + theme(
  axis.text.x = element_blank(),
  axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())

ggsave("zm_score_map.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#macrophyte density map
mac <- sites[!is.na(sites$macrophyte_density),]

register_google(key = "AIzaSyBVVexkM5Z51lP4uDzVjEnxwrCm_JMUriE")
map_mendota <- get_map(location = c(lon = mean(coordinates$lon), lat = mean(coordinates$lat)), zoom = 12,
                       maptype = "satellite", scale = 2)
ZM_score_map <- ggmap(map_mendota)
mdensity_plot <- ZM_score_map + geom_point(data=mac, aes(x=lon, y=lat, color= macrophyte_density), size=1, alpha=0.5) + scale_color_gradient(low = "white", high="green", name = "Macrophyte Score") 

mdensity_plot + theme(
  axis.text.x = element_blank(),
  axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())

ggsave("macrophyte_score_map.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#macrophyte species map

macs <- sites[!is.na(sites$macrophyte_species),]

register_google(key = "AIzaSyBVVexkM5Z51lP4uDzVjEnxwrCm_JMUriE")
map_mendota <- get_map(location = c(lon = mean(coordinates$lon), lat = mean(coordinates$lat)), zoom = 12,
                       maptype = "satellite", scale = 2)
ZM_score_map <- ggmap(map_mendota)
mspecies_plot <- ZM_score_map + geom_point(data=macs, aes(x=lon, y=lat, color= macrophyte_species), size=1, alpha=0.5)  

mspecies_plot + theme(
axis.text.x = element_blank(),
axis.text.y = element_blank()) +
theme(
axis.text.x = element_blank(),
axis.text.y = element_blank(),
axis.ticks = element_blank()) +
  scale_colour_manual(values = c("white", "red", "lightblue","pink","orange","purple","yellow"), , name = "Macrophyte Species")

ggsave("m_species_plot.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#native or non-native plants
macn <- sites[!is.na(sites$macrophyte_native_status),]

register_google(key = "AIzaSyBVVexkM5Z51lP4uDzVjEnxwrCm_JMUriE")
map_mendota <- get_map(location = c(lon = mean(coordinates$lon), lat = mean(coordinates$lat)), zoom = 12,
                       maptype = "satellite", scale = 2)
ZM_score_map <- ggmap(map_mendota)
native_plot <- ZM_score_map + geom_point(data=macn, aes(x=lon, y=lat, color= macrophyte_native_status), size=2, alpha=0.5)  

native_plot + theme(
  axis.text.x = element_blank(),
  axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) +
  scale_colour_manual(values = c("red","orange","yellow"), name="Native Status")

ggsave("native_status.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#zm density vs macrophyte denisty

zm_v_mac <- ggplot(sites, aes(macrophyte_density, zm_density))
zm_v_mac + geom_point(aes(colour = factor(macrophyte_species), size= 3)) + scale_colour_manual(values = c("black", "white", "red", "lightblue","pink","orange","purple","yellow"))

ggsave("mac_dens_vs_zm_dens.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')
