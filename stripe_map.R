#installing packages
#install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
#install.packages(c("maps", "mapdata"))
#install.packages("ggmap")

#loading packages
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

# creating a sample data.frame with your lat/lon points
lon <- c(-89.48,-89.35)
lat <- c(43.05, 43.16)
coordinates <- as.data.frame(cbind(lon,lat))

#loading in site coordinates
getwd()
setwd("Desktop/stripe_data")
sites <- read.csv("stripe_coordinates.csv")


#getting the map
register_google(key = "AIzaSyBVVexkM5Z51lP4uDzVjEnxwrCm_JMUriE")
map_mendota <- get_map(location = c(lon = mean(coordinates$lon), lat = mean(coordinates$lat)), zoom = 12,
                       maptype = "satellite", scale = 2)
figure_1 <- ggmap(map_mendota)
map_of_stripe_sites <- figure_1 + geom_point(data=sites, aes(x=lon, y=lat), color="#FF00FF", size=1, alpha=0.5) 

map_of_stripe_sites



