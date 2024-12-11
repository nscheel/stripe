#loading packages
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
library(ggpubr)
library(ggrepel)

#creating a sample data.frame with your lat/lon points
lon <- c(-89.50,-89.35)
lat <- c(43.05, 43.16)
coordinates <- as.data.frame(cbind(lon,lat))

#loading in data sets
getwd()
setwd("Desktop/stripe_data")
sites <- read.csv("stripe_coordinates.csv")
sites <- replace(sites, sites=='', NA)
sites <- sites[!duplicated(sites), ]

sed <- read.csv("sed_hg_total.csv")
ddl <- read.csv("sed_ddl.csv")
meta <- read.csv("stripe_2023_metadata.csv")
plant <- read.csv("STRIPE_2023_M_THG.csv")
sed_mehg <- read.csv("sed_mehg.csv")
inlets <- read.csv("inlets.csv")

total <- merge(sed, meta ,by= "barcode" )
sed_mehg_j <- merge(total, sed_mehg)

#total <- merge(sed, meta , by.x= "Comments" , by.y= "barcode")

#total map

register_google(key = "AIzaSyBVVexkM5Z51lP4uDzVjEnxwrCm_JMUriE")
map_mendota <- get_map(location = c(lon = mean(coordinates$lon), lat = mean(coordinates$lat)), zoom = 12,
                       maptype = "satellite", scale = 2)
ggmap(map_mendota)

ZM_score_map <- ggmap(map_mendota)
map_of_stripe_sites <- ZM_score_map + geom_point(data= total, aes(x=lon, y=lat, color= NG.G.RESULT), size=1, alpha=0.5) + scale_color_gradient(low = "white", high="purple", name = "Total Hg (ng/g)") 

map_of_stripe_sites + theme(
  axis.text.x = element_blank(),
  axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())

ggsave("hgtotal_map.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#substrate map
substrate_map <- ggmap(map_mendota) + geom_point(data= total, aes(x=lon, y=lat, color= bottom_substrate), size=1, alpha=0.5)

substrate_map
ggsave("substrate_map.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#sed total hg vs zebra mussels, factor bottom substrate

zm_v_total <- ggplot(total, aes(zm_density, NG.G.RESULT))
zm_v_total + geom_point(aes(colour = factor(bottom_substrate)))+
  labs(color='Substrate Type')+
xlab("ZM Score")+
  ylab("Total Hg (ng/g)")+
  


ggsave("hgtotalvzm.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')
#plant density vs zebra mussels

plant_v_total <- ggplot(total, aes(macrophyte_density, NG.G.RESULT))
plant_v_total + geom_point(aes(colour = factor(bottom_substrate), size= 3, shape= factor()))

#native or invasive plants vs sed total hg
macn <- replace(total, total=='', NA)
macn <- total[!is.na(macn$native_status),]

plant_v_total <- ggplot(macn, aes(macrophyte_density, NG.G.RESULT))
plant_v_total + geom_point(aes(colour = factor(native_status), size= 3))

lowdoc$zm_density <- as.numeric(lowdoc$zm_density)
class(lowdoc$zm_density)
#plant species vs sed total hg
macs <- replace(total, total=='', NA)
macs <- total[!is.na(macs$macrophyte_species),]

plant_v_total <- ggplot(macs, aes(macrophyte_species, NG.G.RESULT))
plant_v_total + geom_point(aes(colour = factor(macrophyte_species), size= 3))

#depth vs total hg

depth_v_total <- ggplot(total, aes(bottom_depth_m, NG.G.RESULT))
depth_v_total + geom_point(aes(colour = factor(bottom_substrate)))

#boxplot zm density vs sed total
lowdoc <- total[-(which(total$bottom_substrate %in% "muck")),]

zm_total_box <- boxplot(NG.G.RESULT ~ zm_density, data = lowdoc, 
                     xlab = "ZM Score",
                     ylab = "Total Hg (ug/g)", 
                     main = "Total Mercury (Sandy/Rocky Sediment)",
                     notch = FALSE, 
                     varwidth = TRUE, 
                     col = c("white"),
                     names = c("0","1", "2", "3", "4", "5")) 

zm_total_box_with_points <-

ggplot(lowdoc, aes(x=lowdoc$zm_density, group=lowdoc$zm_density, y=lowdoc$NG.G.RESULT)) +
  geom_boxplot() +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  stat_boxplot(geom = "errorbar",
               width = 1) 

 zm_total_box_with_points + ggtitle("ZM Density vs Total Hg") +
  xlab("ZM Score") + ylab("Total Hg (ng/g)")
 
 #zebra mussels high low none
zmhilonone <- ggplot(lowdoc, aes(x=zebra_mussels, group=zebra_mussels, y=lowdoc$NG.G.RESULT)) +
   geom_boxplot(fill= "purple") +
   geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
   stat_boxplot(geom = "errorbar",
                width = 0.25) 
 
 
 zmhilonone + ggtitle("ZM Density vs Total Hg (Sandy/Rocky Sediment)") +
   xlab("ZM Density") + ylab("Total Hg (ng/g)")

#boxplot zm presence total

zm_total_box <- boxplot(NG.G.RESULT ~ zebra_mussels, data = lowdoc, 
                        xlab = "ZM Presence",
                        ylab = "Total Hg (ug/g)", 
                        main = "Total Mercury (Sandy/Rocky Sediment)",
                        notch = FALSE, 
                        varwidth = TRUE, 
                        col = c("purple"),
                        names = c("no", "yes")) 


#sand scatter zm density vs total
zm_v_total <- ggplot(lowdoc, aes(zm_density, NG.G.RESULT))
zm_v_total + geom_point (aes(), size= 3)

#muck box
hidoc <- total[-(which(total$bottom_substrate %in% "sand")),]
hidoc <- total[-(which(total$bottom_substrate %in% "rock")),]

zm_total_box <- boxplot(NG.G.RESULT ~ zm_density, data = hidoc, 
                        xlab = "ZM Density",
                        ylab = "Total Hg (ug/g)", 
                        main = "Total Mercury (Mucky Sediment)",
                        notch = FALSE, 
                        varwidth = TRUE, 
                        col = c("brown"),
                        names = c("0","1", "2", "3"))

#LOI

#loi plot
mendota_outline <- st_read("/Users/nick/Desktop/stripe_data/mendota_sf/805400.shp")

ggplot() +
  geom_sf(data = mendota_outline, linewidth = 0.5, col = "black")+
  geom_point(data= total, aes(x=lon, y=lat, color= loi ), size=3, alpha=0.5) + scale_color_gradient(low = "white", high="black", name = "% LOI ")+ 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(
    panel.background = element_rect(fill = "gray",
                                    colour = "gray",
                                    size = 0.5, linetype = "solid"))+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave("loi_map.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#adding thg/loi column
total["thg_div_loi"] <- total$NG.G.RESULT/total$loi
head(total)

#ZM score vs THg/LOI
zm_v_totalloi <- ggplot(total, aes(zm_density, thg_div_loi))
zm_v_totalloi + geom_point(aes())+
xlab("ZM Score")+
  ylab("THg/LOI (ng/g)")+
  geom_smooth(method=lm, formula = y ~ x)+
  stat_regline_equation(label.x=2, label.y=2)+
  stat_cor(aes(label=..rr.label..), label.x=2, label.y=1)

ggsave("thgdivloi.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#map thg/loi
ggplot() +
  geom_sf(data = mendota_outline, linewidth = 0.5, col = "black")+
  geom_point(data= total, aes(x=lon, y=lat, color= thg_div_loi ), size=3, alpha=0.5) + scale_color_gradient(low = "white", high="orange", name = "THg/LOI ")+ 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(
    panel.background = element_rect(fill = "gray",
                                    colour = "gray",
                                    size = 0.5, linetype = "solid"))+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("thg_div_loi_map.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')


#THg/loi bar plot yes or no zebra mussels
thg_loi_bp <- ggplot(total, aes(x=zm_yn, group= zm_yn, y=thg_div_loi)) 
thg_loi_bp + geom_boxplot()+
geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
ggtitle("ZMs and THg Normalized for LOI") +
  xlab("ZM Presence") + ylab("THg (ng/g)/LOI(%)")+
  geom_signif(comparisons = list(c("no", "yes")), 
              map_signif_level=TRUE)

ggsave("thgloi_zmyn.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#ThgLOI vs ZM MeHg
zm_v_total_loi <- ggplot(lowdoc, aes(thg_div_loi, ))
zm_v_total_loi + geom_point (aes(), size= 3)

#hi lo none bar plot THg LOI
loihilonone <- ggplot(total, aes(x=zebra_mussels, group=zebra_mussels, y=total$thg_div_loi)) +
  geom_boxplot(fill= "purple") +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) 
loihilonone + geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  ggtitle("ZMs and THg Normalized for LOI") +
  xlab("ZM Presence") + ylab("MeHg (ng/g)/LOI(%)")

#plant Thg

#merge plant and meta
merge.plant.meta <- merge(plant, meta ,by= "macrophyte_barcode_id" )

#plant total map
ggplot() +
  geom_sf(data = mendota_outline, linewidth = 0.5, col = "black")+
  geom_point(data= merge.plant.meta, aes(x=lon, y=lat, color= Plant.thg.ng.p.g ), size=3, alpha=0.5) + scale_color_gradient(low = "white", high="black", name = "Plant THg (ng/g) ")+ 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(
    panel.background = element_rect(fill = "gray",
                                    colour = "gray",
                                    size = 0.5, linetype = "solid"))+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave("plant_total_map.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#zm score v plant THg
zm.v.plant.thg <- ggplot(merge.plant.meta, aes(zm_density, Plant.thg.ng.p.g ))
zm.v.plant.thg + geom_point (aes(), size= 3)

ggsave("zm_score_v_plantthg.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#plant species THg barplot
p.species.bp <- ggplot(merge.plant.meta, aes(x=macrophyte_species, group=macrophyte_species, y=Plant.thg.ng.p.g)) +
  geom_boxplot(fill= "purple") +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) 
p.species.bp + geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  ggtitle("Plant Species and THg (ng/g)") +
  xlab("Plant Species") + ylab("THg (ng/g)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("plant_species_thg.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#EWM and Coontail THg barplot
ewmct <-filter(merge.plant.meta, macrophyte_species == 'Eurasian Milfoil' | macrophyte_species == 'Coontail'  )
               
ewmct.bp <- ggplot(ewmct, aes(x=macrophyte_species, group=macrophyte_species, y=Plant.thg.ng.p.g)) +
  geom_boxplot(fill= "purple") +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) 
ewmct.bp + geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  ggtitle("Plant Species and THg (ng/g)") +
  xlab("Plant Species") + ylab("THg (ng/g)")+
  geom_signif(comparisons = list(c("Coontail", "Eurasian Milfoil")), 
              map_signif_level=TRUE)

  
ggsave("plant_species_thg.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')




#native v non native plants THg
merge.plant.meta <- replace(merge.plant.meta, merge.plant.meta=='', NA)
p.native.erno <- merge.plant.meta[!is.na(merge.plant.meta$native_status),]

thg_nat <- ggplot(p.native.erno, aes(x=native_status, group=native_status, y=Plant.thg.ng.p.g)) +
  geom_boxplot(fill= "purple") +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) 
thg_nat + geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  stat_boxplot(geom = "errorbar",
               width = 0.5) +
  ggtitle("Native Status and Plant THg (ng/g)") +
  xlab("Native Status") + ylab("THg (ng/g)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_signif(comparisons = list(c("exotic", "native")), 
              map_signif_level=TRUE)



ggsave("native_status_thg.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#plant thg vs sed thg
plant_sed <- merge(total, plant ,by= "macrophyte_barcode_id" )

sedthg_v_plant <- ggplot(plant_sed, aes(NG.G.RESULT, Plant.thg.ng.p.g))
sedthg_v_plant + geom_point(aes())+
  xlab("Sed THg (ng/g)")+
  ylab("Plant THg (ng/g)")+
  geom_smooth(method=lm, formula = y ~ x)+
  stat_regline_equation(label.x=7, label.y=37)+
  stat_cor(aes(label=..rr.label..), label.x=7, label.y=32)

ggsave("thg_sed_plant.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#plant thg vs loi
loi_v_plant_thg <- ggplot(plant_sed, aes(loi, Plant.thg.ng.p.g))
loi_v_plant_thg + geom_point(aes())+
  xlab("% LOI")+
  ylab("Plant THg (ng/g)")+
  geom_smooth(method=lm, formula = y ~ x)+
  stat_regline_equation(label.x=7, label.y=37)+
  stat_cor(aes(label=..rr.label..), label.x=7, label.y=32)

#plant thg vs sed thg/loi

sed_v_plant <- ggplot(plant_sed, aes(thg_div_loi, Plant.thg.ng.p.g))
sed_v_plant + geom_point(aes())+
  xlab("Sed THg/LOI (ng/g/%loi)")+
  ylab("Plant THg (ng/g)")+
 geom_smooth(method=lm, formula = y ~ x)+
  stat_regline_equation(label.x=7, label.y=30)+
  stat_cor(aes(label=..rr.label..), label.x=7, label.y=25)

ggsave("native_status_thg.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#mehg v zm density
zm_v_sedmehg <- ggplot(sed_mehg_j, aes(zm_density,mehg_ng_g ))
zm_v_sedmehg + geom_point (aes(), size= 3)

# %mehg
sed_mehg_j["sed_percent_mehg"] <- (sed_mehg_j$mehg_ng_g/sed_mehg_j$NG.G.RESULT)*100

zm_v_sedmehg <- ggplot(sed_mehg_j, aes(zm_density, sed_percent_mehg ))
zm_v_sedmehg + geom_point (aes(), size= 3)+
  geom_smooth(method=lm, formula = y ~ x)+
  stat_regline_equation(label.x=3, label.y=1.5)+
  stat_cor(aes(label=..rr.label..), label.x=3, label.y=2.5)

# % sed mehg map
ggplot() +
  geom_sf(data = mendota_outline, linewidth = 0.5, col = "black")+
  geom_point(data= sed_mehg_j, aes(x=lon, y=lat, color= sed_percent_mehg), size=3, alpha=0.5) + scale_color_gradient(low = "white", high="red", name = "Sed % MeHg")+ 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(
    panel.background = element_rect(fill = "gray",
                                    colour = "gray",
                                    size = 0.5, linetype = "solid"))+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("sed_per.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')
