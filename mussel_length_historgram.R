#Mussel length histogram

library(ggplot2)
library(readr)
library(dplyr)
library(lattice)

getwd()
setwd("Desktop/stripe_2024/data")
lengths <- read_csv("lengths20240626.csv")

hist(lengths$`length(mm)`, breaks=20, 
     main="ZM Length Near the CFL, June 2024",
     xlab= "Length(mm)",
     ylab= "Count",
     col="red")

ggsave("length_hist.jpg", units="in", 
       width=5, height=4, dpi=300)

#boathouse
bh_lengths <- read_csv("zmdens_boathouse.csv")

histogram(bh_lengths$`length(mm)`, breaks=10, 
     main="Boathouse",
     type="density",
     xlab= "Length(mm)",
     ylab= "Relative Abundance",
    )
ggsave("bh_hist.jpg", units="in", 
       width=5, height=4, dpi=300)

#maple bluff

mb_lengths <- read_csv("zmdens_maplebluff.csv")

histogram(mb_lengths$`length(mm)`, breaks=10, 
          main="Maple Bluff",
          type="density",
          xlab= "Length(mm)",
          ylab= "Relative Abundance",
)
ggsave("mb_hist.tiff", units="in", 
       width=5, height=4, dpi=300)

#cfl dock

cfl_lengths <- read_csv("cfldensity.csv")

histogram(cfl_lengths$`length(mm)`, breaks=10, 
          main="CFL",
          type="density",
          xlab= "Length(mm)",
          ylab= "Relative Abundance",
)
ggsave("mb_hist.tiff", units="in", 
       width=5, height=4, dpi=300)

#govs island

gov_lengths <- read_csv("govsisland_density.csv")

histogram(gov_lengths$`length(mm)`, breaks=15, 
          main="Governor's Island",
          type="density",
          xlab= "Length(mm)",
          ylab= "Relative Abundance",
)

ggsave("mb_hist.tiff", units="in", 
       width=5, height=4, dpi=300)

#all zm (so far) dens

all_lengths <- read_csv("all_zm_dens.csv")

histogram(all_lengths$`length(mm)`, breaks=15, 
          main="Mendota",
          type="density",
          xlab= "Length(mm)",
          ylab= "Relative Abundance",
)
ggsave("all_hist.tiff", units="in", 
       width=5, height=4, dpi=300)

##map zm density

#shape file

sf <- ggplot()+ geom_sf(data = mendota_outline, linewidth = 1, col = "gray40")+ 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(panel.background = element_blank())

#map
dens <- data.frame(
  location = c("boathouse", "maple_bluff", "cfl_dock",
               "gov_island", "wally_bauman", "fox_bluff", "marshall_park"),
  lat = c(43.079468, 43.115111, 43.077493,
          43.122833, 43.089203, 43.109571, 43.094165),
  lon = c(-89.411975, -89.384826, -89.402896, 
          -89.402083, -89.441666, -89.458060, -89.482045 ),
  zm_dens = c(476, 449, 3619, 1444, 912, 0, 49  ))

sf + geom_point(data= dens, color= "red", 
                 aes(x=lon, y=lat, size=zm_dens))+ 
  labs(size = "Zebra Mussels/m²")

ggsave("map.tiff", units="in", 
       width=5, height=4, dpi=300)

#wally bauman

gov_lengths <- read_csv("wb_lengths.csv")

histogram(gov_lengths$`length(mm)`, breaks=12, 
          main="Wally Bauman Park",
          type="density",
          xlab= "Length(mm)",
          ylab= "Relative Abundance")

ggsave("wb.tiff", units="in", 
       width=5, height=4, dpi=300)

#marshall park

gov_lengths <- read_csv("marshall_lengths.csv")

histogram(gov_lengths$`length(mm)`, breaks=12, 
          main="Marshall Park",
          type="density",
          xlab= "Length(mm)",
          ylab= "Relative Abundance")

ggsave("mp.tiff", units="in", 
       width=7, height=4, dpi=300)

  