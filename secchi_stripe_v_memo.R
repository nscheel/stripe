#loading packages
library(ggplot2)
library(lubridate)

#read in deep hole and near shore data
ns <- read.csv("/Users/nick/Desktop/stripe_data/STRIPE_Secchi.csv")
dh <- read.csv("/Users/nick/Desktop/stripe_data/deep_hole_secchis.csv")
zmvno <- read.csv("/Users/nick/Desktop/stripe_data/zmvno.csv")

#clean

#dh$Date <- make_date(year = dh$Year, month = dh$Month, day = dh$Day)

#joined <- merge(ns,dh ,by= "Date" )
#joined

#secchi depths nearshore vs deep hole

clarity <- ggplot(ns, aes(Date, secchi_m))
clarity + geom_point(aes(colour = factor(shore)))

location <- boxplot(secchi_m ~ shore, data = ns, 
                  xlab = "Location",
                  ylab = "Secchi Depth (m)", 
                  main = "Water Clarity",
                  notch = FALSE, 
                  varwidth = TRUE, 
                  col = c("green","yellow"),
                  names = c("Deep Hole","Near Shore"))


#secchi depths zebra mussels vs no zebra mussels

clarity + geom_point(aes(colour = factor(zebra_mussels), size= 3), shape=as.factor(ns$shore))

#secchi nearshore divided by deep hole

yesvno <- ggplot(zmvno, aes(Date, nearshore_secchi / deep_hole_secchi))
yesvno + geom_point(aes(colour = factor(zm)))

div <- zmvno$nearshore_secchi / zmvno$deep_hole_secchi

plot(yesvno ~ zm, data=(nearshore_secchi/deep_hole_secchi))

x_div <- as.numeric(unlist(div))
class(x_div)

#boxplot
div <- gsub(",", "", div)  
div <- as.numeric(div)  

presence_absence <- boxplot(div ~ zm, data = zmvno, 
        xlab = "ZM presence",
        ylab = "Corrected Water Clarity", 
        main = "Water Clarity",
        notch = FALSE, 
        varwidth = TRUE, 
        col = c("green","yellow"),
        names = c("Absent","Present"))

wc_hi_low <- boxplot(div ~ zm_hilo, data = zmvno, 
                  xlab = "ZM Concentration",
                  ylab = "Corrected Water Clarity", 
                  main = "Water Clarity",
                  notch = FALSE, 
                  varwidth = TRUE, 
                  col = c("green","yellow"),
                  names = c("High","Low"))




 
