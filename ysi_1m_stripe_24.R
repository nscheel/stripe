
library(ggplot2)
library(readr)
library(dplyr)
library(lattice)
library(lubridate)

#reading data in
setwd("Desktop/stripe_2024/data")
ysi <- read_csv("stripe_ysi_24_1m.csv")

# temperature vs date
ysi$Date <- as.Date(mdy(ysi$Date))


temp_v_date <- ggplot(ysi, aes(Date, Temp.C)) +
  geom_line(data= ysi, aes(x=Date , y= Temp.C ))+
geom_point(data= ysi, aes(x=Date , y= Temp.C ))+
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1) +
labs(title = "Water Temp at 1m", 
     x = "Date", y = "Temperature")
  
temp_v_date

ggsave("temp_v_date.tiff", units="in", width=5, 
       height=4, dpi=300, compression = 'lzw')

#DO v date

DO_v_date <- ggplot(ysi, aes(Date, DO.percent)) +
  geom_point(data= ysi, aes(x=Date , y= DO.percent))+
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "%DO at 1m", 
       x = "Date", y = "% DO")

DO_v_date

ggsave("DO_v_date.tiff", units="in", width=5, 
       height=4, dpi=300, compression = 'lzw')

#DO mg.L 
DOmgl_v_date <- ggplot(ysi, aes(Date, DO.mg.L)) +
  geom_point(data= ysi, aes(x=Date , y= DO.mg.L))+
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "DO.mg.L at 1m", 
       x = "Date", y = "DO.mg.L")

DOmgl_v_date

ggsave("DOmgl_v_date.tiff", units="in", width=5, 
       height=4, dpi=300, compression = 'lzw')

#pH v Date
pH_v_date <- ggplot(ysi, aes(Date, pH)) +
  geom_point(data= ysi, aes(x=Date , y= pH))+
  geom_line(data= ysi, aes(x=Date , y= pH))+
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "pH at 1m", 
       x = "Date", y = "pH")

pH_v_date

ggsave("pH_v_date.tiff", units="in", width=5, 
       height=4, dpi=300, compression = 'lzw')

#SPC v Date
spc_v_date <- ggplot(ysi, aes(Date, SPC.uS.cm)) +
  geom_point(data= ysi, aes(x=Date , y= SPC.uS.cm))+
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "SPC at 1m", 
       x = "Date", y = "SPC.uS.cm")


spc_v_date

ggsave("pH_v_date.tiff", units="in", width=5, 
       height=4, dpi=300, compression = 'lzw')



 