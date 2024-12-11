#shape file with inlets labeled

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

#data frame inlets
flow <- data.frame(
  sitename = c("S", "W", "P", "Y", "D"),
  lat = c(43.08217, 43.078153, 43.105078, 43.146188, 43.140435),
  lon = c(-89.468179, -89.421352, -89.482363, -89.408355, -89.424906))

#labeling inlets
ip <- sf+  geom_point(
  data= flow, 
  aes(x=lon, y=lat),
  size=3, alpha=100,
  stroke= 1,
  shape = 2, color="black")

#shape file with inlets
sfi <- ip +   geom_text(
  nudge_x = -.0085,
  nudge_y = .003,
  data= flow, 
  aes(
    x= lon, 
    y=lat, 
    label=sitename,
  ))+
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

sfi
#zm score
zmd <- sfi + 
  geom_point(data=sites, aes(
    x=lon, y=lat, 
    color= zm_density), 
    size=2, alpha=5) + 
  scale_color_gradient(low = "white", high="red", name = "ZM Score")
zmd
ggsave("zm_score_sf.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#bottom substrate
bs <- sfi + 
  geom_point(
  data= meta, 
  aes(x=lon, y=lat, color= bottom_substrate),
  size=2, alpha=5)+
  scale_colour_manual(values = c("mediumorchid","red","goldenrod1"), 
                      name="Bottom Substrate")
bs
ggsave("substrate.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#zm thg
zmthg <- sfi + geom_point(data= zm_tmeper_joined,
           aes(x=lon, y=lat, color= thg_ug_g),
           size=2.5, alpha=5) + 
  scale_color_gradient(low = "plum1", 
                       high="red", name = "ZM THg (ng/g)")+
   geom_text(size=2,
    nudge_x = 0,
    nudge_y = 0,
    angle= 90,
    data= zm_mehgjoined, 
    aes(
      x= lon, 
      y= lat, 
      label= Sample.Date,
    ))
zmthg
ggsave("thg_zm_map.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#zm mehg
sfi + geom_point(data= zm_mehgjoined,
           aes(x=lon, y=lat, color= mehg.ngperg),
           size=3, alpha=5) + 
  scale_color_gradient(low = "plum1", high="red", 
                       name = "ZM MeHg (ng/g)")
ggsave("mehg_zm_map.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#zm %mehg
sfi + geom_point(data= zm_tmeper_joined, 
                 aes(x=lon, y=lat, color= percent_mehg),
                 size=3, alpha=5) + scale_color_gradient(
                   low = "white", high="red", name = "ZM % MeHg (ng/g)")

ggsave("p_mehg_zm_map.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')
 
#zm
ggplot() +
  geom_sf(data = mendota_outline, linewidth = 0.5, col = "black")+
  geom_point(data= zm_mehgjoined, aes(x=lon, y=lat, color= mehg.ngperg), size=3, alpha=5) + scale_color_gradient(low = "white", high="orange", name = "ZM MeHg (ng/g)")+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) +
  theme(
    panel.background = element_rect(fill = "lightgray",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"))+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#thg
ggplot() +
  geom_sf(data = mendota_outline, linewidth = 0.5, col = "black")+
  geom_point(data= zm_mehgjoined, aes(x=lon, y=lat, 
  color= mehg.ngperg), size=3, alpha=5) + 
  scale_color_gradient(low = "white", high="orange", 
                       name = "ZM MeHg (ng/g)")+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) +
  theme(
    panel.background = element_rect(fill = "lightgray",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"))+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

