



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

#data frame new sites
newsites <- data.frame(
  sitename = c("A", "B", "C" ),
  lat = c(43.07912, 43.0775215, 43.07889),
  lon = c(-89.41382, -89.4029731, -89.39384 ))

#labeling newsites
nsp <- sf+  geom_point(
  data= newsites, 
  aes(x=lon, y=lat),
  size=4, alpha=100,
  stroke= 1,
  shape = 1, color="red")+ geom_label(
    size=3,
    nudge_x = 0,
    nudge_y = 0,
    angle= 0,
    data= newsites, 
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

nsp
ggsave("siteplan24.tiff", units="in", 
       width=5, height=4, dpi=300, compression = 'lzw')

#less sites
less <- data.frame(
  sitename = c("CFL" ),
  lat = c(43.0775215),
  lon = c(-89.4029731))

lsp <- sf+  geom_point(
  data= less, 
  aes(x=lon, y=lat),
  size=4, alpha=100,
  stroke= 1,
  shape = 1, color="red")+ geom_label(
    size=3,
    nudge_x = 0,
    nudge_y = 0,
    angle= 0,
    data= less, 
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

lsp

ggsave("lessismore.jpg", units="in", 
       width=5, height=4, dpi=300)
