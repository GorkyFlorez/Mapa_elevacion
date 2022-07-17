
#  Cargamos las Librerias ----------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(raster)
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(ggthemes)
library(elevatr)
library(ggspatial)
library(tmap)
library(ggpubr)
library(ggrepel)
library(ggforce)


SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric <- st_transform(SurAmerica  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Peru          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()

Per           <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
MDD           <- subset(Per, NAME_1  == "Madre de Dios")

Dist_MDD = st_read("SHP/MDD.geojson")  %>% st_as_sf()
Dist_MD  <- st_transform(Dist_MDD ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Inambari <- subset(Dist_MD, distrito_ == "Inambari")

Kotsimba = st_read("SHP/Kotsimba.geojson")  %>% st_as_sf()
Kotsimb  <- st_transform(Kotsimba,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Kotsimb_xy     <- cbind(Kotsimb , st_coordinates(st_centroid(Kotsimb $geometry)))

elev = get_elev_raster(MDD , z=9)
plot(elev)
Poligo_alt    <- crop(elev, MDD)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, MDD)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)
q4 <- c("#2dc653", "#b5e48c", "#99d98c", "#086375", "#52b69a", "#34a0a4", 
        "#168aad", "#8ea604", "#bc3908", "#10451d")

Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

MDD_xy     <- cbind(MDD , st_coordinates(st_centroid(MDD$geometry)))

d <- data.frame(longitude = c(-70.33333 ),latitude = c(-11.14725),
                name= c("Madre de Dios"))

SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru , fill="gray", color="black")+
  geom_sf(data = MDD, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1, 
           label = "a )South America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1, 
           label = "Atlantic ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10, hjust = 0, vjust = 1, 
           label = "Peru",size = 3, family="serif", color = 
             "black",  fontface="italic")
SurA

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")
library(ggnewscale)

ggplot()+
  geom_sf(data = MDD, fill=NA, color="black")
  

MDD_GG=ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(0,800,1500,2500,6000),
                       na.value = 'white',
                       labels = c("[0 - 799] ","[800 - 1499]", "[1500 - 2499]",
                                  "[2500 - 2999]", "[3000 - 6000]"),
                       name='Elevacion \n(msnm)') +
  geom_sf(data = MDD, fill=NA, color="black")+
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.01)+
  geom_sf(data = Peru , fill=NA, color="black", size=0.01)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf(data = MDD, fill=NA, color="black")+
  geom_sf(data = Kotsimba , fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  coord_sf(xlim = c(-72.40404, -68.65311), ylim = c(-13.7 ,-9.879849)) +
  annotate(geom = "text", x = -70.5, y = -10, hjust = 0, vjust = 1, 
           label = "b) Departamento de \nEstudio",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -71, y = -13.4, hjust = 0, vjust = 1, 
           label = "Madre de Dios",size = 4, family="serif", color = 
             "black",  fontface="italic")



ele = get_elev_raster(Inambari , z=12)
plot(ele)
Poligo_al    <- crop(ele, Kotsimb)                           #   
Poligo_al   <- Poligo_al <- mask(Poligo_al, Kotsimb)
plot(Poligo_al)

slop    = terrain(ele  , opt = "slope") 
aspec    = terrain(ele, opt = "aspect")
hil     = hillShade(slop, aspec, angle = 40, direction = 270)

hil      <-  rasterToPoints(hil)
hi       <-  data.frame(hil)

slope    = terrain(Poligo_al  , opt = "slope") 
aspect    = terrain(Poligo_al, opt = "aspect")
hill     = hillShade(slope, aspect, angle = 40, direction = 270)

hill       <-  rasterToPoints(hill)
hill.p      <-  data.frame(hill)

Geo_dat       <-  rasterToPoints(Poligo_al)
Geo_data_fram <-  data.frame(Geo_dat)
colnames(Geo_data_fram) <- c("x","y", "alt")


Kosim_ele =ggplot()+
  geom_raster(data = hi, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = hill.p, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_fram  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(0,800,1500,2500,6000),
                       na.value = 'white',
                       labels = c("[0 - 799] ","[800 - 1499]", "[1500 - 2499]",
                                  "[2500 - 2999]", "[3000 - 6000]"),
                       name='Elevacion \n(msnm)') +
  
  geom_sf(data = Kotsimb , fill=NA, color="red")+
  coord_sf(xlim = c(-70.45,  -70.00), ylim = c(-13.174,-12.97678)) +
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face="bold", color="black"),
    legend.key.width = unit(3, 'line'),
    panel.border = element_rect(size = 1, color="black"),
    axis.text.x  = element_text(face="bold", color="black", size=8),
    axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
    strip.text=element_text(family='Anton', face='bold', size=14, hjust=0, color='white'),
    strip.background=element_rect(fill='black'))+
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="black"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"),
                              text_family = "ArcherPro Book", text_col="black")+
  annotate(geom = "text", x = -70.05, y = -13, hjust = 0, vjust = 1, 
           label = "d) Area de estudio ",size = 4, family="serif", color = 
             "black",  fontface="italic")+
  labs(title = '', fill = '',  x = '', y = '') 


  library(openxlsx)
  Data <- read.xlsx("Excel/Data.xlsx", sheet="Hoja1")
  Data[1,1] <- "La comunidad Nativa de Kotsimba esta \nubicado en el sur este de la Amazonia peruana \nlimita al sur la región de Puno, al oeste con \nHuepetuhe y al norte con el distrito de \nMadre de Dios y Laberinto, internacionalmente \ncon Bolivia y Brasil"
  
  colnames(Data ) <- c("Mapa de elevacion de la comunidad\nNativa de Kotsimba")
  Tabla.p <- ggtexttable(Data, rows = NULL, theme = ttheme("blank", base_size  =  8))
  
  Table=Tabla.p %>%
    table_cell_font(row = 2:tab_nrow(Tabla.p), column = 1, face = "italic", size= 8)
  
  Table    
# Mapa final
library(cowplot)
Final =ggdraw() +
  coord_equal(xlim = c(0, 22.86), ylim = c(0, 17.78), expand = FALSE) +
  draw_plot(Kosim_ele, width = 23, height = 23,x = -0.5, y = -6)+
  draw_plot(Table , width = 7, height = 7,x = 15, y = 10.8)+
  draw_plot(SurA, width = 7, height = 7,x = -1.3, y = 10.8)+
  draw_plot(MDD_GG, width = 7, height = 7,x = 4.3, y = 10.8)+
  
  theme(panel.background = element_rect(fill = "white"))


ggsave(plot=Final,"Mapa/Mapa de Ubicacion Kotsimba1.png",units = "cm",width = 22.86, #alto
       height = 17.78, #ancho
       dpi=1200)
 