##%######################################################%##
#                                                          #
###                   MAPPING SCRIPT                    ####
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

## LOADING LIBRARIES -----

library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)
library(sf)
library(tidyverse)
library(maptools)
library(rgeos)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggmap)
library(tmap)
library(tmaptools)

## LOADING DATA ----

shrub_agb_p50 <- raster("datasets/berner_data/shrub_agb_p50.tif") # shrub data (from Berner et al 2018)
PCH_core_range <- st_read("datasets/PCH_Core_Range_2016/PCH_Core_Range_2016.shp") # PCH range data 

## EXPLORING DATA ------

# class: what type of data 
class(shrub_agb_p50) # RasterLayer
class(PCH_core_range) # sf dataframe

# resolution of shrub map 
res(shrub_agb_p50) # resolution of map: [1] 30m x 30m

# extent of range polygon
st_bbox(PCH_core_range) 
# xmin      ymin      xmax      ymax 
# 165444.3 1697872.7  849222.0 2270606.5 

# extent of shrub map
st_bbox(shrub_agb_p50) 
#  xmin      ymin      xmax      ymax 
# -540475.3 1933928.1  521674.7 2380628.1 

# projection
projection(PCH_core_range)
projection(shrub_agb_p50)
# same projection

# coordinate ref system (CRS)
crs(PCH_core_range) 
crs(shrub_agb_p50) 
# same CRS: +proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 

## DATA VISUALISATION -----

# setting a personalised theme 
theme_shrub <- function(){ theme(legend.position = "right",
                                 axis.title.x = element_text(face="bold", size=20),
                                 axis.text.x  = element_text(vjust=0.5, size=18, colour = "black"), 
                                 axis.title.y = element_text(face="bold", size=20),
                                 axis.text.y  = element_text(vjust=0.5, size=18, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}

# 1. Shrub map ----
# Plotting shrub raster (entire) with ggplot
(gplot_shrub_agb_p50 <- gplot(shrub_agb_p50) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<1000, scales::rescale(x, to = to, from = c(min(x, na.rm = TRUE), 1000)),1)}, na.value="white") +
    coord_quickmap()+
    theme_shrub() +  # Remove ugly grey background
    xlab("\nLongitude") +
    ylab("Latitude\n") +
    ggtitle("Shrub biomass cover (kg/m2) of Alaskan north slope\n") +
    theme(plot.title = element_text(hjust = 0.5),     # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 0, hjust = 1)))  # rotates x axis text

# ggsave("output/figures/gplot_shrub_agb_p50.png")

# 2. PCH range ----
# Plotting PCH core range (entire) using ggplot
(PCH_range_map <- ggplot() + 
    geom_sf(data = PCH_core_range, size = 0.5, color = "black", fill = "white") + 
    theme_shrub()+
    ggtitle("PCH core range 2016")) 

# ggsave("output/figures/PCH_range_map.png")

# plotting PCH geometry only 
plot(PCH_core_range)
plot(PCH_core_range[, "Id"], key.width = lcm(5), key.pos = 4)
plot(st_geometry(PCH_core_range))

# 3. ***Map overlay*** ----
plot(shrub_agb_p50, xlim = c(-540475.3,  921674.7), ylim = c(1933928.1,2380628.1)) # plotting shrub raster
plot(st_geometry(PCH_core_range), xlim = c(165444.3, 1049222.0), add = TRUE) # adding PCH polygon

# 4. ***Base map*** ----
# Full map of canada
canada <- raster::getData("GADM", country = "CAN", level = 1)
plot(canada)
yukon <- canada[canada$NAME_1 != "Yukon"] # keeping Yukon

# getting data from world 
world <- ne_countries(scale = "medium", returnclass = "sf") 
class(world)

# making a datafrme of coordinates of Alaska/Yukon
Alaska_coords <- data.frame(longitude = c(-180, -80), latitude = c(60, 75)) 
Alaska_polygon <- as(Alaska_coords, 'SpatialPolygonsDataFrame') # making extent into polygon

# plotting base map 
(Alaska_Yukon <- ggplot(data = Alaska_polygon) +
    geom_point(aes(x = longitude, y = latitude)) +
  coord_sf()) # wrong 

(Alaska_Yukon <- ggplot() +
   geom_polygon(data = world, aes(x = long, y = lat, group = group),
                fill = "grey", colour = "black") + 
   coord_cartesian(xlim = c(-180, -80), ylim = c(60, 75)) +
   geom_sf(data = PCH_core_range,
           fill = "yellow", colour = "yellow") + 
   theme_shrub() +  
   xlab("Longitude") +
   ylab("Latitude") ) ## DOESNT WORK

# 5. Cropped map ----
# Loading cropped map
r3_latlong_agg <- raster("datasets/berner_data/r3_latlong_agg.tif")

# Cropped map with viridis palette
(r3_cropped_viridis <- gplot(r3_latlong_agg) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<267, scales::rescale(x, to = to, from = c(min(x, na.rm = TRUE), 267)),1)}, na.value="white", name = "Biomass (kg/m2)") +
    coord_quickmap()+
    theme_shrub() +  # Remove ugly grey background
    xlab("\nLongitude") +
    ylab("Latitude\n") +
    xlim(-147.5, -140)+
    ylim(69,70.5)+
    # ggtitle("Shrub biomass cover (kg/m2) of the PCH alaskan range\n") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 30, hjust = 1)))  # rotates x axis text

ggsave("output/figures/r3_cropped_viridis.png")

# Cropped map with personalised colour palette (low-mid)
(r3_cropped_my_palette <- gplot(r3_latlong_agg) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_gradient(low = "#F0E442", high = "#009E73", 
                        rescaler = function(x, to = c(0, 1), from = NULL) {
                          ifelse(x<347, scales::rescale(x, to = to,from = c(min(x, na.rm = TRUE), 347)),
                                 1)}, na.value="white", name = "Shrub biomass (g/m^2)") +
    coord_quickmap()+
    theme_shrub() +  # Remove ugly grey background
    xlab("\nLongitude") +
    ylab("Latitude\n") +
    xlim(-147.5, -140)+
    ylim(69,70.5)+
    theme(plot.title = element_text(hjust = 0.5),      # centres plot title
          text = element_text(size=20),		       	    # font size
          axis.text.x = element_text(angle = 30, hjust = 1),
          legend.text = element_text(size=20),
          legend.title = element_text(size=25)))  # rotates x axis text

ggsave("output/figures/r3_cropped_my_palette.png")

# adding logo
caribou_logo <- readPNG("caribou_icon.png")
raster_caribou_logo <- as.raster(caribou_logo)
(r3_cropped_my_palette <- r3_cropped_my_palette + annotation_raster(raster_caribou_logo, -142, -140, 69.8, 70.5))
ggsave(file = "output/figures/r3_cropped_my_palette.png")




# Cropped map with personalised colour palette (low-mid-high) 
(r3_cropped_my_palette_2 <- gplot(r3_latlong_agg) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_gradient2(low = "tan", mid = "#FFFF6B", high = "green4", midpoint = 267,  na.value="white", name = "Biomass (kg/m2)") +
    coord_quickmap()+
    theme_shrub() +  
    xlab("\nLongitude") +
    ylab("Latitude\n") +
    xlim(-147.5, -140)+
    ylim(69,70.5)+
    # ggtitle("Shrub biomass cover (kg/m2) of the PCH alaskan range\n") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 30, hjust = 1)))  # rotates x axis text

ggsave("output/figures/r3_cropped_my_palette_2.png")

dev.off()

# END -----

