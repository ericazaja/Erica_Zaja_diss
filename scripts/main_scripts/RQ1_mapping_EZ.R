##%######################################################%##
#                                                          #
###                   MAPPING SCRIPT                    ####
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

### PART 0: MAPPING
## RQ1: How is shrub biomass distributed in the focal study area?

## LOADING LIBRARIES -----

library(sp)
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

# THEME ----
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

# 1. FULL Shrub map ----
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
    ggtitle("Shrub biomass cover (g/m2) of Alaskan north slope\n") +
    theme(plot.title = element_text(hjust = 0.5),     # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 0, hjust = 1)))  # rotates x axis text

# ggsave("output/figures/gplot_shrub_agb_p50.png")

# 2. FULL PCH range ----
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


# 5. Cropped map ----
# Loading cropped map
r3_latlong_agg <- raster("datasets/berner_data/r3_latlong_agg.tif")

# Cropped map with viridis palette
(r3_cropped_viridis <- gplot(r3_latlong_agg) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<267, scales::rescale(x, to = to, from = c(min(x, na.rm = TRUE), 267)),1)}, na.value="white", name = "Biomass (g/m2)") +
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

# ggsave("output/figures/r3_cropped_viridis.png")

# Plotting cropped map with new color palette
# making raster into dataframe
r3_latlong_agg_df <- as.data.frame(r3_latlong_agg, xy=TRUE, 
                                   na.rm=TRUE)

# setting biomass level thresholds using quantiles (as per RQ1_models_EZ.R script)
r3_latlong_agg_df <- r3_latlong_agg_df %>%
  mutate(biomass_level = case_when (r3_latlong_agg < 170.630482     ~ 'Low', # lower than mean biomass
                                    r3_latlong_agg> 170.630482    & r3_latlong_agg < 347.062210 ~ 'Medium', 
                                    r3_latlong_agg > 347.062210 ~ 'High'))

# ordering factor levels
r3_latlong_agg_df$biomass_level <- factor(r3_latlong_agg_df$biomass_level,levels=c("Low", "Medium", "High"),
                                         labels = c("Low", "Medium", "High"),
                                         ordered = T)

# plotting raster with personalised colours
(raster_my_palette_new <- ggplot(r3_latlong_agg_df) + 
  geom_tile(aes(x=x,y=y,fill=biomass_level)) + 
  scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
  coord_quickmap()+
  theme_shrub() +  
  xlab("\nLongitude") +
  ylab("Latitude\n") +
 xlim(-146.5, -141)+
 ylim(69.2,70.2)+ 
  theme(plot.title = element_text(hjust = 0.5),      # centres plot title
                       text = element_text(size=25),	
        axis.title.x =element_text(size=25),
        axis.title.y =element_text(size=25),
        axis.text.x = element_text(size=25, hjust = 1),
                       axis.text.y = element_text(size=25, hjust = 1),
                       legend.text = element_text(size=20),
                       legend.title = element_text(size=30),
  legend.position ="bottom"))

# adding logo
caribou_logo <- readPNG("caribou_icon.png")
raster_caribou_logo <- as.raster(caribou_logo)
(raster_my_palette_new <- raster_my_palette_new + annotation_raster(raster_caribou_logo, -142, -140, 69.8, 70.5))
ggsave(file = "output/figures/raster_my_palette_new.png")



######################################################## END -----

