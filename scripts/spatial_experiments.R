##%######################################################%##
#                                                          #
####         SPATIAL ANALYSIS EXPERIMENTS -----         ####
#               Erica Zaja - 09/10/2021                   #
#                                                         #
##%######################################################%##

# Loading libraries -----
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)
library(sf)
library(tidyverse)
library(ggmap)

# Loading data -----
berner_dataset <- read_csv("datasets/berner_data/berner_dataset.csv")
shrub_agb_p2_5 <- raster("datasets/berner_data/shrub_agb_p2_5.tif")

shrub_dominance_of_agb_p50 <- raster("datasets/berner_data/shrub_dominance_of_agb_p50.tif")

### N.B.same plot=same values in raster info

# Comparing rasters to see if they have the same extent, number of rows and column, projection, resolution and origin
compareRaster(shrub_agb_p2_5, plant_agb_p50)
# TRUE

# Plotting rasters -----
plot(shrub_agb_p2_5)
plot(plant_agb_p50)
plot(shrub_dominance_of_agb_p50)
dev.off()

# Cropping rasters ----- 
plot(shrub_agb_p2_5)
zoom(shrub_agb_p2_5) ## define square 

# Using ggplot ----
# Shrub cover p2_5
gplot_shrub_agb_p2_5 <- gplot(shrub_agb_p2_5) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  # value is the specific value (of reflectance) each pixel is associated with
  scale_fill_viridis_c() +
  coord_quickmap()+
  theme_classic() +  # Remove ugly grey background
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Shrub cover of the north slope of Alaska, raster plot") +
  theme(plot.title = element_text(hjust = 0.5),             # centres plot title
        text = element_text(size=20),		       	    # font size
        axis.text.x = element_text(angle = 90, hjust = 1))  # rotates x axis text

dev.off()
# ggsave("gplot_shrub_agb_p2_5", scale = 1.5, dpi = 300) 		# to save plot

# Plant cover p2_5
gplot_plant_agb_p2_5 <- gplot(plant_agb_p50) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  # value is the specific value (of reflectance) each pixel is associated with
  scale_fill_viridis_c() +
  coord_quickmap() +
  ggtitle("Plant cover of the north slope of Alaska, raster plot") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +   					    # removes defalut grey background
  theme(plot.title = element_text(hjust = 0.5),             # centres plot title
        text = element_text(size=20),		       	    # font size
        axis.text.x = element_text(angle = 90, hjust = 1))  # rotates x axis text

# ggsave("gplot_plant_agb_p2_5", scale = 1.5, dpi = 300) 		# to save plot

# Stacking rasters -----
# Create a stack of all the rasters, so just putting them all on top of each other.
t <- stack(plant_agb_p2_5, shrub_agb_p2_5)

# To visualise all the bands together, we can use facet_wrap in gplot. 
gplot(t) +
  geom_raster(aes(x = x, y = y, fill = value))+
  scale_fill_viridis_c() +
  facet_wrap(~variable) +
  coord_quickmap()+
  ggtitle("Vegetation cover p2, raster plots") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

### PCH summer range -----
# https://gis.stackexchange.com/questions/34310/opening-lyr-file-via-rgdal-ogr

### HOW DO I OVERLAY A RASTER layer WITH A spatial POLYGON? -----
# https://rspatial.org/raster/rosu/Chapter11.html 
## look into SpTransform
library(rspatial)
library(maptools)
library(rgeos)


#### PCH core range ----
PCH_core_range <- st_read("datasets/PCH_Core_Range_2016/PCH_Core_Range_2016.shp") #loading data
projection(PCH_core_range)
projection(shrub_agb_p2_5)
# same projection

ggplot() + 
  geom_sf(data = PCH_core_range, size = 0.5, color = "black", fill = "green") + 
  ggtitle("PCH core range 2016") + 
  coord_sf()

# Cropping plant map to range -----
cropped <- crop(shrub_agb_p2_5, PCH_core_range)
plot(cropped)

plot(shrub_agb_p2_5)
plot(PCH_core_range, add = TRUE)
e <- extract(shrub_agb_p2_5, PCH_core_range)

cropped <- crop(shrub_agb_p2_5, PCH_core_range)
plot(cropped)

zoom()

dev.off()

## Basemap of north america
library(rworldmap)
library(rnaturalworld)
world <- getMap(resolution = "low")


## Specify the required projection using a proj4 string
## Use http://www.spatialreference.org/ to find the required string
## Polyconic for North America
newProj <- CRS("+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

## Project the map extent (first need to specify that it is longlat) 
mapExtentPr <- spTransform(SpatialPoints(mapExtent, 
                                         proj4string=CRS("+proj=longlat")),
                           newProj)

#### Making a  basemap
(Alaska_Yukon <- ggplot(shrub_agb_p2_5) +
    borders("world", colour = "black", fill = "white", size = 0.3) + 
    coord_cartesian(xlim = c(-180, -80), ylim = c(60, 75)) +
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") )

