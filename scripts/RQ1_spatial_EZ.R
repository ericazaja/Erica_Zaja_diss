##%######################################################%##
#                                                          #
####            RQ1: SPATIAL ANALYSIS                   ####
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

## RQ1: What areas within the PCH Alaskan summer range have high-medium-low shrub biomass cover?

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
library(rspatial)
library(maptools)
library(rgeos)

## SHRUB DATA ----
# from Berner et al 2018

# Loading raster of shrub biomass (g/m2) on Alaskan North Slope  -----
shrub_agb_p50 <- raster("datasets/berner_data/shrub_agb_p50.tif") 
# Using the best-estimates: the 50th percentile of the 1,000 permutations

# Plotting shrub raster with base R
plot(shrub_agb_p50)
dev.off()

# Plotting shrub raster with ggplot
(gplot_shrub_agb_p50<- gplot(shrub_agb_p50) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  # value is the specific value (of reflectance) each pixel is associated with
  scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
    ifelse(x<1000, 
           scales::rescale(x,
                           to = to,
                           from = c(min(x, na.rm = TRUE), 1000)),
           1)}) +
  coord_quickmap()+
  theme_classic() +  # Remove ugly grey background
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Shrub biomass cover (g/m2) of Alaskan north slope") +
  theme(plot.title = element_text(hjust = 0.5),     # centres plot title
        text = element_text(size=15),		       	    # font size
        axis.text.x = element_text(angle = 45, hjust = 1)))  # rotates x axis text

dev.off()


### PCH RANGE DATA -----
# PCH core range data from Porcupine Caribou Management Board 2016

# https://gis.stackexchange.com/questions/34310/opening-lyr-file-via-rgdal-ogr
# https://rspatial.org/raster/rosu/Chapter11.html 
## look into SpTransform



PCH_core_range <- st_read("datasets/PCH_Core_Range_2016/PCH_Core_Range_2016.shp") #loading data

# checking PCH range and shrub hve same projection
projection(PCH_core_range)
projection(shrub_agb_p2_5)
# same projection

## NB. PCH range is a POLYGON VS Shrub map is raster data

(PCH_range_map <- ggplot() + 
  geom_sf(data = PCH_core_range, size = 0.5, color = "black", fill = "green") + 
  ggtitle("PCH core range 2016")) ## doesnt work anymore?

# Cropping shrub map to range -----
cropped <- crop(shrub_agb_p2_5, PCH_core_range)
plot(cropped,col = pal(3))
pal <- colorRampPalette(c("tan","green", "green4"))


plot(shrub_agb_p2_5)
plot(PCH_core_range, add = TRUE) # adds range onto shrub map

# Cropped map visualisation - with viridis colour ----
(cropped_viridis <- gplot(cropped) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis_c(limits = c(0, 1400), oob = scales::squish) +
    coord_quickmap()+
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Shrub cover of the PCH alaskan range") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 45, hjust = 1)))  # rotates x axis text

(cropped_viridis <- gplot(cropped) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<700, 
             scales::rescale(x,
                             to = to,
                             from = c(min(x, na.rm = TRUE), 700)),
             1)}) +
    coord_quickmap()+
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Shrub cover of the PCH alaskan range") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 45, hjust = 1)))  # rotates x axis text

(cropped_new <- gplot(cropped) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_gradient(low = "tan", high = "green", 
                        rescaler = function(x, to = c(0, 1), from = NULL) {
                          ifelse(x<900, scales::rescale(x,
                             to = to,
                             from = c(min(x, na.rm = TRUE), 900)),
             1)}) +
    coord_quickmap()+
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Shrub cover of the PCH alaskan range") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 45, hjust = 1)))  # rotates x axis text


#e <- extract(shrub_agb_p2_5, PCH_core_range)
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
#### Making a  basemap
(Alaska_Yukon <- ggplot() +
    borders("world", colour = "black", fill = "white", size = 0.3) + 
    coord_cartesian(xlim = c(-180, -80), ylim = c(60, 75)) +
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") )

plot(shrub_agb_p2_5, add = TRUE)
plot(PCH_core_range, add = TRUE) # adds range onto shrub map
dev.off()

### HOW DO I OVERLAY A RASTER layer WITH A spatial POLYGON? -----
# Cropping rasters 
plot(shrub_agb_p2_5)
zoom(shrub_agb_p2_5) ## define square 

# Comparing rasters to see if they have the same extent, number of rows and column, projection, resolution and origin
# compareRaster(shrub_agb_p2_5, shrub_agb_p50,shrub_agb_p97_5, shrub_dominance_of_agb_p50)
# TRUE

# Stacking rasters (??)
# Create a stack of all the rasters, so just putting them all on top of each other.
# t <- stack(shrub_agb_p50, shrub_agb_p2_5)

# To visualise all the bands together, we can use facet_wrap in gplot. 
# gplot(t) +
#geom_raster(aes(x = x, y = y, fill = value))+
#scale_fill_viridis_c() +
#facet_wrap(~variable) +
#coord_quickmap()+
#ggtitle("Vegetation cover p2, raster plots") +
#xlab("Longitude") +
#ylab("Latitude") +
#theme_classic() +
#theme(text = element_text(size=20),
#     axis.text.x = element_text(angle = 90, hjust = 1)) +
#theme(plot.title = element_text(hjust = 0.5))
