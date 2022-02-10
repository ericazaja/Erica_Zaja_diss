##%######################################################%##
#                                                          #
###              RQ1: SPATIAL ANALYSIS                  ####
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

## RQ1: What areas within the PCH Alaskan summer range have high-medium-low shrub biomass cover?

# LOADING LIBRARIES -----
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)
library(sf)
library(tidyverse)
library(ggmap)
library(maptools)
library(rgeos)
library(rworldmap)


### LOADING DATA -----

## SHRUB DATA: from Berner et al 2018
# Loading raster of shrub biomass (g/m2) on Alaskan North Slope  -----
shrub_agb_p50 <- raster("datasets/berner_data/shrub_agb_p50.tif") 
# Using the best-estimates: the 50th percentile of the 1,000 permutations

### PCH CORE RANGE DATA: from Porcupine Caribou Management Board (2016)
# Loading polygon of PCH range 
PCH_core_range <- st_read("datasets/PCH_Core_Range_2016/PCH_Core_Range_2016.shp") #loading data


### EXTRACTION ----
st_bbox(PCH_core_range) # extent of the PCH range

# plotting shrub raster (entire) 
plot(shrub_agb_p50)

# defining extent of the PCH range polygon
range_extent <- extent(165444.3,  849222.0, 1697872.7, 2270606.5) # xmin, xmax, ymin, ymax

# cropping shrub map to extent of the PCH range
shrub_crop <- crop(x = shrub_agb_p50, y = range_extent)

# plotting cropped shrub map to visualise extent
(cropped_vis <- gplot(shrub_crop_4) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<1000, 
             scales::rescale(x,
                             to = to,
                             from = c(min(x, na.rm = TRUE), 1000)), 1)}) +
    coord_quickmap()+
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Shrub biomass cover (g/m2) of the PCH alaskan range") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 45, hjust = 1)))  # rotates x axis text

# extent of the cropped shrub map
st_bbox(shrub_crop) 

# subdividing cropped map into smaller chunks (strips) and extracting biomass 
# 1. 
range_extent_1 <- extent(165454.7, 175454.7, 1933928.1, 2270618.1) # class: extent
shrub_crop_1 <- crop(x = shrub_agb_p50, y = range_extent_1) # raster layer
poly_1 <- as(range_extent_1, 'SpatialPolygons') # making extent into polygon
class(poly_1) # checking it's a polygon
extracted_shrub_1 <- raster::extract(x = shrub_crop_1, y = poly_1, df = TRUE) # extracting pixels
glimpse(extracted_shrub_1)
extracted_shrub_1 <- na.omit(extracted_shrub_1)

# 2. 
range_extent_2 <- extent(175454.7, 185454.7,  1933928.1, 2270618.1)
shrub_crop_2 <- crop(x = shrub_agb_p50, y = range_extent_2)
poly_2 <- as(range_extent_2, 'SpatialPolygons') # making extent into polygon
class(poly_2) # checking it's a polygon
extracted_shrub_2 <- raster::extract(x = shrub_crop_2, y = poly_2, df = TRUE) # extracting pixels
glimpse(extracted_shrub_2)
extracted_shrub_2 <- na.omit(extracted_shrub_2)

# 3. 
range_extent_3 <- extent(185454.7, 195454.7,  1933928.1, 2270618.1)
shrub_crop_3 <- crop(x = shrub_agb_p50, y = range_extent_3)
poly_3 <- as(range_extent_3, 'SpatialPolygons') # making extent into polygon
class(poly_3) # checking it's a polygon
extracted_shrub_3 <- raster::extract(x = shrub_crop_3, y = poly_3, df = TRUE) # extracting pixels
glimpse(extracted_shrub_3)
extracted_shrub_3 <- na.omit(extracted_shrub_3)

# 4. 
range_extent_4 <- extent(195454.7, 205454.7,  1933928.1, 2270618.1)
shrub_crop_4 <- crop(x = shrub_agb_p50, y = range_extent_4)
poly_4 <- as(range_extent_4, 'SpatialPolygons') # making extent into polygon
class(poly_4) # checking it's a polygon
extracted_shrub_4 <- raster::extract(x = shrub_crop_4, y = poly_4, df = TRUE) # extracting pixels
glimpse(extracted_shrub_4)
extracted_shrub_4 <- na.omit(extracted_shrub_4)


# 5. 
range_extent_5 <- extent(205454.7, 215454.7,  1933928.1, 2270618.1)
shrub_crop_5 <- crop(x = shrub_agb_p50, y = range_extent_5)
poly_5 <- as(range_extent_5, 'SpatialPolygons') # making extent into polygon
class(poly_5) # checking it's a polygon
extracted_shrub_5 <- raster::extract(x = shrub_crop_5, y = poly_5, df = TRUE) # extracting pixels
glimpse(extracted_shrub_5)
extracted_shrub_5 <- na.omit(extracted_shrub_5)


# 6. 
range_extent_6 <- extent(215454.7, 225454.7,  1933928.1, 2270618.1)
shrub_crop_6 <- crop(x = shrub_agb_p50, y = range_extent_6)
poly_6 <- as(range_extent_6, 'SpatialPolygons') # making extent into polygon
class(poly_6) # checking it's a polygon
extracted_shrub_6 <- raster::extract(x = shrub_crop_6, y = poly_6, df = TRUE) # extracting pixels
glimpse(extracted_shrub_6)
extracted_shrub_6 <- na.omit(extracted_shrub_6)
