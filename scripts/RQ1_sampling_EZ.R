##%######################################################%##
#                                                          #
###              RQ1: SPATIAL ANALYSIS                   ###
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

### PART 1: SAMPLING
## RQ1: How is shrub biomass distributed in the focal study area?

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
library(tidyverse)
library(lme4)
library(Require)
library(SpaDES.tools)
library(geosphere)
library(dplyr)
library(ggeffects)
library(stargazer)
library(factoextra)
library(corrplot)
library(MuMIn)
library(performance)
library(png)
library(patchwork)

##  LOADING DATA -----

# SHRUB DATA: from Berner et al 2018
# Loading raster of shrub biomass (g/m2) on Alaskan North Slope  
shrub_agb_p50 <- raster("datasets/berner_data/shrub_agb_p50.tif") 
# Using the best-estimates: the 50th percentile of the 1,000 permutations

# PCH CORE RANGE DATA: from Porcupine Caribou Management Board (2016)
# Loading polygon of PCH range 
PCH_core_range <- st_read("datasets/PCH_Core_Range_2016/PCH_Core_Range_2016.shp") # loading data
st_bbox(PCH_core_range) # extent of the PCH range

## CROPPING -----
# Cropping shrub raster to the PCH range 
r2 <- crop(shrub_agb_p50, extent(PCH_core_range))
r3 <- mask(r2, PCH_core_range)
plot(r3) # plot cropped raster
plot(PCH_core_range, add = TRUE, lwd = 2)

# transforming CRS of cropped map from proj = aea (alaska albers) to proj = lalong 
r3_latlong <- projectRaster(r3, crs="+init=EPSG:4326", xy = TRUE)
# writeRaster(r3_latlong, "datasets/berner_data/r3_latlong.tif") #saving raster
# r3_latlong <- raster("datasets/berner_data/r3_latlong.tif") # loading raster

# resolution of cropped map
res(r3_latlong)
# 0.000726 x 0.000270 degrees 

# plotting cropped shrub map with viridis palette
(r3_cropped_viridis <- gplot(r3_latlong_agg) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<500, scales::rescale(x, to = to, from = c(min(x, na.rm = TRUE), 500)),1)}, na.value="white") +
    coord_quickmap()+
    theme_shrub() +  
    xlab("\nLongitude") +
    ylab("Latitude\n") +
    ggtitle("Shrub biomass cover (kg/m2) of the PCH alaskan range\n") +
    theme(plot.title = element_text(hjust = 0.5),     # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 30, hjust = 1)))  # rotates x axis text

## AGGREGATION ----

# aggregate shrub data to coarser resolution before extraction using aggregate()
# factor chosen dividing climate cell resolution 0.008333333 x 0.008333333 by the resolution of the cropped shrub map (latlong)
r3_latlong_agg <- aggregate(r3_latlong, fact=c(11.47842,30.8642), fun = mean, expand = TRUE) 
# writeRaster(r3_latlong_agg, "datasets/berner_data/r3_latlong_agg.tif") # saving new raster
r3_latlong_agg <- raster("datasets/berner_data/r3_latlong_agg.tif") # loading raster

# checking new resolution
res(r3_latlong_agg)
# 0.007986 x 0.008370 
projection(r3_latlong_agg)

# RANDOM SAMPLE WHOLE MAP ----

# Measuring area of raster
# get sizes of all cells in raster [km2]
cell_size <- area(r3_latlong_agg, na.rm=TRUE, weights=FALSE)
# delete NAs from vector of all raster cells
# NAs lie outside of the rastered region, can thus be omitted
cell_size <- cell_size[!is.na(cell_size)] # 0.2815663
#compute area [km2] of all cells in geo_raster
raster_area <-length(cell_size)*median(cell_size)
# print area of shrub map according to raster object
print(paste("Area of PCH Alaskan range (raster)", round(raster_area, digits = 1),"km2"))
# [1] "Area of PCH Alaskan range (raster) is 9583.6 km2"
# This means there are 9583.6 cells of ~1km x 1km 
# NB. PIXELS = CELLS

## BUFFER 
# deciding on buffer distance
res(r3_latlong_agg)
# 0.007986 0.008370 degrees
# ie. raster divided into ~1km x 1km grid cells 
# diagonal of a grid square = 1414.2 m
# buffer = diagonal of grid cell means that no point will be taken from same grid cell


# Buffered random sampling 

# a. Extracting all pixels (9583)
r3_rsample_00 <- as.data.frame(sampleRandom(r3_latlong_agg, 9583, buffer = 1414.2, na.rm=TRUE, ext=NULL, 
                                            cells=TRUE, rowcol=FALSE, xy = TRUE)) 

hist(r3_rsample_00$r3_latlong_agg) # checking distribution
mean(r3_rsample_00$r3_latlong_agg) 

# b. Extracting 1 every 2 pixels (9583/2= 4792)
r3_rsample_0 <- as.data.frame(sampleRandom(r3_latlong_agg, 4792, buffer = 1414.2, na.rm=TRUE, ext=NULL, 
                                           cells=TRUE, rowcol=FALSE, xy = TRUE)) 

hist(r3_rsample_0$r3_latlong_agg) # checking distribution
mean(r3_rsample_0$r3_latlong_agg) # 266.3

# c. Extracting 1 every 3 pixels (9583/3 = 3195)
r3_rsample_1 <- as.data.frame(sampleRandom(r3_latlong_agg, 3195, buffer = 1414.2, na.rm=TRUE, ext=NULL, 
                                           cells=TRUE, rowcol=FALSE, xy = TRUE)) 

hist(r3_rsample_1$r3_latlong_agg) # checking distribution
mean(r3_rsample_1$r3_latlong_agg) # 267.6
# mean and histogram look similar to the above, confirming extraction is accurate

# LOGIC checks 
# trying to sample 30000 pixels to see if the distribution is different 
r3_rsample_0_try <- as.data.frame(sampleRandom(r3_latlong_agg, 30000, buffer = 1414.2, na.rm=TRUE, ext=NULL, 
                                               cells=TRUE, rowcol=FALSE, xy = TRUE)) # 30000 pixels 
hist(r3_rsample_0_try$r3_latlong_agg) # checking distribution - looks similar to the other histogram
mean(r3_rsample_0_try$r3_latlong_agg) #267.4842

# I decide to use the random sample with 1 every 3 pixels sampled: r3_rsample_1 

# Checking buffer works
# calculating Haversine distance between points (x and y coordinates)
r3_rsample_01 <- r3_rsample_1  %>% 
  mutate(r3_rsample_1, Distance = distHaversine(cbind(x, y),
                                                cbind(lag(x), lag(y))))

# If distance between points > buffer distance, buffer works
r3_rsample_01 <- r3_rsample_01 %>% 
  mutate(buff = case_when(Distance >= 1414.2 ~ "T", Distance < 1414.2 ~ "F"))

r3_rsample_01 <- r3_rsample_01 %>%  filter(buff %in% c("T")) # only keeping obseervations where buff worked
unique(r3_rsample_01$buff) # T: buffer works
glimpse(r3_rsample_01)

# Cleaning random sample dataframe and making a gridcell column the new dataframe
r3_rsample_002  <- r3_rsample_01 %>%
  rename (cell_ID = "cell", 
          latitude = "y",
          longitude = "x", 
          biomass = "r3_latlong_agg") %>%
  mutate(lat = plyr::round_any(latitude, 0.5, f = floor),
         long = ifelse(longitude > 0, plyr::round_any(longitude, 0.5, f = floor), plyr::round_any(longitude, 0.5, f = ceiling))) %>% 
  mutate(gridcell = paste0("_", lat, "_", long))  %>%
  dplyr::select(cell_ID, latitude, longitude, long, lat, biomass, gridcell)

# saving all datasets
# write.csv(r3_rsample_00, file= "datasets/berner_data/r3_rsample_00.csv") # with all pixels (a.)
# write.csv(r3_rsample_001, file= "datasets/berner_data/r3_rsample_001.csv") # with half pixels (b.)
# write.csv(r3_rsample_002, file= "datasets/berner_data/r3_rsample_002.csv") # with 1/3 pixels (c.)

# I USE THE r3_rsample_002 for subsequent analyses