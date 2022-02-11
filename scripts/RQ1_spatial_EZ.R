##%######################################################%##
#                                                          #
###              RQ1: SPATIAL ANALYSIS                  ####
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

## RQ1: What areas within the PCH Alaskan summer range have high-medium-low shrub biomass cover?
### Variation in shrub biomass across latitudes 

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


### EXTRACTION (West-to-East) ----
st_bbox(PCH_core_range) # extent of the PCH range

# plotting shrub raster (entire) 
plot(shrub_agb_p50)

# defining extent of the PCH range polygon
range_extent <- extent(165444.3,  849222.0, 1697872.7, 2270606.5) # xmin, xmax, ymin, ymax

# cropping shrub map to extent of the PCH range
shrub_crop <- crop(x = shrub_agb_p50, y = range_extent)

# plotting cropped shrub map to visualise extent
(cropped_vis <- gplot(shrub_crop) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<1000, 
             scales::rescale(x,to = to, from = c(min(x, na.rm = TRUE), 1000)), 1)}) +
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

# subdividing cropped map into 5 smaller chunks (strips from West to East) and extracting biomass 
# 1. 
range_extent_1 <- extent(165454.7, 236698.7, 1933928.1, 2270618.1) # class: extent
shrub_crop_1 <- crop(x = shrub_agb_p50, y = range_extent_1) # raster layer
poly_1 <- as(range_extent_1, 'SpatialPolygons') # making extent into polygon
class(poly_1) # checking it's a polygon
extracted_shrub_1 <- raster::extract(x = shrub_crop_1, y = poly_1, cellnumbers = T, df = TRUE)# extracting pixels
glimpse(extracted_shrub_1)
shrub_1 <- cbind(extracted_shrub_1, xyFromCell(shrub_crop_1, extracted_shrub_1[,1])) # create coordinate columns using xyFromCell
shrub_1 <- na.omit(extracted_shrub_1) %>% mutate(strip = rep(1))

write.csv(shrub_1, "datasets/berner_data/shrub_1.csv") # saving strip dataframe
#extracted_shrub_1 <- read_csv("datasets/berner_data/extracted_shrub_1.csv")

hist(shrub_crop_1)


# 2. 
range_extent_2 <- extent(236698.7, 307942.7,  1933928.1, 2270618.1)
shrub_crop_2 <- crop(x = shrub_agb_p50, y = range_extent_2)
poly_2 <- as(range_extent_2, 'SpatialPolygons') # making extent into polygon
class(poly_2) # checking it's a polygon
extracted_shrub_2 <- raster::extract(x = shrub_crop_2, y = poly_2, df = TRUE) # extracting pixels
glimpse(extracted_shrub_2)
extracted_shrub_2 <- na.omit(extracted_shrub_2)
hist(shrub_crop_2)
extracted_shrub_2 <- extracted_shrub_2 %>% mutate(strip = rep(2)) 

# write.csv(extracted_shrub_2, "datasets/berner_data/extracted_shrub_2.csv")
# extracted_shrub_2 <- read_csv("datasets/berner_data/extracted_shrub_2.csv")


# 3. 
range_extent_3 <- extent(307942.7, 379186.7,  1933928.1, 2270618.1)
shrub_crop_3 <- crop(x = shrub_agb_p50, y = range_extent_3)
poly_3 <- as(range_extent_3, 'SpatialPolygons') # making extent into polygon
class(poly_3) # checking it's a polygon
extracted_shrub_3 <- raster::extract(x = shrub_crop_3, y = poly_3, df = TRUE) # extracting pixels
glimpse(extracted_shrub_3)
extracted_shrub_3 <- na.omit(extracted_shrub_3)
hist(shrub_crop_3)
extracted_shrub_3 <- extracted_shrub_3 %>% mutate(strip = rep(3)) 

# write.csv(extracted_shrub_3, "datasets/berner_data/extracted_shrub_3.csv")
# extracted_shrub_3 <- read_csv("datasets/berner_data/extracted_shrub_3.csv")

# 4. 
range_extent_4 <- extent(379186.7, 450430.7,  1933928.1, 2270618.1)
shrub_crop_4 <- crop(x = shrub_agb_p50, y = range_extent_4)
poly_4 <- as(range_extent_4, 'SpatialPolygons') # making extent into polygon
class(poly_4) # checking it's a polygon
extracted_shrub_4 <- raster::extract(x = shrub_crop_4, y = poly_4, df = TRUE) # extracting pixels
glimpse(extracted_shrub_4)
extracted_shrub_4 <- na.omit(extracted_shrub_4)
hist(shrub_crop_4)
extracted_shrub_4 <- extracted_shrub_4 %>% mutate(strip = rep(4)) 

# write.csv(extracted_shrub_4, "datasets/berner_data/extracted_shrub_4.csv")
# extracted_shrub_4 <- read_csv("datasets/berner_data/extracted_shrub_4.csv")



# 5. 
range_extent_5 <- extent(450430.7, 521674.7,  1933928.1, 2270618.1)
shrub_crop_5 <- crop(x = shrub_agb_p50, y = range_extent_5)
poly_5 <- as(range_extent_5, 'SpatialPolygons') # making extent into polygon
class(poly_5) # checking it's a polygon
extracted_shrub_5 <- raster::extract(x = shrub_crop_5, y = poly_5, df = TRUE) # extracting pixels
glimpse(extracted_shrub_5)
extracted_shrub_5 <- na.omit(extracted_shrub_5)
hist(shrub_crop_5)
extracted_shrub_5 <- extracted_shrub_5 %>% mutate(strip = rep(5)) 

# write.csv(extracted_shrub_5, "datasets/berner_data/extracted_shrub_5.csv")
# extracted_shrub_5 <- read_csv("datasets/berner_data/extracted_shrub_5.csv")


## MERGING DATASETS ----

shrub_all <- rbind(extracted_shrub_1, extracted_shrub_2, extracted_shrub_3, extracted_shrub_4, extracted_shrub_5) 
# write.csv(shrub_all, "datasets/berner_data/shrub_all.csv")
# shrub_all <- read_csv("datasets/berner_data/shrub_all.csv")

### EXTRACTION (North-to-South) ----


# LOGIC CHECKS ----
# checking if norhtern strip has lower biomass than southern strip

# Northern strip 
range_extent_n <- extent(165454.7, 521674.7, 2170618.1, 2200618.1) # class: extent
shrub_crop_n <- crop(x = shrub_agb_p50, y = range_extent_n)
poly_n <- as(range_extent_n, 'SpatialPolygons') # making extent into polygon
class(poly_n) # checking it's a polygon
# buffered random sampling
shrub_sample_n <- as.data.frame(sampleRandom(shrub_crop_n, 10000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                             cells=TRUE, rowcol=FALSE, xy=TRUE))

shrub_sample_n <- shrub_sample_n %>% mutate(zone = "north")

hist(shrub_sample_n$shrub_agb_p50)

extracted_shrub_n <- raster::extract(x = shrub_crop_n, y = poly_n, df = TRUE) # extracting pixels
glimpse(extracted_shrub_n)
extracted_shrub_n <- na.omit(extracted_shrub_n)  %>% mutate(zone = "north")
hist(shrub_crop_n)
range(extracted_shrub_n) # 1-2248


# Southern strip 
range_extent_s <- extent(165454.7, 521674.7, 2100000.1, 2130000.1) # class: extent
shrub_crop_s <- crop(x = shrub_agb_p50, y = range_extent_s)
poly_s <- as(range_extent_s, 'SpatialPolygons') # making extent into polygon
class(poly_s) # checking it's a polygon

# buffered random sampling
shrub_sample_s <- as.data.frame(sampleRandom(shrub_crop_s, 10000, buffer = 900, na.rm=TRUE, ext=NULL, 
             cells=TRUE, rowcol=FALSE, xy=TRUE))

shrub_sample_s <- shrub_sample_s %>% mutate(zone = "south")

# 10000: positive integer giving the number of items to choose
# cells = TRUE, sampled cell numbers are also returned
# If xy = TRUE, coordinates of sampled cells are also returned
hist(shrub_sample_s$shrub_agb_p50)

glimpse(try)
extracted_shrub_s <- raster::extract(x = shrub_crop_s, y = poly_s, buffer = 900, fun = mean, cellnumbers = T, df = TRUE) # extracting pixels
glimpse(extracted_shrub_s)
# create coordinate columns using xyFromCell
df.coords <- cbind(extracted_shrub_s, xyFromCell(shrub_crop_s, extracted_shrub_s[,1]))
glimpse(df.coords)
df.coords <- na.omit(df.coords) %>% mutate(zone = "south")
hist(df.coords$shrub_agb_p50)
str(df.coords)
range(extracted_shrub_s) # 1 2788



# North and south strips in the same histogram 
shrub_check <- rbind(extracted_shrub_s, extracted_shrub_n)
buff_shrub_check <- rbind(shrub_sample_n, shrub_sample_s)

(hist_check <- buff_shrub_check %>%
  ggplot(aes(x = shrub_agb_p50, fill = zone)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 60) +
  scale_fill_manual(values=c("#404080", "#69b3a2")) +
  theme_bw())
# NB the histogram for the north is more skewed towards lower biomass
## More datapoints in the Northern strip due to how map is
## To the regions comparable: randomly sample the Northern strip to get down to the same sample size as the Southern strip. 
# There is a dplyr function for that.

### measuring area of focal area 
# shrub_latlong <- projectRaster(shrub_crop, crs = "+init=epsg:4326") # doesnt work 
cell_size <- area(shrub_latlong, na.rm=TRUE, weights=FALSE)
cell_size  <-cell_size[!is.na(cell_size)]
raster_area<-length(cell_size)*median(cell_size)
#print(paste("Area of focal area (raster):“,round(raster_area, digits=1),“km2″))

