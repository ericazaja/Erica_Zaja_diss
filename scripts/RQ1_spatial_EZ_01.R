##%######################################################%##
#                                                          #
###              RQ1: SPATIAL ANALYSIS                  ####
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

## RQ1: What areas within the PCH Alaskan summer range have high-medium-low shrub biomass cover?
### Variation in shrub biomass across latitudes ? 

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


### LOADING DATA -----

## SHRUB DATA: from Berner et al 2018
# Loading raster of shrub biomass (g/m2) on Alaskan North Slope  
shrub_agb_p50 <- raster("datasets/berner_data/shrub_agb_p50.tif") 
# Using the best-estimates: the 50th percentile of the 1,000 permutations

### PCH CORE RANGE DATA: from Porcupine Caribou Management Board (2016)
# Loading polygon of PCH range 
PCH_core_range <- st_read("datasets/PCH_Core_Range_2016/PCH_Core_Range_2016.shp") #loading data

st_bbox(PCH_core_range) # extent of the PCH range

### CROPPING SHRUB MAP -----

# plotting shrub raster (entire) 
plot(shrub_agb_p50)

# defining extent of the PCH range polygon
range_extent <- extent(165444.3,  849222.0, 1697872.7, 2270606.5) # xmin, xmax, ymin, ymax

# cropping shrub map to extent of the PCH range
shrub_crop <- crop(x = shrub_agb_p50, y = range_extent)

# exploring resolution 
res(shrub_crop) # resolution 30m x 30m

# plotting cropped shrub map to visualise extent
(cropped_vis <- gplot(shrub_crop_e) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<500, 
             scales::rescale(x,to = to, from = c(min(x, na.rm = TRUE), 500)), 1)}) +
    coord_quickmap()+
    ylim(68.2,70.5)+
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Shrub biomass cover (g/m2) of the PCH alaskan range") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 45, hjust = 1)))  # rotates x axis text

# extent of the cropped shrub map
st_bbox(shrub_crop) 

# transforming CRS of cropped map from aea to lalong
shrub_crop_latlong <- projectRaster(shrub_crop, crs="+init=EPSG:4326", xy = TRUE) # changing to latitude longitude coords
# writeRaster(shrub_crop_latlong, "datasets/berner_data/shrub_crop_latlong.tif")

# resolution of cropped map
res(shrub_crop_latlong)
# 0.000726 m x 0.000270 m

### JOE: AGGREGATION -----
# aggregate shrub data before extraction(?) using aggregate function()
shrub_crop_latlong_agg <- aggregate(shrub_crop_latlong, fact=c(11.47842,30.8642), fun=mean, expand = TRUE) 
# factor chosen dividing climate cell resolution 0.008333333 x 0.008333333 by the resolution of the cropped shrub map (latlong)

res(shrub_crop_latlong_agg)
# 0.007986 x 0.008370 m
# not EXACTLY the same as climate resolution but close enough? 
# writeRaster(shrub_crop_latlong_agg, "datasets/berner_data/shrub_crop_latlong_agg.tif")
# shrub_crop_latlong_agg <- raster("datasets/berner_data/shrub_crop_latlong_agg.tif") # loding raster 

# OR can aggregate using resample() function
#shrub_crop_new_res <- resample(shrub_crop_latlong, precip, method="bilinear") # bilinear method is like mean for aggregate
#res(shrub_crop_new_res)== res(precip) # checking shrub and climate rasters have same resolution
# [1] TRUE TRUE but it doesnt plot


# NOT SURE THIS WORKS Splitting raster into tiles ----
nx <- 2 # number of tiles for x axis to be split into
ny <- 5 # number of tiles for y axis to be split into
raster_tiles <- splitRaster(shrub_crop_latlong_agg, nx, ny, c(2, 2), path ="datasets/berner_data")
# buffer: 10 pixels along both axes
layout(mat = matrix(seq_len(nx*ny), ncol = nx, nrow = ny))
plotOrder <- c(1,2,3,4,5,6,7,8,9,10)
if (interactive()) invisible(lapply(raster_tiles[plotOrder], plot))

### EXTRACTION (West-to-East) ----
# subdividing cropped map into 5 smaller chunks (vertical strips from West to East) and extracting biomass 
# STRIPS 1 to 5
# NB need to change to Latlong 

# Strip (1) -----
range_extent_1 <- extent(-150.17942, -140.50837 , 69.68408, 70.37209) # class: extent
shrub_crop_1 <- crop(x = shrub_crop_latlong_agg, y = range_extent_1) # class: raster layer
# shrub_crop_1_latlong <- projectRaster(shrub_crop_1, crs="+init=EPSG:4326", xy = TRUE) # changing to latitude longitude coords
res(shrub_crop_1_latlong) # 0.000733 0.000271

# random sample 
shrub_rsample_1 <- as.data.frame(sampleRandom(shrub_crop_1_latlong, 10000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                             cells=TRUE, rowcol=FALSE, xy = TRUE)) %>% mutate(strip = "1")

glimpse(shrub_rsample_1)
hist(shrub_rsample_1$layer)
# write.csv(shrub_rsample_1, "datasets/berner_data/shrub_rsample_1.csv") # saving strip dataframe

# Raster::extract
poly_1 <- as(range_extent_1, 'SpatialPolygons') # making extent into polygon
class(poly_1) # checking it's a polygon
extracted_shrub_1 <- raster::extract(x = shrub_crop_1_latlong, y = poly_1, cellnumbers = T, df = TRUE)# extracting pixels
glimpse(extracted_shrub_1)
shrub_1 <- cbind(extracted_shrub_1, xyFromCell(shrub_crop_1, extracted_shrub_1[,1])) # create coordinate columns using xyFromCell
shrub_1 <- na.omit(extracted_shrub_1) %>% mutate(strip = rep(1))
hist(shrub_crop_1)

# write.csv(shrub_1, "datasets/berner_data/shrub_1.csv") # saving strip dataframe
# extracted_shrub_1 <- read_csv("datasets/berner_data/extracted_shrub_1.csv")



# Strip (2) -----
range_extent_2 <- extent(236698.7, 307942.7,  1933928.1, 2270618.1)
shrub_crop_2 <- crop(x = shrub_agb_p50, y = range_extent_2)
shrub_crop_2_latlong <- projectRaster(shrub_crop_2, crs="+init=EPSG:4326", xy = TRUE) # changing to latitude longitude coords
res(shrub_crop_2_latlong) # 0.000733 0.000271

# random sample 
shrub_rsample_2 <- as.data.frame(sampleRandom(shrub_crop_2_latlong, 10000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy=TRUE)) %>% mutate(strip = "2")
glimpse(shrub_rsample_2)
hist(shrub_rsample_2$layer)
# write.csv(shrub_rsample_2, "datasets/berner_data/shrub_rsample_2.csv") # saving strip dataframe


# Raster::extract
poly_2 <- as(range_extent_2, 'SpatialPolygons') # making extent into polygon
class(poly_2) # checking it's a polygon
extracted_shrub_2<- raster::extract(x = shrub_crop_2_latlong, y = poly_2, cellnumbers = T, df = TRUE)# extracting pixels
glimpse(extracted_shrub_2)
shrub_2 <- cbind(extracted_shrub_2, xyFromCell(shrub_crop_2, extracted_shrub_2[,1])) # create coordinate columns using xyFromCell
shrub_2 <- na.omit(extracted_shrub_2) %>% mutate(strip = rep(2))
hist(shrub_crop_2)

# write.csv(extracted_shrub_2, "datasets/berner_data/extracted_shrub_2.csv")
# extracted_shrub_2 <- read_csv("datasets/berner_data/extracted_shrub_2.csv")


# Strip (3) ----- 
range_extent_3 <- extent(307942.7, 379186.7,  1933928.1, 2270618.1)
shrub_crop_3 <- crop(x = shrub_agb_p50, y = range_extent_3)
shrub_crop_3_latlong <- projectRaster(shrub_crop_3, crs="+init=EPSG:4326", xy = TRUE) # changing to latitude longitude coords

# random sample 
shrub_rsample_3 <- as.data.frame(sampleRandom(shrub_crop_3_latlong, 10000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy=TRUE)) %>% mutate(strip = "3")
glimpse(shrub_rsample_3)
hist(shrub_rsample_3$layer)
# write.csv(shrub_rsample_3, "datasets/berner_data/shrub_rsample_3.csv")

# Raster::extract
poly_3 <- as(range_extent_3, 'SpatialPolygons') # making extent into polygon
class(poly_3) # checking it's a polygon
extracted_shrub_3 <- raster::extract(x = shrub_crop_3_latlong, y = poly_3, cellnumbers = T, df = TRUE)# extracting pixels
glimpse(extracted_shrub_3)
shrub_3 <- cbind(extracted_shrub_3, xyFromCell(shrub_crop_3, extracted_shrub_3[,1])) # create coordinate columns using xyFromCell
shrub_3 <- na.omit(extracted_shrub_3) %>% mutate(strip = rep(3))
hist(shrub_crop_3)

# write.csv(extracted_shrub_3, "datasets/berner_data/extracted_shrub_3.csv")
# extracted_shrub_3 <- read_csv("datasets/berner_data/extracted_shrub_3.csv")

# Strip (4) ----- 
range_extent_4 <- extent(379186.7, 450430.7,  1933928.1, 2270618.1)
shrub_crop_4 <- crop(x = shrub_agb_p50, y = range_extent_4)
shrub_crop_4_latlong <- projectRaster(shrub_crop_4, crs="+init=EPSG:4326", xy = TRUE) # changing to latitude longitude coords

# random sample 
shrub_rsample_4 <- as.data.frame(sampleRandom(shrub_crop_4_latlong, 10000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy=TRUE)) %>% mutate(strip = "4")
glimpse(shrub_rsample_4)
hist(shrub_rsample_4$layer)
write.csv(shrub_rsample_4, "datasets/berner_data/shrub_rsample_4.csv")

# Raster::extract
poly_4 <- as(range_extent_4, 'SpatialPolygons') # making extent into polygon
class(poly_4) # checking it's a polygon
extracted_shrub_4 <- raster::extract(x = shrub_crop_4_latlong, y = poly_4, cellnumbers = T, df = TRUE)# extracting pixels
glimpse(extracted_shrub_4)
shrub_4 <- cbind(extracted_shrub_4, xyFromCell(shrub_crop_4, extracted_shrub_4[,1])) # create coordinate columns using xyFromCell
shrub_4 <- na.omit(extracted_shrub_4) %>% mutate(strip = rep(4))
hist(shrub_crop_4)

# write.csv(extracted_shrub_4, "datasets/berner_data/extracted_shrub_4.csv")
# extracted_shrub_4 <- read_csv("datasets/berner_data/extracted_shrub_4.csv")


# Strip (5) ----- 
range_extent_5 <- extent(450430.7, 521674.7,  1933928.1, 2270618.1)
shrub_crop_5 <- crop(x = shrub_agb_p50, y = range_extent_5)
shrub_crop_5_latlong <- projectRaster(shrub_crop_5, crs="+init=EPSG:4326", xy = TRUE) # changing to latitude longitude coords

# random sample 
shrub_rsample_5 <- as.data.frame(sampleRandom(shrub_crop_5_latlong, 10000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy=TRUE)) %>% mutate(strip = "5")
glimpse(shrub_rsample_5)
hist(shrub_rsample_5$layer)
#write.csv(shrub_rsample_5, "datasets/berner_data/shrub_rsample_5.csv")

# Raster::extract
poly_5 <- as(range_extent_5, 'SpatialPolygons') # making extent into polygon
class(poly_5) # checking it's a polygon
extracted_shrub_5 <- raster::extract(x = shrub_crop_5_latlong, y = poly_5, cellnumbers = T, df = TRUE)# extracting pixels
glimpse(extracted_shrub_5)
shrub_5 <- cbind(extracted_shrub_5, xyFromCell(shrub_crop_5, extracted_shrub_5[,1])) # create coordinate columns using xyFromCell
shrub_5 <- na.omit(extracted_shrub_5) %>% mutate(strip = rep(5))
hist(shrub_crop_5)

# write.csv(extracted_shrub_5, "datasets/berner_data/extracted_shrub_5.csv")
# extracted_shrub_5 <- read_csv("datasets/berner_data/extracted_shrub_5.csv")


## Merging strips West to East (1-5) ----

shrub_all <- rbind(extracted_shrub_1, extracted_shrub_2, extracted_shrub_3, extracted_shrub_4, extracted_shrub_5) 
# write.csv(shrub_all, "datasets/berner_data/shrub_all.csv")
# shrub_all <- read_csv("datasets/berner_data/shrub_all.csv")

shrub_all_random_WE <- rbind(shrub_rsample_1, shrub_rsample_2, shrub_rsample_3, shrub_rsample_4, shrub_rsample_5) 
# write.csv(shrub_all_random, "datasets/berner_data/shrub_all_random.csv")

### Distribtion ----
# Histogram of shrub biomass for each strip
(hist_random <-shrub_all_random %>%
    ggplot(aes(x = layer, fill = strip)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 60) +
    scale_fill_manual(values=c("#404080", "#69b3a2", "red", "yellow", "green")) +
    theme_bw())

# Histogram of overall shrub biomass
(hist_random_all <-shrub_all_random %>%
    ggplot(aes(x = layer)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 60) +
    theme_bw())

### Data right skewed. More shrubs with lower biomass.

### Model 1: Biomass VS strip  ----
shrub_all_random_new_WE <- shrub_all_random %>%
  rename (cell_ID = "cell", 
              lat = x, long = y, 
              biomass = layer) %>%
  mutate(biomass_level = case_when (biomass <= 160 ~ 'Low',
                                    biomass > 160  & biomass < 500 ~ 'Medium', 
                                    biomass >= 500 ~ 'High')) 

# write.csv(shrub_all_random_new, "datasets/berner_data/shrub_all_random_new.csv")

str(shrub_all_random_new)
shrub_all_random_new$biomass_level <- as.character(as.factor(shrub_all_random_new$biomass_level))   
str(shrub_all_random_new$biomass_level) # biomass level is factor with 3 levels 

shrub_all_random_new$strip <- as.factor(as.character(shrub_all_random_new$strip))   
str(shrub_all_random_new$strip) # strip is factor with 5 levels (random effect)

(hist_levels <-shrub_all_random_new %>%
    ggplot(aes(x = biomass, fill = biomass_level)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 60) +
    scale_fill_manual(values=c("#404080", "#69b3a2", "yellow")) +
    theme_bw())


model_1 <- lmer(biomass ~ strip + (1|strip), data = shrub_all_random_new) # doesnt run
summary(model_1)

# Boxplot : biomass ~ strip
(mixed_plot <- ggplot(shrub_all_random_new, aes(x = strip, y = biomass, colour = strip)) +
    geom_boxplot(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))

boxplot(biomass ~ strip, data = shrub_all_random_new)



### JOE: EXTRACTION (North-to-South) ----
# STRIPS a to e
# extent of the cropped shrub map
st_bbox(shrub_crop_latlong_agg) 
#  xmin       ymin       xmax       ymax 
# -150.17942   66.93202 -140.50837   70.37209 
# Using 68.40000 as ymin since that's where the shrub cover map starts

# Strip (a) -----
range_extent_a <- extent(-150.17942, -140.50837 ,69.97767, 70.37209 ) # class: extent
shrub_crop_a <- crop(x = shrub_crop_latlong_agg, y = range_extent_a) # class: raster layer
res(shrub_crop_a) # 0.007986 0.008370

# JOE: random sample 
shrub_rsample_a <- as.data.frame(sampleRandom(shrub_crop_a, 5000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE)) %>% mutate(strip = "a")
# How do I pick how many points to sample? For now i picked 5000 random samples for each strip - given strip e small size 
# and is the buffer working?
glimpse(shrub_rsample_a)
hist(shrub_rsample_a$shrub_crop_latlong_agg)
# write.csv(shrub_rsample_a, "datasets/berner_data/shrub_rsample_a.csv") # saving strip dataframe

# Strip (b) -----
range_extent_b <- extent(-150.17942, -140.50837 , 69.58325, 69.97767) # class: extent
shrub_crop_b <- crop(x = shrub_crop_latlong_agg, y = range_extent_b) # class: raster layer
res(shrub_crop_b) # 0.007986 0.008370

# JOE: random sample 
shrub_rsample_b <- as.data.frame(sampleRandom(shrub_crop_b, 5000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE)) %>% mutate(strip = "b")
# How do I pick how many points to sample? and is the buffer working?
glimpse(shrub_rsample_b)
hist(shrub_rsample_b$shrub_crop_latlong_agg)
# write.csv(shrub_rsample_a, "datasets/berner_data/shrub_rsample_a.csv") # saving strip dataframe

# Strip (c) -----
range_extent_c <- extent(-150.17942, -140.50837, 69.18883 , 69.58325) # class: extent
shrub_crop_c <- crop(x = shrub_crop_latlong_agg, y = range_extent_c) # class: raster layer
res(shrub_crop_c) # 0.007986 0.008370

# JOE: random sample 
shrub_rsample_c <- as.data.frame(sampleRandom(shrub_crop_c, 5000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE)) %>% mutate(strip = "c")
# How do I pick how many points to sample? and is the buffer working?
glimpse(shrub_rsample_c)
hist(shrub_rsample_c$shrub_crop_latlong_agg)
# write.csv(shrub_rsample_c, "datasets/berner_data/shrub_rsample_c.csv") # saving strip dataframe

# Strip (d) -----
range_extent_d <- extent(-150.17942, -140.50837, 68.79441 , 69.18883) # class: extent
shrub_crop_d <- crop(x = shrub_crop_latlong_agg, y = range_extent_d) # class: raster layer
res(shrub_crop_d) # 0.007986 0.008370

# JOE: random sample 
shrub_rsample_d <- as.data.frame(sampleRandom(shrub_crop_d, 5000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE)) %>% mutate(strip = "d")
# How do I pick how many points to sample? and is the buffer working?
glimpse(shrub_rsample_d)
hist(shrub_rsample_d$shrub_crop_latlong_agg)
# write.csv(shrub_rsample_d, "datasets/berner_data/shrub_rsample_d.csv") # saving strip dataframe

# Strip (e) -----
range_extent_e <- extent(-150.17942, -140.50837, 68.39999, 68.79441) # class: extent
shrub_crop_e <- crop(x = shrub_crop_latlong_agg, y = range_extent_e) # class: raster layer
res(shrub_crop_e) # 0.007986 0.008370

# JOE: random sample 
shrub_rsample_e <- as.data.frame(sampleRandom(shrub_crop_e, 5000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE)) %>% mutate(strip = "e")
# How do I pick how many points to sample? and is the buffer working?
glimpse(shrub_rsample_e)
hist(shrub_rsample_e$shrub_crop_latlong_agg)
# write.csv(shrub_rsample_e, "datasets/berner_data/shrub_rsample_e.csv") # saving strip dataframe

# Merging datasets North-south (strips a-e) ----
shrub_all_random_NS <- rbind(shrub_rsample_a, shrub_rsample_b, shrub_rsample_c, shrub_rsample_d, shrub_rsample_e) 
# write.csv(shrub_all_random_NS, "datasets/berner_data/shrub_all_random_NS.csv")

# Distribution ----
# Overall shrub biomass
(hist_random_all_NS <-shrub_all_random_NS %>%
    ggplot(aes(x = shrub_crop_latlong_agg)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 60) +
    theme_bw())

# By north-south strip
(hist_random_NS <-shrub_all_random_NS %>%
    ggplot(aes(x = shrub_crop_latlong_agg, fill = strip)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 60) +
    scale_fill_manual(values=c("#404080", "#69b3a2", "red", "yellow", "green")) +
    theme_bw())

# JOE: Model ----
shrub_all_random_new_NS <- shrub_all_random_NS %>%
  rename (cell_ID = "cell", 
          lat = x, long = y, 
          biomass = shrub_crop_latlong_agg)

# write.csv(shrub_all_random_new_NS, "datasets/berner_data/shrub_all_random_new_NS.csv")
boxplot(biomass~strip, data = shrub_all_random_new_NS)

# making strip a factor
shrub_all_random_new_NS$strip <- as.factor(as.character(shrub_all_random_new_NS$strip))   
str(shrub_all_random_new_NS)

# model biomass as function of latitude with strip as random effect
model_b <- lmer(biomass ~ lat + (1|strip), data = shrub_all_random_new_NS)
summary(model_b)

# visualising scatter 
(scatter_model_b <- ggplot(shrub_all_random_new_NS, aes(x = lat, y = biomass)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm") +
    theme_classic())
# I think I need the ggpredict lmer plot
# random slopes



# LOGIC CHECKS ----
# checking if norhtern strip has lower biomass than southern strip

# Northern strip 
range_extent_n <- extent(165454.7, 521674.7, 2170618.1, 2200618.1) # class: extent
shrub_crop_n <- crop(x = shrub_agb_p50, y = range_extent_n)
poly_n <- as(range_extent_n, 'SpatialPolygons') # making extent into polygon
class(poly_n) # checking it's a polygon

## More datapoints in the Northern strip due to how map is
## To the regions comparable: randomly sample the Northern strip to get down to the same sample size as the Southern strip. 

# buffered random sampling in Northern strip
shrub_sample_n <- as.data.frame(sampleRandom(shrub_crop_n, 10000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                             cells=TRUE, rowcol=FALSE, xy=TRUE)) %>% mutate(zone = "north")
# 10000: positive integer giving the number of items to choose
# cells = TRUE, sampled cell numbers are also returned
# If xy = TRUE, coordinates of sampled cells are also returned
hist(shrub_sample_n$shrub_agb_p50)

#extracted_shrub_n <- raster::extract(x = shrub_crop_n, y = poly_n, df = TRUE) # extracting pixels
#glimpse(extracted_shrub_n)
#extracted_shrub_n <- na.omit(extracted_shrub_n)  %>% mutate(zone = "north")
#hist(shrub_crop_n)
#range(extracted_shrub_n) # 1-2248

# Southern strip 
range_extent_s <- extent(165454.7, 521674.7, 2100000.1, 2130000.1) # class: extent
shrub_crop_s <- crop(x = shrub_agb_p50, y = range_extent_s)
poly_s <- as(range_extent_s, 'SpatialPolygons') # making extent into polygon
class(poly_s) # checking it's a polygon

# buffered random sampling in Southern strip
shrub_sample_s <- as.data.frame(sampleRandom(shrub_crop_s, 10000, buffer = 900, na.rm=TRUE, ext=NULL, 
             cells=TRUE, rowcol=FALSE, xy=TRUE)) %>% mutate(zone = "south")


hist(shrub_sample_s$shrub_agb_p50)

#extracted_shrub_s <- raster::extract(x = shrub_crop_s, y = poly_s, buffer = 900, fun = mean, cellnumbers = T, df = TRUE) # extracting pixels
#glimpse(extracted_shrub_s)
# create coordinate columns using xyFromCell
#df.coords <- cbind(extracted_shrub_s, xyFromCell(shrub_crop_s, extracted_shrub_s[,1]))
#glimpse(df.coords)
#df.coords <- na.omit(df.coords) %>% mutate(zone = "south")
#hist(df.coords$shrub_agb_p50)
#str(df.coords)
#range(extracted_shrub_s) # 1 2788


# North and south strips in the same histogram 
#shrub_check <- rbind(extracted_shrub_s, extracted_shrub_n)
buff_shrub_check <- rbind(shrub_sample_n, shrub_sample_s)

(hist_check <- buff_shrub_check %>%
  ggplot(aes(x = shrub_agb_p50, fill = zone)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 60) +
  scale_fill_manual(values=c("#404080", "#69b3a2")) +
  theme_bw())
# NB the histogram for the north is more skewed towards lower biomass


## GRID???? -----
# Dividing map into 10 plots

# Plot (a) -----
range_extent_a <- extent(165454.7, 236698.7, 1933928.1, 2102273.1) # class: extent
shrub_plot_a <- crop(x = shrub_agb_p50, y = range_extent_a) # class: raster layer
shrub_plot_a_latlong <- projectRaster(shrub_plot_a, crs="+init=EPSG:4326", xy = TRUE) # changing to latitude longitude coords
shrub_plot_a_latlong <- raster::aggregate(shrub_plot_a_latlong, fact=33, fun=mean) # factor is wrong
res(shrub_plot_a_latlong)

# random sample 
shrub_rsample_a <- as.data.frame(sampleRandom(shrub_plot_a_latlong, 10000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE)) %>% mutate(plot = "a")

glimpse(shrub_rsample_a)
hist(shrub_rsample_a$shrub_agb_p50)
# write.csv(shrub_rsample_a, "datasets/berner_data/shrub_rsample_a.csv") # saving strip dataframe

# Raster::extract
poly_1 <- as(range_extent_1, 'SpatialPolygons') # making extent into polygon
class(poly_1) # checking it's a polygon
extracted_shrub_1 <- raster::extract(x = shrub_crop_1_latlong, y = poly_1, cellnumbers = T, df = TRUE)# extracting pixels
glimpse(extracted_shrub_1)
shrub_1 <- cbind(extracted_shrub_1, xyFromCell(shrub_crop_1, extracted_shrub_1[,1])) # create coordinate columns using xyFromCell
shrub_1 <- na.omit(extracted_shrub_1) %>% mutate(strip = rep(1))
hist(shrub_crop_1)

# write.csv(shrub_1, "datasets/berner_data/shrub_1.csv") # saving strip dataframe
# extracted_shrub_1 <- read_csv("datasets/berner_data/extracted_shrub_1.csv")


### AGGREGATION
#aggregate from 30x30m resolution to 0.008333333x0.008333333m (factor = 33)





