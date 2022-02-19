##%######################################################%##
#                                                          #
###              RQ1: SPATIAL ANALYSIS                  ####
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

## RQ1: What areas within the PCH Alaskan summer range have high-medium-low shrub biomass cover?
### How does shrub biomass vary across latitudes? 

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

# cropping shrub map to extent of the PCH range (WRONG)
shrub_crop <- crop(x = shrub_agb_p50, y = range_extent)

# exploring resolution 
res(shrub_crop) # resolution 30m x 30m

# plotting cropped shrub map to visualise extent
(cropped_vis <- gplot(r3_latlong_agg) +
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

# transforming CRS of cropped map from proj = aea to proj = lalong
shrub_crop_latlong <- projectRaster(shrub_crop, crs="+init=EPSG:4326", xy = TRUE) # changing to latitude longitude coords
# writeRaster(shrub_crop_latlong, "datasets/berner_data/shrub_crop_latlong.tif")
# shrub_crop_latlong <- raster("datasets/berner_data/shrub_crop_latlong.tif")

# RIGHT crop and mask ---- 
r2 <- crop(shrub_agb_p50, extent(PCH_core_range))
r3 <- mask(r2, PCH_core_range)
plot(r3) 
plot(PCH_core_range, add=TRUE, lwd=2)

r3_latlong <- projectRaster(r3, crs="+init=EPSG:4326", xy = TRUE)
writeRaster(r3_latlong, "datasets/berner_data/r3_latlong.tif")

# resolution of cropped map
res(shrub_crop_latlong)
# 0.000726 m x 0.000270 m

### AGGREGATION -----

# aggregate shrub data to coarser resolution before extraction(?) using aggregate function()
shrub_crop_latlong_agg <- aggregate(shrub_crop_latlong, fact=c(11.47842,30.8642), fun=mean, expand = TRUE) 
# factor chosen dividing climate cell resolution 0.008333333 x 0.008333333 by the resolution of the cropped shrub map (latlong)
shrub_crop_latlong_agg <- raster("datasets/berner_data/shrub_crop_latlong_agg.tif")

res(shrub_crop_latlong_agg)
# 0.007986 x 0.008370 m
# not EXACTLY the same as climate resolution but close enough? 
# writeRaster(shrub_crop_latlong_agg, "datasets/berner_data/shrub_crop_latlong_agg.tif")
# shrub_crop_latlong_agg <- raster("datasets/berner_data/shrub_crop_latlong_agg.tif") # loding raster 

r3_latlong_agg <- aggregate(r3_latlong, fact=c(11.47842,30.8642), fun=mean, expand = TRUE) 
writeRaster(r3_latlong_agg, "datasets/berner_data/r3_latlong_agg.tif")

# RANDOM SAMPLE WHOLE MAP ----
# measuring area of raster
#get sizes of all cells in raster [km2]
cell_size<-area(r3_latlong_agg, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
raster_area<-length(cell_size)*median(cell_size)
#print area of shrub map according to raster object
print(paste("Area of PCH Alaskan range (raster)", round(raster_area, digits=1),"km2"))
# [1] "Area of PCH Alaskan range (raster) is 9583.6 km2"

# deciding on buffer distance
res(shrub_crop_latlong_agg)
# 0.007986 0.008370
# divided into 1km x 1km grid cells 
# diagonal of a grid square = 1414.2 m
# buffer = diagonal of grid cell means that no point will be taken from same grid cell

# buffered random sampling
shrub_rsample_0 <- as.data.frame(sampleRandom(shrub_crop_latlong_agg, 25000, buffer = 1414.2, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE))

hist(shrub_rsample_0$shrub_crop_latlong_agg)

# scatter shrub biomass Vs lat
(scatter_model_b <- ggplot(shrub_rsample_0, aes(x = y, y = shrub_crop_latlong_agg)) +
  geom_point(size = 0.1) +
  geom_smooth(method = "lm") +
  theme_classic())
# biomass decreases with increasing lat

ggplot(shrub_rsample_0,aes(x=x,y=y))+ geom_point(aes(shrub_crop_latlong_agg))

# checking buffer works
shrub_rsample_01 <- shrub_rsample_0 %>% 
  mutate(shrub_rsample_0, Distance = distHaversine(cbind(x, y),
                                cbind(lag(x), lag(y))))
         
shrub_rsample_01 <- shrub_rsample_01 %>% 
  mutate(buff = case_when(Distance >= 1414.2 ~ "T", Distance < 1414.2 ~ "F"))

shrub_rsample_01 <- shrub_rsample_01 %>%  filter(buff %in% c("T"))
# only keeping obseervations where buff worked

unique(shrub_rsample_01$buff) # T

# Cleaning and making a gridcell column the new dataframe
shrub_rsample_00 <- shrub_rsample_01 %>%
  rename (cell_ID = "cell", 
          lat = "y", long = "x", 
          biomass = "shrub_crop_latlong_agg") %>%
  mutate(lat = plyr::round_any(lat, 0.5, f = floor),
         long = ifelse(long > 0, plyr::round_any(long, 0.5, f = floor), plyr::round_any(long, 0.5, f = ceiling))) %>% 
  mutate(gridcell = paste0("_", lat, "_", long))%>%
  select(cell_ID, long, lat, biomass, gridcell)

# write.csv(shrub_rsample_00, file= "datasets/berner_data/shrub_rsample_00.csv")

# THEME----
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

# MODELLING ----
hist(shrub_rsample_00$biomass) # distribution 

# Model 1. biomass vs lat ----
model_1 <- lmer(biomass~lat + (1|gridcell), data = shrub_rsample_00)
summary(model_1)
# total variance: 6713 + 10450 =17163
# variance for gridcell =  6713 
# amount of variance explained by random effect: 6713/17163 =0.3911321 = ~40%
# I.e. differences between grid cells explain ~40% of the variance 
# that’s “left over” after the variance explained by our fixed effect (lat).
# estimate for latitude (exp variable =  -115.16 ) i.e. latitude negatively impacts biomass
# significant effect of lat on biomass: -115.165***  

# Checking model 1 assumptions ----
plot(model_1)
qqnorm(resid(model_1))
qqline(resid(model_1))  # points fall nicely onto the line - good!

# Output table model 1----
library(stargazer)

stargazer(model_1, type = "text",
digits = 3,
star.cutoffs = c(0.05, 0.01, 0.001),
digit.separator = "")

# scatter shrub biomass Vs lat
(scatter_model_1 <- ggplot(shrub_rsample_00, aes(x = lat, y = biomass)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm") +
    theme_shrub())

# Extracting model predictions 
pred_model_1 <- ggpredict(model_1, terms = c("lat"))  # this gives overall predictions for the model
# write.csv(pred_model_1, file = "datasets/pred_model_1.csv")

# Plot the predictions 
plot_model_1 <- (ggplot(pred_model_1) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = shrub_rsample_00,                      # adding the raw data 
               aes(x = lat, y = biomass), size = 0.5) + 
    labs(x = "Latitude", y = "Shrub biomass (g/m2)", 
         title = "Shrub biomass decreases with latitude") + 
    theme_shrub()
)

# Model 2. biomass vs long ----
model_2 <- lmer(biomass~long + (1|gridcell), data = shrub_rsample_00)
summary(model_2)
# total variance: 10797+ 10450 =21247
# variance for gridcell =  10797 
# amount of variance explained by random effect: 10797 /21247 =0.5081659 = ~50%
# I.e. differences between grid cells explain ~40% of the variance 
# that’s “left over” after the variance explained by our fixed effect (long).
# estimate for latitude (exp variable =  4.065  ) 
# not significant effect of long on biomass


# Checking model 2 assumptions ----
plot(model_2)
qqnorm(resid(model_2))
qqline(resid(model_2))  # points fall nicely onto the line - good!

# Output table model 2 ----
stargazer(model_2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

(scatter_model_2 <- ggplot(shrub_rsample_00, aes(x = long, y = biomass)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm") +
    theme_shrub())

# Extracting model predictions 
pred_model_2 <- ggpredict(model_2, terms = c("long"))  # this gives overall predictions for the model
# write.csv(pred_model_2, file = "datasets/pred_model_2.csv")

# Plot the predictions 
plot_model_2 <- (ggplot(pred_model_2) + 
                   geom_line(aes(x = x, y = predicted)) +          # slope
                   geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                               fill = "lightgrey", alpha = 0.5) +  # error band
                   geom_point(data = shrub_rsample_00,                      # adding the raw data 
                              aes(x = long, y = biomass), size = 0.5) + 
                   labs(x = "Longitude", y = "Shrub biomass (g/m2)", 
                        title = "Shrub biomass does not vary with longitude") + 
                   theme_shrub()
)

#### END -----



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

### EXTRACTION (West-to-East) ----
# subdividing cropped map into 5 smaller chunks (vertical strips from West to East) and extracting biomass 
# STRIPS 1 to 5
# extent of the cropped shrub map
st_bbox(shrub_crop_latlong_agg) 
#  xmin       ymin       xmax       ymax 
# -150.17942   66.93202 -140.50837   70.37209 
# Using 68.40000 as ymin since that's where the shrub cover map starts

# Strip (1) -----
range_extent_1 <- extent(-150.17942, -148.2452 ,  68.40000, 70.37209) # class: extent
shrub_crop_1 <- crop(x = shrub_crop_latlong_agg, y = range_extent_1) # class: raster layer
res(shrub_crop_1) # 0.007986 0.008370

# random sample 
shrub_rsample_1 <- as.data.frame(sampleRandom(shrub_crop_1, 5000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE)) %>% mutate(strip = "1")


glimpse(shrub_rsample_1)
hist(shrub_rsample_1$shrub_crop_latlong_agg)
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
range_extent_2 <- extent(-148.2452, -146.311 ,  68.40000, 70.37209) # class: extent
shrub_crop_2 <- crop(x = shrub_crop_latlong_agg, y = range_extent_2) # class: raster layer
res(shrub_crop_2) # 0.007986 0.008370

# random sample 
shrub_rsample_2 <- as.data.frame(sampleRandom(shrub_crop_2, 5000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE)) %>% mutate(strip = "2")

glimpse(shrub_rsample_2)
hist(shrub_rsample_2$shrub_crop_latlong_agg)
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
range_extent_3 <- extent(-146.311 , -144.3768,  68.40000, 70.37209) # class: extent
shrub_crop_3 <- crop(x = shrub_crop_latlong_agg, y = range_extent_3) # class: raster layer
res(shrub_crop_3) # 0.007986 0.008370

# random sample 
shrub_rsample_3 <- as.data.frame(sampleRandom(shrub_crop_3, 5000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE)) %>% mutate(strip = "3")

glimpse(shrub_rsample_3)
hist(shrub_rsample_3$shrub_crop_latlong_agg)
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
range_extent_4 <- extent(-144.3768,  -142.4426,  68.40000, 70.37209) # class: extent
shrub_crop_4 <- crop(x = shrub_crop_latlong_agg, y = range_extent_4) # class: raster layer
res(shrub_crop_4) # 0.007986 0.008370

# random sample 
shrub_rsample_4 <- as.data.frame(sampleRandom(shrub_crop_4, 5000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE)) %>% mutate(strip = "4")

glimpse(shrub_rsample_4)
hist(shrub_rsample_4$shrub_crop_latlong_agg)
# write.csv(shrub_rsample_4, "datasets/berner_data/shrub_rsample_4.csv")

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
range_extent_5 <- extent(-142.4426, -140.5084 ,  68.40000, 70.37209) # class: extent
shrub_crop_5 <- crop(x = shrub_crop_latlong_agg, y = range_extent_5) # class: raster layer
res(shrub_crop_5) # 0.007986 0.008370

# random sample 
shrub_rsample_5 <- as.data.frame(sampleRandom(shrub_crop_5, 5000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE)) %>% mutate(strip = "5")

glimpse(shrub_rsample_5)
hist(shrub_rsample_5$shrub_crop_latlong_agg)
write.csv(shrub_rsample_5, "datasets/berner_data/shrub_rsample_5.csv")

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



### EXTRACTION (North-to-South) ----
# STRIPS a to e
# subdividing cropped map into 5 smaller chunks (horizontal strips from North to South) and extracting biomass 

# extent of the cropped shrub map
st_bbox(shrub_crop_latlong_agg) 
#  xmin       ymin       xmax       ymax 
# -150.17942   66.93202 -140.50837   70.37209 
# Using 68.40000 as ymin since that's where the shrub cover map starts

# Strip (a) -----
range_extent_a <- extent(-150.17942, -140.50837 ,69.97767, 70.37209 ) # class: extent
shrub_crop_a <- crop(x = shrub_crop_latlong_agg, y = range_extent_a) # class: raster layer
res(shrub_crop_a) # 0.007986 0.008370
#create a buffer around the points
sp_buffer <-st_buffer(st_as_sf(points),1000) 
raster::extract(shrub_crop_a, sp_buffer)

# JOE: random sample 
shrub_rsample_a <- as.data.frame(sampleRandom(shrub_crop_a, 5000, buffer = 900, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE)) %>% mutate(strip = "a")
# How do I pick how many points to sample? For now i picked 5000 random samples for each strip - given strip e small size 
# and is the buffer working?

buff_a <- buffer(shrub_crop_a, width=100000)

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

# Model ----
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
(scatter_model_b <- ggplot(shrub_all_random_new_NS, aes(x = long, y = biomass)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm") +
    theme_classic())
# I think I need the ggpredict lmer plot
# random slopes



#####################################################################################
# Experiments (ignore) ----

# Can aggregate using resample() function
#shrub_crop_new_res <- resample(shrub_crop_latlong, precip, method="bilinear") # bilinear method is like mean for aggregate
#res(shrub_crop_new_res)== res(precip) # checking shrub and climate rasters have same resolution
# [1] TRUE TRUE but it doesnt plot

# Splitting raster into tiles 
nx <- 2 # number of tiles for x axis to be split into
ny <- 5 # number of tiles for y axis to be split into
raster_tiles <- splitRaster(shrub_crop_latlong_agg, nx, ny, c(2, 2), path ="datasets/berner_data")
# buffer: 10 pixels along both axes
layout(mat = matrix(seq_len(nx*ny), ncol = nx, nrow = ny))
plotOrder <- c(1,2,3,4,5,6,7,8,9,10)
if (interactive()) invisible(lapply(raster_tiles[plotOrder], plot))

# buffer try
r <- raster(ncol=36,nrow=18)
values(r) <- NA
r[500] <- 1
b <- buffer(r, width=5000000) 
plot(b)


points <- SpatialPoints(shrub_crop_latlong_agg)
b <- buffer(points, 1414.2 )

points <- SpatialPoints(shrub_crop_latlong_agg)
b <- buffer(points, 1414.2 )
plot(shrub_crop_latlong_agg)
lines(b)


