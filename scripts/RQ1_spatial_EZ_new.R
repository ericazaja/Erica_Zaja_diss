##%######################################################%##
#                                                          #
###              RQ1: SPATIAL ANALYSIS                  ####
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

## RQ1: How does shrub biomass vary with latitude and longitude within the study area? 
## What areas within the study area have high-medium-low shrub biomass cover?

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


##  LOADING DATA -----

#1. SHRUB DATA: from Berner et al 2018
# Loading raster of shrub biomass (kg/m2) on Alaskan North Slope  
shrub_agb_p50 <- raster("datasets/berner_data/shrub_agb_p50.tif") 
# Using the best-estimates: the 50th percentile of the 1,000 permutations

# 2. PCH CORE RANGE DATA: from Porcupine Caribou Management Board (2016)
# Loading polygon of PCH range 
PCH_core_range <- st_read("datasets/PCH_Core_Range_2016/PCH_Core_Range_2016.shp") #loading data
st_bbox(PCH_core_range) # extent of the PCH range

## CROPPING -----

# Cropping shrub raster to the PCH range and mask 
r2 <- crop(shrub_agb_p50, extent(PCH_core_range))
r3 <- mask(r2, PCH_core_range)
plot(r3) # cropped raster
plot(PCH_core_range, add=TRUE, lwd=2)

# transforming CRS of cropped map from proj = aea to proj = lalong
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
    theme_shrub() +  # Remove ugly grey background
    xlab("\nLongitude") +
    ylab("Latitude\n") +
    ggtitle("Shrub biomass cover (kg/m2) of the PCH alaskan range\n") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 30, hjust = 1)))  # rotates x axis text

## AGGREGATION ----

# aggregate shrub data to coarser resolution before extraction using aggregate function()
# factor chosen dividing climate cell resolution 0.008333333 x 0.008333333 by the resolution of the cropped shrub map (latlong)
r3_latlong_agg <- aggregate(r3_latlong, fact=c(11.47842,30.8642), fun=mean, expand = TRUE) 
# writeRaster(r3_latlong_agg, "datasets/berner_data/r3_latlong_agg.tif")
# r3_latlong_agg <- raster("datasets/berner_data/r3_latlong_agg.tif") # loading raster

# checking new resolution
res(r3_latlong_agg)
# 0.007986 x 0.008370 
# not EXACTLY the same as climate resolution but close enough? 

# RANDOM SAMPLE WHOLE MAP ----

# Measuring area of raster
# get sizes of all cells in raster [km2]
cell_size <- area(r3_latlong_agg, na.rm=TRUE, weights=FALSE)
# delete NAs from vector of all raster cells
# NAs lie outside of the rastered region, can thus be omitted
cell_size <- cell_size[!is.na(cell_size)] # 0.2815663
#compute area [km2] of all cells in geo_raster
raster_area <-length(cell_size)*median(cell_size)
#print area of shrub map according to raster object
print(paste("Area of PCH Alaskan range (raster)", round(raster_area, digits=1),"km2"))
# [1] "Area of PCH Alaskan range (raster) is 9583.6 km2"

# I think this means there are 9583.6 cells of ~1km x 1km 
# sampling 2 pixels per raster cell: 9583.6 cells  *2 = 19167.2 --> I sample 20000 pixels

# deciding on buffer distance
res(r3_latlong_agg)
# 0.007986 0.008370 degrees
# ie. raster divided into 1km x 1km grid cells 
# diagonal of a grid square = 1414.2 m
# buffer = diagonal of grid cell means that no point will be taken from same grid cell
# But I want 2 pixels per raster grid cell, so I divide 1414.2/2 = 707.1

# buffered random sampling
r3_rsample_0 <- as.data.frame(sampleRandom(r3_latlong_agg, 20000, buffer = 707.1, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE)) 
hist(r3_rsample_0$r3_latlong_agg) # checking distribution

r3# trying to sample 30000 pixels to see if the distribution is different 
r3_rsample_0_try <- as.data.frame(sampleRandom(r3_latlong_agg, 30000, buffer = 707.1, na.rm=TRUE, ext=NULL, 
                                           cells=TRUE, rowcol=FALSE, xy = TRUE)) # 30000 pixels 
hist(r3_rsample_0_try$r3_latlong_agg) # checking distribution - looks similar to the other histogram

# checking buffer works
# calculating distance between points (x and y coordinates)
r3_rsample_01 <- r3_rsample_0  %>% 
  mutate(r3_rsample_0 , Distance = distHaversine(cbind(x, y),
                                                   cbind(lag(x), lag(y))))

# If istance between points > buffer distance, buffer works
r3_rsample_01 <- r3_rsample_01 %>% 
  mutate(buff = case_when(Distance >= 707.1 ~ "T", Distance < 707.1 ~ "F"))

r3_rsample_01 <- r3_rsample_01 %>%  filter(buff %in% c("T")) # only keeping obseervations where buff worked
unique(r3_rsample_01$buff) # T

# Cleaning radnom sample dataframe and making a gridcell column the new dataframe
r3_rsample_00 <- r3_rsample_01 %>%
  rename (cell_ID = "cell", 
          latitude = "y", longitude = "x", 
          biomass = "r3_latlong_agg") %>%
  mutate(lat = plyr::round_any(latitude, 0.5, f = floor),
         long = ifelse(longitude > 0, plyr::round_any(longitude, 0.5, f = floor), plyr::round_any(longitude, 0.5, f = ceiling))) %>% 
  mutate(gridcell = paste0("_", lat, "_", long))%>%
  select(cell_ID, latitude, longitude, long, lat, biomass, gridcell)

# write.csv(r3_rsample_00, file= "datasets/berner_data/r3_rsample_00.csv")

# THEME ----

theme_shrub <- function(){ theme(legend.position = "right",
                                 axis.title.x = element_text(face="bold", size=18),
                                 axis.text.x  = element_text(vjust=0.5, size=15, colour = "black"), 
                                 axis.title.y = element_text(face="bold", size=18),
                                 axis.text.y  = element_text(vjust=0.5, size=15, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}

# MODELLING ----

hist(r3_rsample_00$biomass) # distribution 
str(r3_rsample_00) # lat and long and biomass numeric

# Model 1. biomass vs lat ----
model_1 <- lm(biomass~latitude, data = r3_rsample_00)
summary(model_1)
# F-statistic:  4134 on 1 and 19995 DF,  p-value: < 2.2e-16***

# Quick scatter
(scatter_1 <- ggplot(r3_rsample_00, aes(x = latitude, y = biomass))+
    geom_point(color='#2980B9', size = 0.1) +
    geom_smooth(method = lm, color ='black', fill = "grey", se=TRUE)+
    labs(x = "\nLatitude", y = "Shrub biomass (kg/m2)\n", 
         title = "Shrub biomass decreases with latitude\n") + 
    theme_shrub())

#ggsave(file = "output/figures/biomass_vs_lat_scatter.png")

# Checking model 1 assumptions 
plot(model_1)
qqnorm(resid(model_1))
qqline(resid(model_1))  # points fall nicely onto the line - good!

# Output table model 1
stargazer(model_1, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
pred_model_1 <- ggpredict(model_1, terms = c("latitude"))  # this gives overall predictions for the model
write.csv(pred_model_1, file = "datasets/pred_model_1.csv")

# Plot the predictions 
(biomass_vs_lat <- (ggplot(pred_model_1) + 
                   geom_line(aes(x = x, y = predicted)) +          # slope
                   geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                               fill = "lightgrey", alpha = 0.5) +  # error band
                     geom_point(data = r3_rsample_00,                      # adding the raw data 
                              aes(x = latitude, y = biomass), size = 0.5) + 
                   labs(x = "\nLatitude", y = "Shrub biomass (kg/m2)\n", 
                        title = "Shrub biomass decreases with latitude\n") + 
                   theme_shrub())
)

# ggsave(file = "output/figures/biomass_vs_lat.png")

dev.off()

# Model 2. biomass vs long ----
model_2 <- lm(biomass~longitude, data = r3_rsample_00)
summary(model_2)
# F-statistic: 552.4 on 1 and 19995 DF,  p-value: < 2.2e-16***

# Quick scatter
(scatter_2 <- ggplot(r3_rsample_00, aes(x = longitude, y = biomass)) +
    geom_point(color='#2980B9', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLongitude", y = "Shrub biomass (kg/m2)\n", 
         title = "Shrub biomass decreases with longitude\n") + 
    theme_shrub())

# ggsave(file = "output/figures/biomass_vs_long_scatter.png")

# Checking model 2 assumptions 
plot(model_2)
qqnorm(resid(model_2))
qqline(resid(model_2))  # points fall nicely onto the line - good!

# Output table model 2 
stargazer(model_2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
pred_model_2 <- ggpredict(model_2, terms = c("longitude"))  # this gives overall predictions for the model
# write.csv(pred_model_2, file = "datasets/pred_model_2.csv")

# Plot the predictions 
(biomass_vs_long <- (ggplot(pred_model_2) + 
                   geom_line(aes(x = x, y = predicted)) +          # slope
                   geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                               fill = "lightgrey", alpha = 0.5) +  # error band
                   geom_point(data = r3_rsample_00,                      # adding the raw data 
                              aes(x = longitude, y = biomass), size = 0.5) + 
                   labs(x = "\nLongitude", y = "Shrub biomass (kg/m2)\n", 
                        title = "Shrub biomass decreases with longitude\n") + 
                   theme_shrub())
)

# ggsave(file = "output/figures/biomass_vs_long.png")


# BIOMASS LEVELS ----

# Categorising into high-medium-low level
mean(r3_rsample_00$biomass)
# 267.1607 kg/m2 mean biomass
range(r3_rsample_00$biomass)
# 4.709974 1144.706055

r3_rsample_categ <- r3_rsample_00 %>%
  mutate(biomass_level = case_when (biomass < 267.1607 ~ 'Low', # lower than mean biomass
                                    biomass >= 267.1607 & biomass < 400 ~ 'Medium', 
                                    biomass >= 400 ~ 'High')) %>% 
  mutate(biomass_level_0 = case_when (biomass_level == 'Low' ~ 1, # 1 = low level
                                    biomass_level == 'Medium' ~ 2, # 2 = medium level
                                    biomass_level == 'High'~ 3)) %>% # 3 = high level
 dplyr::select(latitude, longitude, biomass, biomass_level, biomass_level_0)

r3_rsample_categ$biomass_level <- as.factor(as.character(r3_rsample_categ$biomass_level))


# Histogram of biomass level
(hist_high_medium_low <- r3_rsample_categ %>%
    ggplot(aes(x = biomass, fill = biomass_level)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 60) +
    geom_vline(aes(xintercept = mean(biomass)),            
               colour = "red", linetype = "dashed", size = 1) +
    labs(x = "\nShrub biomass (kg/m2)", y = "Frequency\n") +
    scale_fill_manual(values=c( "green4", "tan", "yellow")) +
    theme_shrub())

# ggsave(file = "output/figures/hist_high_medium_low.png")

# Filter for high/medium/low biomass separately
# 1. HIGH 
r3_high_biomass <- r3_rsample_categ %>% filter (biomass_level == "High")
model_lat_high <- lm(biomass~latitude, data = r3_high_biomass )
summary(model_lat_high)
# F-statistic: 382.7 on 1 and 2445 DF,  p-value: < 2.2e-16***
model_long_high <- lm(biomass~longitude, data = r3_high_biomass )
summary(model_long_high) 
# F-statistic: 190.7 on 1 and 2445 DF,  p-value: < 2.2e-16 ***

# 2. MEDIUM
r3_med_biomass <- r3_rsample_categ %>% filter (biomass_level == "Medium")
model_lat_med <- lm(biomass~latitude, data = r3_med_biomass )
summary(model_lat_med)
# F-statistic: 157.2 on 1 and 7034 DF,  p-value: < 2.2e-16
model_long_med <- lm(biomass~longitude, data = r3_med_biomass )
summary(model_long_med) 
#F-statistic: 2.245 on 1 and 7034 DF,  p-value: 0.1341


# 3. LOW 
r3_low_biomass <- r3_rsample_categ %>% filter (biomass_level == "Low")
model_lat_low <- lm(biomass~latitude, data = r3_low_biomass )
summary(model_lat_low)
# F-statistic: 199.6 on 1 and 10512 DF,  p-value: < 2.2e-16***
model_long_low <- lm(biomass~longitude, data = r3_low_biomass )
summary(model_long_low) 
# F-statistic: 174.1 on 1 and 10512 DF,  p-value: < 2.2e-16***

# Vary this scatterto visualise relationships
(scatter_high <- ggplot(r3_high_biomass, aes(x = latitude, y = biomass)) +
    geom_point(color='#2980B9', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLatitude OR Longitude", y = "Shrub biomass (kg/m2)\n", 
         title = "High/Med/Low Shrub biomass decreases OR increases with longitude\n") + 
    theme_shrub())

# Binomial model ----
r3_rsample_categ_bi <- r3_rsample_00 %>%
  mutate(biomass_level = case_when (biomass < 267.1607 ~ 'Low', # lower than mean biomass
                                    biomass >= 267.1607 ~ 'High' ))

r3_rsample_categ_bi$biomass_level <- as.factor(as.character(r3_rsample_categ_bi$biomass_level))
biomass_glm <- glm(biomass_level ~latitude, data =r3_rsample_categ_bi, family = binomial())
summary(biomass_glm)
stargazer(biomass_glm, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

box_high_med_low <- ggplot(r3_rsample_categ_bi, aes(x = latitude, y = biomass_level)) +
  geom_boxplot() +
  labs(x = "\nLatitude", y = "Shrub biomass level \n", 
       title = "More high biomass shrubs at lower latitudes \n") + 
  theme_shrub()

ggsave(file = "output/figures/box_high_med_low.png")


# KMEANS ----
# Kmeans clustering: Biomass level ~ lat 

clusters <- kmeans(r3_rsample_categ, centers = 3, nstart = 25)
cluster.df <- as.data.frame(clusters$cluster)
cluster_plot <- fviz_cluster(clusters, data = r3_rsample_categ)

r3_rsample_categ <- r3_rsample_00 %>%
  mutate(biomass_level = case_when (biomass < 267.1607 ~ 'Low', # lower than mean biomass
                                    biomass >= 267.1607 & biomass < 400 ~ 'Medium', 
                                    biomass >= 400 ~ 'High'))

r3_rsample_categ_clust <- r3_rsample_categ %>%
  add_column(cluster = clusters$cluster )

# adding cluster column to dataframe

r3_rsample_categ_clust$cluster <- as.factor(as.character(r3_rsample_categ_clust$cluster ))
str(r3_rsample_categ_clust$cluster)

(scatter_high_medium_low <- ggplot(r3_rsample_categ_clust) +
    geom_point(aes(x = lat, y = biomass, colour = cluster), size = 0.5) +
    scale_colour_manual(values = c("1"= "tan", "2" = "yellow", "3"= "green4"))+
     geom_smooth(aes(x = lat, y = biomass, colour = cluster), method = "lm") +
    labs(x= "Latitude\n", y = "\nShrub biomass (kg/m2)") +
    theme_shrub())

ggsave(file = "output/figures/scatter_high_medium_low.png")
dev.off()
# END -----




