##%######################################################%##
#                                                          #
###              RQ1: SPATIAL ANALYSIS                   ###
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

## RQ1: How is shrub biomass distributed in the focal study area?
# Colour palette credit: David Nichols (WONG palette)

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

### PART 1: SAMPLING -----

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
r3_rsample_001  <- r3_rsample_01 %>%
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
# write.csv(r3_rsample_001, file= "datasets/berner_data/r3_rsample_002.csv") # with 1/3 pixels (c.)

### PART 2: MODELLING ----

# THEME ----

theme_shrub <- function(){ theme(legend.position = "right", 
                                 axis.title.x = element_text(size=25),
                                 axis.text.x  = element_text(vjust=0.5, size=20, colour = "black"), 
                                 axis.title.y = element_text(size=25),
                                 axis.text.y  = element_text(vjust=0.5, size=20, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}



# Loading the random sample dataset
r3_rsample_00 <- read_csv("datasets/berner_data/r3_rsample_00.csv")

hist(r3_rsample_001$biomass) # distribution 
str(r3_rsample_001) # lat and long and biomass numeric
range(r3_rsample_001$biomass) # 9.820163 1003.684387

# Standardising lat and long (explanatory variables)
r3_rsample_001$latitude <- scale(r3_rsample_001$latitude, center = TRUE, scale = TRUE)
r3_rsample_001$longitude <- scale(r3_rsample_001$longitude, center = TRUE, scale = TRUE)

# Model 1. biomass vs lat ----
model_1 <- lm(biomass~latitude, data = r3_rsample_001)
summary(model_1)
# F-statistic: 639.3 on 1 and 3190 DF,  p-value: < 2.2e-16
# slope =  -49.079***

# null model
model_1_null <- lm(biomass~1, data = r3_rsample_001)
AIC(model_1, model_1_null) # delta AIC indicates very diff models

# Quick scatter
(scatter_lat <- ggplot(r3_rsample_001, aes(x = latitude, y = biomass))+
    geom_point(color="#70B1A6", size = 0.1) +
    geom_smooth(method = lm, color ='black', fill = "grey", se=TRUE)+
    labs(x = "\nLatitude", y = "Shrub biomass (g/m2)\n") +
   annotate(geom = "text", x = 2, y = 1100, label="(a)", size = 10) +
   annotate(geom = "text", x = 1, y = 800, label="slope =  -49.079*** ", size = 6) +
         # title = "Shrub biomass decreases with latitude\n") + 
    theme_shrub())

ggsave(file = "output/figures/biomass_vs_lat_scatter.png")

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
predictions_1 <- as.data.frame(predict(model_1, newdata = r3_rsample_001, interval = "confidence")) # this gives overall predictions for the model
model_1_lat <- cbind(r3_rsample_001, predictions_1)

# Plot the predictions 
(predictions_biomass_vs_lat <- (ggplot(model_1_lat, aes(latitude, fit)) + 
                      geom_point(data = model_1_lat, aes(x = latitude, y = biomass), colour =  "#006146", size = 0.5) +
                      stat_smooth(method=lm, colour = "black", size = 2)+
                      geom_line(aes(y=lwr),  color = "#F96E00", linetype = "dashed", size = 0.5)+
                      geom_line(aes(y=upr), color = "#F96E00", linetype = "dashed", size = 0.5)+
                       # annotate(geom = "text", x = 2, y = 1100, label="(a)", size = 15) +
                        annotate(geom = "text", x = 1, y = 800, label="slope =  -49.079*** ", size = 10) +
                      xlab("\nScaled latitude") +
                      ylab(bquote("Shrub biomass "*(g~m^-2)*""))+ 
                      theme_shrub()+
                      theme(axis.title.x =element_text(size=25),
                              axis.title.y =element_text(size=25),
                              axis.text.x = element_text(size=25, hjust = 1),
                              axis.text.y = element_text(size=25, hjust = 1) )) )

ggsave(file = "output/figures/predictions_biomass_vs_lat.png")

# adding icon
lat_logo <- readPNG("lat_icon.png")
raster_lat_logo <- as.raster(lat_logo)
(predictions_biomass_vs_lat <- predictions_biomass_vs_lat + annotation_raster(raster_lat_logo, 0.5, 1.5, 800, 1200))
ggsave(file = "output/figures/predictions_biomass_vs_temp.png")


dev.off()

# Model 2. biomass vs long ----
model_2 <- lm(biomass~longitude, data = r3_rsample_001)
summary(model_2)
# F-statistic: 110.9 on 1 and 3190 DF,  p-value: < 2.2e-16***
# slope -22.021

# null model
model_2_null <- lm(biomass~1, data = r3_rsample_001)
AIC(model_2, model_2_null) # delta AIC indicates very different models


# Quick scatter
(scatter_lon <- ggplot(r3_rsample_001, aes(x = longitude, y = biomass)) +
    geom_point(color="skyblue", size = 0.01) +
    geom_smooth(method = lm, colour='black') +
    labs(x = "\nLongitude", y = "Shrub biomass (kg/m2)\n") +  
   annotate(geom = "text", x = 2, y = 1250, label="(b)", size = 15) +
  annotate(geom = "text", x = 1, y = 900, label="slope = -22.021*** ", size = 10) +
         # title = "Shrub biomass decreases with longitude\n") + 
    theme_shrub())

ggsave(file = "output/figures/biomass_vs_long_scatter.png")

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
predictions_2 <- as.data.frame(predict(model_2, newdata = r3_rsample_001, interval = "confidence")) # this gives overall predictions for the model
model_2_long <- cbind(r3_rsample_001, predictions_2)

# Plot the predictions 
(predictions_biomass_vs_long <- (ggplot(model_2_long, aes(longitude, fit)) + 
                      geom_point(data = model_2_long, aes(x= longitude, y = biomass), colour =  "#006146", size = 0.5) +
                      stat_smooth(method=lm, colour = "black", size = 2)+
                      geom_line(aes(y=lwr),  color = "#F96E00", linetype = "dashed", size = 0.5)+
                      geom_line(aes(y=upr), color = "#F96E00", linetype = "dashed", size = 0.5)+
                       # annotate(geom = "text", x = 2, y = 1100, label="(b)", size = 15) +
                        annotate(geom = "text", x = 1, y = 800, label="slope = -22.021*** ", size = 10) +
                        xlab("\nScaled longitude") +
                        ylab(bquote("Shrub biomass "*(g~m^-2)*""))+ 
                        theme_shrub()+
                        theme(axis.title.x =element_text(size=25),
                              axis.title.y =element_text(size=25),
                              axis.text.x = element_text(size=25, hjust = 1),
                              axis.text.y = element_text(size=25, hjust = 1) )) )

ggsave(file = "output/figures/predictions_biomass_vs_long.png")

# adding icon
long_logo <- readPNG("long_icon.png")
raster_long_logo <- as.raster(long_logo)
(predictions_biomass_vs_long <- predictions_biomass_vs_long + annotation_raster(raster_long_logo, 0, 2, 900, 1150))
ggsave(file = "output/figures/predictions_biomass_vs_temp.png")


# Panel latlong ----
# Panel of scatters 
panel_title <- text_grob("Shrub biomass decreases with latitude and longitude",
                         size = 18, face = "bold")

(panel_latlong <- grid.arrange(arrangeGrob(scatter_lat, scatter_lon,
                                           ncol = 2))) # Sets number of panel columns
                              #  top = panel_title  # Adding panel title

ggsave(panel_latlong, file = "output/figures/panel_latlong.png", width = 18, height = 9)

(panel_latlong_predictions <- grid.arrange(arrangeGrob(predictions_biomass_vs_lat, predictions_biomass_vs_long,
                                           ncol = 2))) # Sets number of panel columns

ggsave(panel_latlong_predictions, file = "output/figures/panel_latlong_predictions.png", width = 18, height = 9)


# Model  biomass vs long*lat ----
model_2a <- lm(biomass~longitude*latitude, data = r3_rsample_00)
summary(model_2a)
# F-statistic:  1058 on 3 and 9575 DF,  p-value: < 2.2e-16
cor.test( r3_rsample_00$latitude,r3_rsample_00$longitude, method = "pearson")
# t = -20.437, df = 9577, p-value < 2.2e-16 
# lat and long are correlated 
# doesnt mean much. As latitude increases longitude decreases


# BIOMASS LEVELS ----

# Categorising into high-medium-low level
mean(r3_rsample_001$biomass) 
#  267.5807 g/m2 mean biomass
range(r3_rsample_001$biomass)
#  9.820163 1003.684387
quantile(r3_rsample_001$biomass)
# 0%         25%         50%         75%        100% 
#  9.820163  170.630482  257.406342  347.062210 1003.684387 

r3_rsample_categ <- r3_rsample_001 %>%
  mutate(biomass_level = case_when (biomass < 170.630482     ~ 'Low', # lower than mean biomass
                                    biomass > 170.630482    & biomass < 347.062210 ~ 'Medium', 
                                    biomass > 347.062210 ~ 'High')) %>% 
  mutate(biomass_level_0 = case_when (biomass_level == 'Low' ~ 1, # 1 = low level
                                    biomass_level == 'Medium' ~ 2, # 2 = medium level
                                    biomass_level == 'High'~ 3)) %>% # 3 = high level
 dplyr::select(latitude, longitude, biomass, biomass_level, biomass_level_0)

r3_rsample_categ$biomass_level <- as.factor(as.character(r3_rsample_categ$biomass_level))

# reordeing factor levels 
r3_rsample_categ$biomass_level <- factor(r3_rsample_categ$biomass_level,levels=c("Low", "Medium", "High"),
                          labels = c("Low", "Medium", "High"),
                          ordered = T)


# Histogram of biomass level
(hist_high_medium_low <- r3_rsample_categ %>%
    ggplot(aes(x = biomass, fill = biomass_level)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 60) +
    geom_vline(aes(xintercept = mean(biomass)),            
               colour = "black", linetype = "dashed", size = 1) +
    annotate(geom = "text", x = 450, y = 200, label="mean = 267.6", size = 14) +
    geom_curve(aes(x = 470, y = 210, xend = mean(biomass) + 2, yend = 210),
               arrow = arrow(length = unit(0.07, "inch")), size = 1,
               color = "grey30", curvature = 0.3) +
    ylab("Frequency\n") +
    xlab(bquote("Shrub biomass "*(g~m^-2)*""))+ 
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    theme_shrub()+
    theme( axis.title.x =element_text(size=25),
           axis.title.y =element_text(size=25),
           axis.text.x = element_text(size=25, hjust = 1),
           axis.text.y = element_text(size=25, hjust = 1),
      legend.text = element_text(size=20),
          legend.title = element_text(size=25),
          legend.position = "bottom"))

ggsave(file = "output/figures/hist_high_medium_low.png")

# adding shrub logo
shrub_logo <- readPNG("team_shrub_logo.png")
raster_logo <- as.raster(shrub_logo)
(histogram <- hist_high_medium_low + annotation_raster(raster_logo, 750, 900, 150, 200))
ggsave(file = "output/figures/histogram.png")

# Random slope and intercept biomass level across latitudes ----
# Standardising explanatory variables
r3_rsample_categ$latitude <- scale(r3_rsample_categ$latitude, center = TRUE, scale = TRUE)
r3_rsample_categ$longitude <- scale(r3_rsample_categ$longitude, center = TRUE, scale = TRUE)

level_rs <- lmer(biomass ~ latitude + (1 + latitude|biomass_level), data = r3_rsample_categ)
summary(level_rs)
#  latitude estimate:  -16.298    
r.squaredGLMM(level_rs) # same as below
r2_nakagawa(level_rs)

# null model
level_rs_null <- lm(biomass ~ 1, data = r3_rsample_categ)
AIC(level_rs, level_rs_null)

stargazer(level_rs, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Random intercepts and slopes 
predict_levels <- ggpredict(level_rs , terms = c("latitude", "biomass_level"), type = "re") 
(levels_rand_slopes <- ggplot(predict_levels, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE, size=1.5)  +
    scale_colour_manual(values=c("#F0E442", "#E69F00", "#009E73"), name = "Biomass level") + 
    theme(legend.position = "bottom") +
    annotate(geom = "text", x = 2, y = 510, label="(a)", size = 15) +
    xlab("\nScaled latitude") +
    ylab(bquote("Shrub biomass "*(g~m^-2)*""))+ 
    theme_shrub()+ 
    theme(legend.text=element_text(size=25),
          legend.title=element_text(size=30)))

ggsave(levels_rand_slopes, file = "output/figures/levels_rand_slopes.png")


# Random slope and intercept biomass level across longitudes ----
level_rs_long <- lmer(biomass ~ longitude + (1 + longitude|biomass_level), data = r3_rsample_categ)
summary(level_rs_long)
# long  estimate:  3.245  (not sig)
r.squaredGLMM(level_rs_long) # same as below
r2_nakagawa(level_rs_long)

# null model
level_rs_long_null <- lm(biomass ~ 1, data = r3_rsample_categ)
AIC(level_rs_long, level_rs_long_null)

stargazer(level_rs_long, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Random intercepts and slopes 
predict_levels_long <- ggpredict(level_rs_long , terms = c("longitude", "biomass_level"), type = "re") 

(levels_rand_slopes_long <- ggplot(predict_levels_long, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE, size = 1.5)  +
    scale_colour_manual(values=c("#F0E442", "#E69F00", "#009E73"), name = "Biomass level") + 
    theme(legend.position = "bottom") +
    xlab("\nScaled longitude") +
    ylab(bquote("Shrub biomass "*(g~m^-2)*""))+ 
    annotate(geom = "text", x = 2, y = 510, label="(b)", size = 15) +
    theme_shrub()+ 
    theme(legend.text=element_text(size=25),
          legend.title=element_text(size=30)))
          

ggsave(levels_rand_slopes_long, file = "output/figures/levels_rand_slopes_long.png")

(panel_slopes_levels <- ggarrange(levels_rand_slopes, levels_rand_slopes_long,
                                                  ncol = 2, common.legend = TRUE, legend="bottom"))# Sets number of panel columns


ggsave(panel_slopes_levels, file = "output/figures/panel_slopes_levels.png", height = 10, width=18)

# Filter for high/medium/low biomass separately
# 1. HIGH  ----
r3_high_biomass <- r3_rsample_categ %>% filter (biomass_level == "High")
model_lat_high <- lm(biomass~latitude, data = r3_high_biomass )
summary(model_lat_high)
# F-statistic: 119.7 on 1 and 796 DF,  p-value: < 2.2e-16***

# null model 
model_lat_high_null <- lm(biomass~1, data = r3_high_biomass )
AIC(model_lat_high, model_lat_high_null)

(scatter_high_lat <- ggplot(r3_high_biomass, aes(x = latitude, y = biomass)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLatitude", y = "High biomass (g/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))

model_long_high <- lm(biomass~longitude, data = r3_high_biomass )
summary(model_long_high) 
# F-statistic: 177.2 on 1 and 2393 DF,  p-value: < 2.2e-16***

# null model 
model_long_high_null <- lm(biomass~1, data = r3_high_biomass )
AIC(model_long_high, model_long_high_null)

(scatter_high_long <- ggplot(r3_high_biomass, aes(x = longitude, y = biomass)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLongitude", y = "High biomass (kg/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))

# 2. MEDIUM -----
r3_med_biomass <- r3_rsample_categ %>% filter (biomass_level == "Medium")
model_lat_med <- lm(biomass~latitude, data = r3_med_biomass )
summary(model_lat_med)
# F-statistic: 207.7 on 1 and 4787 DF,  p-value: < 2.2e-16***

# null model
model_lat_med_null <- lm(biomass~1, data = r3_med_biomass )
AIC(model_lat_med, model_lat_med_null)

(scatter_med_lat <- ggplot(r3_med_biomass, aes(x = latitude, y = biomass)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLatitude", y = "Medium biomass (kg/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))

model_long_med <- lm(biomass~longitude, data = r3_med_biomass )
summary(model_long_med) 
#F-statistic: 358.1 on 1 and 4787 DF,  p-value: < 2.2e-16***

# null model
model_long_med_null <- lm(biomass~1, data = r3_med_biomass )
AIC(model_long_med, model_long_med_null)

(scatter_med_long <- ggplot(r3_med_biomass, aes(x = longitude, y = biomass)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLongitude", y = "Medium biomass (kg/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))


# 3. LOW -----
r3_low_biomass <- r3_rsample_categ %>% filter (biomass_level == "Low")
model_lat_low <- lm(biomass~latitude, data = r3_low_biomass )
summary(model_lat_low)
# F-statistic: 22.18 on 1 and 2393 DF,  p-value: 2.624e-06***

# null model
model_lat_low_null <- lm(biomass~1, data = r3_low_biomass )
AIC(model_lat_low, model_lat_low_null)

(scatter_low_lat <- ggplot(r3_low_biomass, aes(x = latitude, y = biomass)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLatitude", y = "Low biomass (kg/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))

model_long_low <- lm(biomass~longitude, data = r3_low_biomass )
summary(model_long_low) 
# F-statistic: 79.16 on 1 and 2393 DF,  p-value: < 2.2e-16***

# null model
model_long_low_null <- lm(biomass~1, data = r3_low_biomass )
AIC(model_long_low, model_long_low_null)

(scatter_low_long <- ggplot(r3_low_biomass, aes(x = longitude, y = biomass)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLongitude", y = "Low biomass (kg/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))


(scatter <- ggplot(r3_rsample_categ, aes(x = latitude, y = biomass, colour = biomass_level)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    facet_wrap(~biomass_level)+
    labs(x = "\nLongitude", y = "Low biomass (kg/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))
                          



# Panel latlong levels -----
(panel_latlong_levels <- grid.arrange(arrangeGrob(scatter_low_lat, scatter_low_long,
                                                  scatter_med_lat, scatter_med_long,
                                                  scatter_high_lat, scatter_high_long,
                                           ncol = 2))) # Sets number of panel columns

ggsave(panel_latlong_levels,  file="output/figures/panel_latlong_levels.png", height = 16, width = 15)

# Kmeans ----
# Kmeans clustering: Biomass level ~ lat 
set.seed(99)
clusters <- kmeans(na.omit(r3_rsample_categ$biomass), centers = 3, nstart = 25)
cluster.df <- as.data.frame(clusters$cluster)
cluster_plot <- fviz_cluster(clusters, data = r3_rsample_categ)

r3_rsample_categ_clust <- r3_rsample_categ %>%
  add_column(cluster = clusters$cluster)

# adding cluster column to dataframe

r3_rsample_categ_clust$cluster <- as.factor(as.character(r3_rsample_categ_clust$cluster ))
str(r3_rsample_categ_clust$cluster)

r3_rsample_categ_clust <- r3_rsample_categ_clust %>%
  mutate(cluster_level = case_when (cluster == "2" ~ 'Low', # 1 = low level
                                      cluster == "1" ~ 'Medium', # 2 = medium level
                                      cluster == "3" ~ 'High')) 
r3_rsample_categ_clust$cluster_level <- as.factor(as.character(r3_rsample_categ_clust$cluster_level ))

# reordeing factor levels 
r3_rsample_categ_clust$cluster_level <- factor(r3_rsample_categ_clust$cluster_level,
                                               levels=c("Low","Medium", "High"),
                                         labels = c("Low","Medium", "High"),
                                         ordered = T)

(scatter_high_medium_low_lat <- ggplot(r3_rsample_categ_clust) +
    geom_point(aes(x = latitude, y = biomass, colour = cluster_level), size = 0.3) +
   scale_colour_manual(values = c("Low"= "tan", "Medium" = "yellow", "High"= "green4"), name = "Biomass level")+
     geom_smooth(aes(x = latitude, y = biomass), colour = "black", method = "lm") +
    facet_wrap(~cluster_level) +
   #  scale_fill_manual(name = "Biomass level", values=c( "tan", "yellow", "green4")) +
    labs(x= "\nLatitude", y = "Shrub biomass (kg/m2)\n") +
    theme_shrub()+
    theme(axis.text.x = element_text(vjust=0.5, angle = 45, size=12, colour = "black"), 
))

ggsave(file = "output/figures/scatter_high_medium_low_lat.png")

(scatter_high_medium_low_long <- ggplot(r3_rsample_categ_clust) +
    geom_point(aes(x = longitude, y = biomass, colour = cluster_level), size = 0.3) +
    scale_colour_manual(values = c("Low"= "tan", "Medium" = "yellow", "High"= "green4"), name = "Biomass level")+
    geom_smooth(aes(x = longitude, y = biomass), colour = "black", method = "lm") +
    facet_wrap(~cluster_level) +
    #  scale_fill_manual(name = "Biomass level", values=c( "tan", "yellow", "green4")) +
    labs(x= "\nLongitude", y = "Shrub biomass (kg/m2)\n") +
    theme_shrub() +
  theme(axis.text.x = element_text(vjust=0.5, angle = 45, size=12, colour = "black"), 
  ))


ggsave(file = "output/figures/scatter_high_medium_low_long.png")
dev.off()

# Plotting kmeans
copy_raster <- r3_latlong_agg
# Now replace raster cell values with 
# array
copy_raster[] <- clusters$cluster
plot(copy_raster, main = "Kmeans", col = viridis_pal(option = "D")(3))




# END -----




