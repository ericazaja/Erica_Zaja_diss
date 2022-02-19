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

# LOADING DATA -----
## SHRUB DATA: from Berner et al 2018
# Loading raster of shrub biomass (g/m2) on Alaskan North Slope  
shrub_agb_p50 <- raster("datasets/berner_data/shrub_agb_p50.tif") 
# Using the best-estimates: the 50th percentile of the 1,000 permutations

### PCH CORE RANGE DATA: from Porcupine Caribou Management Board (2016)
# Loading polygon of PCH range 
PCH_core_range <- st_read("datasets/PCH_Core_Range_2016/PCH_Core_Range_2016.shp") #loading data
st_bbox(PCH_core_range) # extent of the PCH range

### CROPPING -----
# Crop and mask ---- 
r2 <- crop(shrub_agb_p50, extent(PCH_core_range))
r3 <- mask(r2, PCH_core_range)
plot(r3) 
plot(PCH_core_range, add=TRUE, lwd=2)

# changing coordinates to lat long
r3_latlong <- projectRaster(r3, crs="+init=EPSG:4326", xy = TRUE)
# writeRaster(r3_latlong, "datasets/berner_data/r3_latlong.tif")

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

## AGGREGATION ----
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
res(r3_latlong_agg)
# 0.007986 0.008370
# divided into 1km x 1km grid cells 
# diagonal of a grid square = 1414.2 m
# buffer = diagonal of grid cell means that no point will be taken from same grid cell

# buffered random sampling
r3_rsample_0 <- as.data.frame(sampleRandom(r3_latlong_agg, 20000, buffer = 1414.2, na.rm=TRUE, ext=NULL, 
                                              cells=TRUE, rowcol=FALSE, xy = TRUE))

hist(r3_rsample_0$shrub_agb_p50)

# checking buffer works
r3_rsample_01 <- r3_rsample_0  %>% 
  mutate(r3_rsample_0 , Distance = distHaversine(cbind(x, y),
                                                   cbind(lag(x), lag(y))))

r3_rsample_01 <- r3_rsample_01 %>% 
  mutate(buff = case_when(Distance >= 1414.2 ~ "T", Distance < 1414.2 ~ "F"))

r3_rsample_01 <- r3_rsample_01 %>%  filter(buff %in% c("T"))
# only keeping obseervations where buff worked

unique(r3_rsample_01$buff) # T

# Cleaning and making a gridcell column the new dataframe
r3_rsample_00 <- r3_rsample_01 %>%
  rename (cell_ID = "cell", 
          lat = "y", long = "x", 
          biomass = "shrub_agb_p50") %>%
  mutate(lat = plyr::round_any(lat, 0.5, f = floor),
         long = ifelse(long > 0, plyr::round_any(long, 0.5, f = floor), plyr::round_any(long, 0.5, f = ceiling))) %>% 
  mutate(gridcell = paste0("_", lat, "_", long))%>%
  select(cell_ID, long, lat, biomass, gridcell)

# write.csv(r3_rsample_00, file= "datasets/berner_data/r3_rsample_00.csv")

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
hist(r3_rsample_00$biomass) # distribution 

# Model 1. biomass vs lat ----
model_1 <- lmer(biomass~lat + (1|gridcell), data = r3_rsample_00)
summary(model_1)
# total variance: 
# variance for gridcell =  
# amount of variance explained by random effect: 
# I.e. differences between grid cells explain  of the variance 
# that’s “left over” after the variance explained by our fixed effect (long).
# estimate for latitude (exp variable =   ) 
# not significant effect of long on biomass

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

# Extracting model predictions 
pred_model_1 <- ggpredict(model_a, terms = c("lat"))  # this gives overall predictions for the model
# write.csv(pred_model_1, file = "datasets/pred_model_1.csv")

# Plot the predictions 
(plot_model_1 <- (ggplot(pred_model_a) + 
                   geom_line(aes(x = x, y = predicted)) +          # slope
                   geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                               fill = "lightgrey", alpha = 0.5) +  # error band
                   geom_point(data = r3_rsample_00,                      # adding the raw data 
                              aes(x = lat, y = biomass), size = 0.5) + 
                   labs(x = "Latitude", y = "Shrub biomass (g/m2)", 
                        title = "Shrub biomass decreases with latitude") + 
                   theme_shrub())
)

dev.off()

# Model 2. biomass vs long ----
model_2 <- lmer(biomass~long + (1|gridcell), data = r3_rsample_00)
summary(model_2)
# total variance: 
# variance for gridcell =  
# amount of variance explained by random effect: 
# I.e. differences between grid cells explain  of the variance 
# that’s “left over” after the variance explained by our fixed effect (long).
# estimate for latitude (exp variable =   ) 
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
(plot_model_2 <- (ggplot(pred_model_2) + 
                   geom_line(aes(x = x, y = predicted)) +          # slope
                   geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                               fill = "lightgrey", alpha = 0.5) +  # error band
                   geom_point(data = r3_rsample_00,                      # adding the raw data 
                              aes(x = long, y = biomass), size = 0.5) + 
                   labs(x = "Longitude", y = "Shrub biomass (g/m2)", 
                        title = "Shrub biomass does not vary with longitude") + 
                   theme_shrub())
)

# Hih-medium-low ----
### CATEGORISE into HIGH/MEDIUM/LOW biomass 
mean(r3_rsample_00$biomass)
# 267.1607 mean biomass
range(r3_rsample_00$biomass)
# 4.709974 1144.706055

r3_rsample_categ <- r3_rsample_00 %>%
  mutate(biomass_level = case_when (biomass < 267.1607 ~ 'Low', # lower than mean biomass
                                    biomass >= 267.1607 & biomass < 400 ~ 'Medium', 
                                    biomass >= 400 ~ 'High'))

# Hist 
(hist_random <- r3_rsample_categ %>%
    ggplot(aes(x = biomass, fill = biomass_level )) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 60) +
    geom_vline(aes(xintercept = mean(biomass)),            
               colour = "red", linetype = "dashed", size = 1) +
    scale_fill_manual(values=c( "green4", "tan", "yellow")) +
    theme_shrub())





