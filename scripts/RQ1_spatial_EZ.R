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
library(rworldmap)

## Setting a theme ----
shrub.theme <- theme(legend.position = "right",
                     axis.title.x = element_text(face="bold", size=20),
                     axis.text.x  = element_text(vjust=0.5, size=18, colour = "black"), 
                     axis.title.y = element_text(face="bold", size=20),
                     axis.text.y  = element_text(vjust=0.5, size=18, colour = "black"),
                     panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                     panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                     plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                     plot.margin = unit(c(1,1,1,1), units = , "cm"))


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
# PCH core range data from Porcupine Caribou Management Board (2016)

# Loading polygon of PCH range 
PCH_core_range <- st_read("datasets/PCH_Core_Range_2016/PCH_Core_Range_2016.shp") #loading data

# Plotting PCH core range using ggplot
(PCH_range_map <- ggplot() + 
  geom_sf(data = PCH_core_range, size = 0.5, color = "black", fill = "yellow") + 
  theme_bw()+
  ggtitle("PCH core range 2016")) 

# Checking PCH range and shrub map have same projection
projection(PCH_core_range)
projection(shrub_agb_p50)
# same projection


### BASE MAP of North America ----
world <- getMap(resolution = "low")
(Alaska_Yukon_base <- ggplot() +
    borders("world", colour = "black", fill = "white", size = 0.3) + 
    coord_cartesian(xlim = c(-180, -80), ylim = c(60, 75)) +
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") ) 

(Alaska_Yukon <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "grey", colour = "black") + 
    coord_cartesian(xlim = c(-180, -80), ylim = c(60, 75)) +
    geom_sf(data = PCH_core_range,
            fill = "yellow", colour = "yellow") + 
    theme_classic() +  
    xlab("Longitude") +
    ylab("Latitude") ) ## DOESNT WORK

## CROPPED SHRUB MAP ----
# Cropping shrub map to PCH range
plot(shrub_agb_p50) # plots raster 
plot(PCH_core_range, add = TRUE) # adds range polygon onto shrub map
dev.off()

cropped <- crop(shrub_agb_p50, PCH_core_range)
cropped_new <- projectRaster(cropped, crs="+init=EPSG:4326")

# Cropped map with viridis palette
(cropped_viridis <- gplot(cropped_new) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<1000, 
             scales::rescale(x,
                             to = to,
                             from = c(min(x, na.rm = TRUE), 1000)),
             1)}) +
    coord_quickmap()+
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Shrub biomass cover (g/m2) of the PCH alaskan range") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 45, hjust = 1)))  # rotates x axis text

# Cropped map with personalised colour palette (low-mid)
(cropped_my_palette <- gplot(cropped_new) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_gradient(low = "tan", high = "green4", 
                        rescaler = function(x, to = c(0, 1), from = NULL) {
                          ifelse(x<1000, scales::rescale(x,
                             to = to,
                             from = c(min(x, na.rm = TRUE), 1000)),
             1)}) +
    coord_quickmap()+
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Shrub biomass cover (g/m2) of the PCH alaskan range") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 45, hjust = 1)))  # rotates x axis text


# Cropped map with personalised colour palette (low-mid-high)
(cropped_new_mid <- gplot(cropped) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_gradient2(low = "green", mid = "green4", high = "brown", midpoint = 50) +
    coord_quickmap()+
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Shrub biomass cover (g/m2) of the PCH alaskan range") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 45, hjust = 1)))  # rotates x axis text

dev.off()

## EXTRACTING RASTER DATA ----
projection(cropped)

cropped_data <- as.data.frame(cropped, xy=TRUE)
glimpse(cropped_data)

# Histogram of shrub agb (g/m2) 
hist(cropped_data$shrub_agb_p50)

# (cropped_histogram <- ggplot(cropped_data, aes(x = shrub_agb_p50, fill = y)) +  
    #geom_histogram(stat = "count") +
    #geom_vline(aes(xintercept = mean(shrub_agb_p50)),            
              # colour = "red", linetype = "dashed", size = 1))

dev.off()

### CATEGORISE into HIGH/MEDIUM/LOW biomass and NORTH VS SOUTH range
cropped_data <- cropped_data %>%
  mutate(biomass_level = case_when
                (shrub_agb_p50 <= 100 ~ 'Low',
                  shrub_agb_p50 > 100  & shrub_agb_p50 < 500 ~ 'Medium', 
                  shrub_agb_p50 >= 500 ~ 'High')) %>%
  mutate(area = case_when )



glimpse(cropped_data)
str(cropped_data)

### OTHER (Random) ----

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


## Specify the required projection using a proj4 string
newProj <- CRS("+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

#e <- extract(shrub_agb_p2_5, PCH_core_range)
zoom()