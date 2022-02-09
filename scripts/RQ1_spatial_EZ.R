##%######################################################%##
#                                                          #
###              RQ1: SPATIAL ANALYSIS                  ####
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

## RQ1: What areas within the PCH Alaskan summer range have high-medium-low shrub biomass cover?

## ISLA START ----

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
st_bbox(PCH_core_range) # extent 

# Plotting shrub raster with base R
plot(shrub_agb_p50)

# defining extent of the PCH range polygon
range_extent <- extent(165444.3, 1697872.7,  849222.0, 2270606.5)

# cropping shrub map to extent of the PCH range
shrub_crop <- crop(x = shrub_agb_p50, y = range_extent)

# making cropped raster into a dataframe

shrub_crop_df <- as.data.frame(shrub_crop, xy = TRUE) 
shrub_crop_omit <- na.omit(shrub_crop_df)

# extracting shrub biomass data from pixels within the range 
extracted_shrub <- raster::extract(x = shrub_agb_p50,
                             y = as(PCH_core_range, "Spatial"),
                             df = TRUE)


### ISLA STOP ----



# shrub_latlong <- projectRaster(shrub_agb_p50, crs = "+proj=longlat +lat_0=50 
# +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") # takes too long

ggplot() +
  geom_raster(shrub_agb_p50) + 
  scale_fill_gradientn(name = "Bathymetry", colors = terrain.colors(10)) +
  geom_sf(data = PCH_core_range, color = "blue", fill = NA) +
  coord_sf()

bathy_Cropped <- crop(x = shrub_agb_p50, y = as_Spatial(PCH_core_range))
plot(bathy_Cropped) # right !
erie_bathy_Cropped_df <- as.data.frame(bathy_Cropped, xy = TRUE)

ggplot() +
  geom_sf(data = st_as_sfc(st_bbox(shrub_agb_p50)), fill = "green",
          color = "green", alpha = .2) +  
  geom_raster(data = erie_bathy_Cropped_df,
              aes(x = x, y = y, fill =shrub_agb_p50)) + 
  scale_fill_gradientn(name = "Bathymetry", colors = terrain.colors(10)) + 
  coord_sf()

str(erie_bathy_Cropped_df)

fish_tracks_bathy <- raster::extract(x = bathy_Cropped,
                             y = as(PCH_core_range, "Spatial"),
                             df = TRUE)
str(PCH_core_range)

str(fish_tracks_bathy)

# Plotting shrub raster with ggplot
(gplot_shrub_agb_p50 <- gplot(shrub_agb_p50) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  # value is the specific value (of reflectance) each pixel is associated with
  scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
  ifelse(x<1000, 
           scales::rescale(x,
                           to = to,
                           from = c(min(x, na.rm = TRUE), 1000)),
           1)}) +
  coord_quickmap()+
  theme_shrub() +  # Remove ugly grey background
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Shrub biomass cover (g/m2) of Alaskan north slope") +
  theme(plot.title = element_text(hjust = 0.5),     # centres plot title
        text = element_text(size=15),		       	    # font size
        axis.text.x = element_text(angle = 0, hjust = 1)))  # rotates x axis text

dev.off()


## Setting a theme ----
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

# Plotting PCH core range using ggplot
(PCH_range_map <- ggplot() + 
  geom_sf(data = PCH_core_range, size = 0.5, color = "black", fill = "grey") + 
  theme_shrub()+
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
    theme_shrub() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") ) 

### PROBLEM 1 -----
# trying to overlay polygon of PCH range onto the basemap 
(Alaska_Yukon <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "grey", colour = "black") + 
    coord_cartesian(xlim = c(-180, -80), ylim = c(60, 75)) +
    geom_sf(data = PCH_core_range,
            fill = "yellow", colour = "yellow") + 
    theme_shrub() +  
    xlab("Longitude") +
    ylab("Latitude") ) ## DOESNT WORK


## CROPPED SHRUB MAP ----
### PROBLEM 2 ----- 
# Cropping shrub map to PCH range
plot(shrub_agb_p50) # plots raster 
plot(PCH_core_range, add = TRUE) # adds range polygon onto shrub map but UGLY ! 
dev.off()

cropped <- crop(shrub_agb_p50, PCH_core_range)

cropped_latlong <- projectRaster(cropped, crs="+init=EPSG:4326") # works but then removes AGB at the bottom
(cell_size <-area(cropped_latlong, na.rm=TRUE, weights=FALSE))

cropped_latlong <- projectRaster(cropped, crs = "+proj=longlat +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") 
projection(cropped_latlong)

# Cropped map with viridis palette
(cropped_viridis <- gplot(cropped_latlong) +
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
(cropped_my_palette <- gplot(cropped) +
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


# Cropped map with personalised colour palette (low-mid-high) and lat long
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

cropped_shrub <- as.data.frame(cropped, xy=TRUE)
glimpse(cropped_shrub)

cropped_shrub_2 <- cropped_shrub %>% 
  dplyr::select(shrub_agb_p50)

glimpse(cropped_shrub_2)
head(cropped_shrub_2)
tail(cropped_shrub_2)
# write.csv(cropped_shrub_2, "datasets/berner_data/cropped_shrub_2.csv")

# PROBLEM 3 ----
cropped_coords <- as.data.frame(cropped_latlong, xy = TRUE)
glimpse(cropped_coords) # cancels (makes into NAs) all my shrub biomass data ! 
cropped_coords <- cropped_coords %>% 
  dplyr::select(- shrub_agb_p50)
# cropped_coords now is only the x and y lat and long! 
str(cropped_coords)
# write.csv(cropped_coords, "datasets/berner_data/cropped_coords.csv")

# I could try joining the two dataframes: so i have latlong AND shrub biomass
# shrub_PCH_range <- bind_rows(cropped_coords, cropped_shrub_2)

# Histogram of shrub agb (g/m2) 
hist(cropped_shrub$shrub_agb_p50)

# (cropped_histogram <- ggplot(cropped_data, aes(x = shrub_agb_p50, fill = y)) +  
    #geom_histogram(stat = "count") +
    #geom_vline(aes(xintercept = mean(shrub_agb_p50)),            
              # colour = "red", linetype = "dashed", size = 1))

dev.off()

### CATEGORISE into HIGH/MEDIUM/LOW biomass and EAST VS WEST range area
cropped_shrub_categ <- cropped_shrub %>%
  mutate(biomass_level = case_when (shrub_agb_p50 <= 100 ~ 'Low',
                  shrub_agb_p50 > 100  & shrub_agb_p50 < 500 ~ 'Medium', 
                  shrub_agb_p50 >= 500 ~ 'High'), 
        area = case_when (x >= 300000 ~ 'East' ,
                           x < 300000 ~ 'West'))

glimpse(cropped_shrub_categ)
str(cropped_shrub_categ)

## MODEL ----
# making area a factor with two levels (west and east)
cropped_shrub_categ$area <- as.factor(as.character(cropped_shrub_categ$area))   

model_1 <- lm(shrub_agb_p50 ~ area, data = cropped_shrub_categ) # doesnt run
save(model_1, file = "img/spatial_output/model_1_output.RData")

model_1 <- get(load("img/spatial_output/model_1_output.RData"))

summary(model_1) # doesnt run
# maybe something to do with my data being skewed 

### DATA VIS ----
# I want a boxplot: area on x axis (west VS east) and shrub biomass (g/m2) on y 
# This will show me:
# 1. if there is a difference in distribution of shrub biomass between east and west areas of the range.
# 2. where shrubs with higher/lower biomass are 



### OTHER (Random) ----

# Cropping rasters 
#plot(shrub_agb_p2_5)
#zoom(shrub_agb_p2_5) ## define square 

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
#newProj <- CRS("+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

#e <- extract(shrub_agb_p2_5, PCH_core_range)
#zoom()

# experimenting ----
e <- drawExtent() # draw on the shrub map
plot(e, asp = 1, xlab = "", ylab = "", axes = FALSE)
exp <- plot(shrub_agb_p50, add = TRUE)

exp <- plot(shrub_agb_p50, axes = FALSE, xlim = c(0,2e+05), ylim = c(2200000, 2300000))
exp_df <- as.data.frame(exp, xy=TRUE)

exp <- zoom(shrub_agb_p50, ext = drawExtent())
crop <- crop(shrub_agb_p50, exp)
exp_df <- as.data.frame(crop, xy=TRUE)