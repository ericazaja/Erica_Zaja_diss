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
library(rnaturalearth)
library(rnaturalearthdata)


# LOADING DATA ----
shrub_agb_p50 <- raster("datasets/berner_data/shrub_agb_p50.tif") # shrub data
PCH_core_range <- st_read("datasets/PCH_Core_Range_2016/PCH_Core_Range_2016.shp") # PCH range data

res(shrub_agb_p50)
# resolution of map: [1] 30 30

# visualising shrub map
ggplot() +
  geom_raster(shrub_agb_p50) + 
  scale_fill_gradientn(name = "Bathymetry", colors = terrain.colors(10)) +
  geom_sf(data = PCH_core_range, color = "blue", fill = NA) +
  coord_sf()


ggplot() +
  geom_sf(data = st_as_sfc(st_bbox(shrub_agb_p50)), fill = "green",
          color = "green", alpha = .2) +  
  geom_raster(data = erie_bathy_Cropped_df,
              aes(x = x, y = y, fill =shrub_agb_p50)) + 
  scale_fill_gradientn(name = "Bathymetry", colors = terrain.colors(10)) + 
  coord_sf()


### DATA VISUALISATION -----

# setting a theme 
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

# Plotting shrub raster (entire) with ggplot
(gplot_shrub_agb_p50 <- gplot(shrub_agb_p50) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<1000, 
             scales::rescale(x,
                             to = to,
                             from = c(min(x, na.rm = TRUE), 1000)),1)}) +
    coord_quickmap()+
    theme_shrub() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Shrub biomass cover (g/m2) of Alaskan north slope") +
    theme(plot.title = element_text(hjust = 0.5),     # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 0, hjust = 1)))  # rotates x axis text


# Plotting PCH core range (entire) using ggplot
(PCH_range_map <- ggplot() + 
    geom_sf(data = PCH_core_range, size = 0.5, color = "black", fill = "grey") + 
    theme_shrub()+
    ggtitle("PCH core range 2016")) 


# Checking PCH range and shrub map have same projection
projection(PCH_core_range)
projection(shrub_agb_p50)
# same projection

# converting to latitude-longitude from aes projection
# shrub_latlong <- projectRaster(shrub_agb_p50, crs = "+proj=longlat +lat_0=50 
# +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") # takes too long


### BASE MAP of North America ----
world <- ne_countries(scale = "medium", returnclass = "sf") 
class(world)

Alaska_coords <- data.frame(longitude = c(-180, -80), latitude = c(60, 75)) # making a datafrme of coordinates of Alaska/Yukon

# plotting map 
(Alaska_Yukon <- ggplot(data = world) +
    geom_point(data = Alaska_coords, aes(x = longitude, y = latitude)) +
  geom_sf(data = PCH_core_range, size = 0.5, color = "black", fill = "grey") +
  coord_sf(xlim = c(-180, -80), ylim = c(60, 75), expand = FALSE))

class(PCH_core_range)

### OVERLAYING MAPS 
# trying to overlay polygon of PCH range onto the basemap 

Alaska_polygon <- as(Alaska_coords, 'SpatialPolygonsDataFrame') # making extent into polygon

ggplot(data = Alaska_coords) +
  geom_polygon(PCH_core_range, aes(x = long, y = lat, group = group), fill = "red") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

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

### measuring area of focal area 
# shrub_latlong <- projectRaster(shrub_crop, crs = "+init=epsg:4326") # doesnt work 
cell_size <- area(shrub_latlong, na.rm=TRUE, weights=FALSE)
cell_size  <-cell_size[!is.na(cell_size)]
raster_area<-length(cell_size)*median(cell_size)
#print(paste("Area of focal area (raster):“,round(raster_area, digits=1),“km2″))

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

# world <- getMap(resolution = "low")

(NA_base <- ggplot() +
    borders("world", colour = "black", fill = "white", size = 0.3) + 
    coord_cartesian(xlim = c(-180, -80), ylim = c(60, 75)) +
    theme_shrub() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") ) 


### Questions: 


