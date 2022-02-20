##%######################################################%##
#                                                          #
###                   MAPPING SCRIPT                    ####
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

## LOADING LIBRARIES -----
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)
library(sf)
library(tidyverse)
library(maptools)
library(rgeos)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggmap)
library(tmap)
library(tmaptools)

## LOADING DATA ----
shrub_agb_p50 <- raster("datasets/berner_data/shrub_agb_p50.tif") # shrub data (from Berner et al 2018)
PCH_core_range <- st_read("datasets/PCH_Core_Range_2016/PCH_Core_Range_2016.shp") # PCH range data 

## EXPLORING DATA ------
# class: what type of data 
class(shrub_agb_p50) # RasterLayer
class(PCH_core_range) # sf dataframe

# resolution of shrub map 
res(shrub_agb_p50) # resolution of map: [1] 30m x 30m

# extent of range polygon
st_bbox(PCH_core_range) 
# xmin      ymin      xmax      ymax 
# 165444.3 1697872.7  849222.0 2270606.5 

# extent of shrub map
st_bbox(shrub_agb_p50) 
#  xmin      ymin      xmax      ymax 
# -540475.3 1933928.1  521674.7 2380628.1 

# projection
projection(PCH_core_range)
projection(shrub_agb_p50)
# same projection

# coordinate ref system (CRS)
crs(PCH_core_range) 
crs(shrub_agb_p50) 
# same CRS: +proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 

## DATA VISUALISATION -----
# setting a personalised theme 
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

# 1. Shrub map ----
# Plotting shrub raster (entire) with ggplot
(gplot_shrub_agb_p50 <- gplot(shrub_agb_p50) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<1000, scales::rescale(x, to = to, from = c(min(x, na.rm = TRUE), 1000)),1)}, na.value="white") +
    coord_quickmap()+
    theme_shrub() +  # Remove ugly grey background
    xlab("\nLongitude") +
    ylab("Latitude\n") +
    ggtitle("Shrub biomass cover (g/m2) of Alaskan north slope\n") +
    theme(plot.title = element_text(hjust = 0.5),     # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 0, hjust = 1)))  # rotates x axis text

# ggsave("output/figures/gplot_shrub_agb_p50.png")

# 2. PCH range ----
# Plotting PCH core range (entire) using ggplot
(PCH_range_map <- ggplot() + 
    geom_sf(data = PCH_core_range, size = 0.5, color = "black", fill = "white") + 
    theme_shrub()+
    ggtitle("PCH core range 2016")) 

# ggsave("output/figures/PCH_range_map.png")

# plotting PCH geometry only 
plot(PCH_core_range)
plot(PCH_core_range[, "Id"], key.width = lcm(5), key.pos = 4)
plot(st_geometry(PCH_core_range))

# 3. Map overlay ----
plot(shrub_agb_p50, xlim = c(-540475.3,  921674.7), ylim = c(1933928.1,2380628.1))
plot(st_geometry(PCH_core_range), xlim = c(165444.3, 1049222.0), add = TRUE)

# converting to latitude-longitude from aes projection
# shrub_latlong <- projectRaster(shrub_agb_p50, crs = "+proj=longlat +lat_0=50 
# +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") # takes too long
range(shrub_rsample_00$lat)

### BASE MAP of North America ----
# Full map of canada
canada <- raster::getData("GADM", country = "CAN", level = 1)
plot(canada)
yukon <- canada[canada$NAME_1 != "Yukon"]


world <- ne_countries(scale = "medium", returnclass = "sf") 
class(world)

Alaska_coords <- data.frame(longitude = c(-180, -80), latitude = c(60, 75)) # making a datafrme of coordinates of Alaska/Yukon

# plotting map 
(Alaska_Yukon <- ggplot(data = Alaska_coords) +
    geom_point(aes(x = longitude, y = latitude)) +
  coord_sf())


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


# Loading cropped map
shrub_crop_latlong_agg <- raster("datasets/berner_data/shrub_crop_latlong_agg.tif")

# Cropped map with viridis palette
(cropped_viridis <- gplot(shrub_crop_latlong_agg) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<500, 
             scales::rescale(x,
                             to = to,
                             from = c(min(x, na.rm = TRUE), 500)),
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
(cropped_my_palette <- gplot(shrub_crop_latlong_agg) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_gradient(low = "tan", high = "green4", 
                        rescaler = function(x, to = c(0, 1), from = NULL) {
                          ifelse(x<500, scales::rescale(x,
                                                         to = to,
                                                         from = c(min(x, na.rm = TRUE), 500)),
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
(cropped_new_mid <- gplot(shrub_crop_latlong_agg) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_gradient2(low = "yellow", mid = "green4", high = "brown", midpoint = 500) +
    coord_quickmap()+
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Shrub biomass cover (g/m2) of the PCH alaskan range") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 45, hjust = 1)))  # rotates x axis text

dev.off()



shrub_rsample_00 <- read.csv("datasets/berner_data/shrub_rsample_00.csv")
range(shrub_rsample_00$biomass)
# 2.000 1484.069



### CATEGORISE into HIGH/MEDIUM/LOW biomass and EAST VS WEST range area
mean(shrub_rsample_00$biomass)
# 255.8016 mean biomass

shrub_rsample_categ <- shrub_rsample_00 %>%
  mutate(biomass_level = case_when (biomass <= 200 ~ 'Low',
                                    biomass > 200  & biomass < 500 ~ 'Medium', 
                                    biomass >= 500 ~ 'High'))

# Hist 
(hist_random <-shrub_rsample_categ %>%
    ggplot(aes(x = biomass, fill = biomass_level )) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 60) +
    geom_vline(aes(xintercept = mean(biomass)),            
               colour = "red", linetype = "dashed", size = 1) +
    scale_fill_manual(values=c( "green4", "tan", "yellow")) +
    theme_shrub())



dev.off()


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


plot(shrub_agb_p50,
     main = "Shrub aboveground biomass in the PCH summer range",
     axes = FALSE,
     #ext = extent(PCH_core_range),
     box = FALSE)
plot(PCH_core_range,
     col = "white",
     add = TRUE)

mask <- mask(shrub_agb_p50, PCH_core_range)

(tm_shape(mask,  xlim = c(-540475.3,849222.0) )+
    tm_raster(col="shrub_agb_p50", style= "cont")+
    tm_shape(PCH_core_range, xlim = c(165444.3, 849222.0), ylim = c(1697872.7,2380628.1) )+
    tm_borders(col="black"))


