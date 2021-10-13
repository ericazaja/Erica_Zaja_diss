##%######################################################%##
#                                                          #
####         SPATIAL ANALYSIS EXPERIMENTS -----         ####
#               Erica Zaja - 09/10/2021                   #
#                                                         #
##%######################################################%##

# Loading libraries -----
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)

# Loading data -----
berner_dataset <- read_csv("datasets/berner_data/data/berner_dataset.csv")
plant_agb_p2_5 <- raster("datasets/berner_data/data/plant_agb_p2_5.tif")
# class      : RasterLayer 
# dimensions : 14890, 35405, 527180450  (nrow, ncol, ncell)
# resolution : 30, 30  (x, y)
# extent     : -540475.3, 521674.7, 1933928, 2380628  (xmin, xmax, ymin, ymax)
# crs        : +proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 
# source     : /Users/ericazaja/Desktop/dissertation/R_dissertation/datasets/berner_data/data/plant_agb_p2_5.tif 
# names      : plant_agb_p2_5 
# values     : 0, 65535  (min, max)

plant_agb_p50 <- raster("datasets/berner_data/data/plant_agb_p50.tif")

# coordinate reference system (CRS) 

shrub_agb_p2_5 <- raster("datasets/berner_data/data/shrub_agb_p2_5.tif")
# class      : RasterLayer 
# dimensions : 14890, 35405, 527180450  (nrow, ncol, ncell)
# resolution : 30, 30  (x, y)
# extent     : -540475.3, 521674.7, 1933928, 2380628  (xmin, xmax, ymin, ymax)
# crs        : +proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 
# source     : /Users/ericazaja/Desktop/dissertation/R_dissertation/datasets/berner_data/data/shrub_agb_p2_5.tif 
# names      : shrub_agb_p2_5 
# values     : 0, 65535  (min, max)

shrub_dominance_of_agb_p50 <- raster("datasets/berner_data/data/shrub_dominance_of_agb_p50.tif")


### N.B.same plot=same values in raster info

# Comparing rasters to see if they have the same extent, number of rows and column, projection, resolution and origin
compareRaster(shrub_agb_p2_5, plant_agb_p2_5)
# TRUE

# Plotting rasters -----
plot(shrub_agb_p2_5)
plot(plant_agb_p2_5)
plot(plant_agb_p50)
plot(shrub_dominance_of_agb_p50)
dev.off()

# Cropping rasters ----- 

plot(shrub_agb_p2_5)
zoom(shrub_agb_p2_5) ## define square 

# Using ggplot ----
# Shrub cover p2_5
gplot_shrub_agb_p2_5 <- gplot(shrub_agb_p2_5) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  # value is the specific value (of reflectance) each pixel is associated with
  scale_fill_viridis_c() +
  coord_quickmap() +
  ggtitle("Shrub cover of the north slope of Alaska, raster plot") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +   					    # removes defalut grey background
  theme(plot.title = element_text(hjust = 0.5),             # centres plot title
        text = element_text(size=20),		       	    # font size
        axis.text.x = element_text(angle = 90, hjust = 1))  # rotates x axis text

# ggsave("gplot_shrub_agb_p2_5", scale = 1.5, dpi = 300) 		# to save plot

# Plant cover p2_5
gplot_plant_agb_p2_5 <- gplot(plant_agb_p2_5) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  # value is the specific value (of reflectance) each pixel is associated with
  scale_fill_viridis_c() +
  coord_quickmap() +
  ggtitle("Plant cover of the north slope of Alaska, raster plot") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +   					    # removes defalut grey background
  theme(plot.title = element_text(hjust = 0.5),             # centres plot title
        text = element_text(size=20),		       	    # font size
        axis.text.x = element_text(angle = 90, hjust = 1))  # rotates x axis text

# ggsave("gplot_plant_agb_p2_5", scale = 1.5, dpi = 300) 		# to save plot

# Stacking rasters -----
# Create a stack of all the rasters, so just putting them all on top of each other.
t <- stack(plant_agb_p2_5, shrub_agb_p2_5)

# To visualise all the bands together, we can use facet_wrap in gplot. 
gplot(t) +
  geom_raster(aes(x = x, y = y, fill = value))+
  scale_fill_viridis_c() +
  facet_wrap(~variable) +
  coord_quickmap()+
  ggtitle("Vegetation cover p2, raster plots") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

### PCH summer range -----
# https://gis.stackexchange.com/questions/34310/opening-lyr-file-via-rgdal-ogr
range <- readOGR("datasets/PCH_data/Summer_range/v10/cyr_2013.gdb")
plot(range)
# class       : SpatialPolygonsDataFrame 
# features    : 1 
# extent      : 295021.4, 722245.6, 2015442, 2271115  (xmin, xmax, ymin, ymax)
# crs         : +proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 
# variables   : 7
# names       : SEASONAL_, SEASONAL_I,      HERD,        RANGE,    Shape_Leng,     Shape_Length,       Shape_Area 
# value       :        29,        122, Porcupine, Summer Range, 1511342.57305, 1568629.41263943, 73550115378.1924 

### HOW DO I OVERLAY A RASTER layer WITH A spatial POLYGON? -----
# https://rspatial.org/raster/rosu/Chapter11.html 
library(rspatial)
library(maptools)
library(rgeos)

projection(plant_agb_p2_5)
projection(range)
# same projection

range <- spTransform(range, crs(plant_agb_p2_5))

plot(plant_agb_p2_5)
plot(range, add = TRUE)
e <- extract(plant_agb_p2_5, range)

# Cropping plant map to range -----
cropped <- crop(plant_agb_p2_5, range)
masked <- mask(cropped, range)
plot(masked)
plot(range, add=T)
