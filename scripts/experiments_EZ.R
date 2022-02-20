##%######################################################%##
#                                                          #
###              Random experiments script              ####
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

# DO NOT RUN

# cropping and extracting from rasters -----

plot(shrub_agb_p2_5)
zoom(shrub_agb_p2_5) ## define square 

# Specify the required projection using a proj4 string
newProj <- CRS("+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

e <- extract(shrub_agb_p2_5, PCH_core_range)
zoom()

# Base maps ----
world <- getMap(resolution = "low")

(NA_base <- ggplot() +
    borders("world", colour = "black", fill = "white", size = 0.3) + 
    coord_cartesian(xlim = c(-180, -80), ylim = c(60, 75)) +
    theme_shrub() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") ) 

# Overlaying ----
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


