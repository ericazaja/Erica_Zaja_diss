##%######################################################%##
#                                                          #
####         RQ2: SHRUB BIOMASS VS CLIMATE              #### 
#               Erica Zaja - 22/10/2021                   ##
#                                                         #
##%######################################################%##

# RQ2: how does biomass vary with temperature and precipitation?
# PART 1: CLIMATE DATA EXTRACTION ----
# Script credit: adapted from Joseph Everest and Mariana Garcia Criado

# LOADING LIBRARIES -----
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)
library(lme4)
library(sjPlot)
library(gridExtra)
library(ggpubr)
library(corrplot)
library(Hmisc)


# LOADING DATA ------

# Climate data from CHELSA 2022
temp <- raster("datasets/climate_data/CHELSA_bio10_10.tif") 
# Temperature climatologies: mean daily mean air temperatures of the warmest quarter (bio10) (°C). Offset -273.15

precip <- raster("datasets/climate_data/CHELSA_bio10_18.tif")
# Precipitation climatologies: mean monthly precipitation amount of the warmest quarter (bio18) (kg m-2)

# DATA EXPLORATION -----
# checking resolution of rasters
res(temp)
res(precip)
# [1] 0.008333333 0.008333333
# The spatial resolution of a raster refers the size of each cell in meters. 
# This size in turn relates to the area on the ground that the pixel represents.
# The higher the resolution for the same extent the crisper the image (and the larger the file size) 

# Visualising climate rasters
plot(temp, main = "Mean daily mean air temperatures of the warmest quarter (°C)")
plot(precip, main = "Mean monthly precipitation of the warmest quarter (kg m-2)")
precip_raster <- levelplot(precip)
temp_raster <- levelplot(temp)

# EXTRACTION ------
# Loading the coordinates of the cropped shrub map
coords <- read.csv("datasets/berner_data/r3_rsample_002.csv") %>% 
  dplyr::select(longitude, latitude) # keeping lat and long

# Creating SpatialPoints (sp) object of unique coordinates
coords_sp <- SpatialPoints(coords)

# creating raster stack
chelsa.stack <- stack(precip, temp)

# Extracting variables values for each pair of coordinates
chelsa.extract <- raster::extract(chelsa.stack, coords_sp, df = TRUE) # extract coords 

# Combining dataframes:
# Converting the SpatialPoints (sp) object into a dataframe 
coord.df <- as.data.frame(coords_sp)

# Reassigning the 'ID' to the coordinates dataframe
coord.df$ID <- row.names(coord.df)
coord.df$ID <- as.numeric(coord.df$ID) # Make numeric

# Merging the two dataframes: extracted CHELSA variables and the coordinates
coord.chelsa.combo <- left_join(chelsa.extract, coord.df, by = c("ID" = "ID"))

# Loading the shrub biomass df
biomass.df <- read.csv("datasets/berner_data/r3_rsample_002.csv") %>%
  rename(ID = X) %>%
  dplyr::select(ID, biomass, gridcell)

hist(biomass.df$biomass)

# Merging biomass df with climate df
coord.chelsa.combo.a <- left_join(coord.chelsa.combo, biomass.df, by = c("ID" = "ID"))

# Modifying some of the variables to more useful values
coord.chelsa.combo.b <- coord.chelsa.combo.a %>% 
  mutate(CHELSA_bio10_10 = CHELSA_bio10_10/10) # Divide by 10 to get to degC

# Renaming the variables to shorter column headings
coord.chelsa.combo.c <- coord.chelsa.combo.b %>% 
  rename(CH_TempMeanSummer = CHELSA_bio10_10,
         CH_PrecipMeanSummer = CHELSA_bio10_18) %>% na.omit()

unique(coord.chelsa.combo.c$CH_TempMeanSummer)

# Exporting the dataframe to csv
write.csv(coord.chelsa.combo.c, "datasets/climate_data/coord_chelsa_combo_new.csv")

################################################################## ÉND -----