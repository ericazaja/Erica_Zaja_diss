##%######################################################%##
#                                                          #
####         RQ2: SHRUB BIOMASS VS CLIMATE -----        #### 
#               Erica Zaja - 22/10/2021                   ##
#                                                         #
##%######################################################%##

# RQ2: how does biomass vary with temp and precip?
# (a) Is shrub biomass greater in areas with higher summer temperatures? 
# (b) Is shrub biomass greater in areas with higher summer precipitation? 
# (c) Is shrub biomass greater in areas with higher summer temperature and precipitation? 
  
# Shrub data from Berner et al 2018
# Climate data from CHELSA 2022
# Temperature climatologies: mean daily mean air temperatures of the warmest quarter (bio10) (°C). Offset -273.15
# Precipitation climatologies: mean monthly precipitation amount of the warmest quarter (bio18) (kg m-2)

# Loading libraries -----
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)
library(lme4)

# JOE: Loading CHELSA data ------
temp <- raster("datasets/climate_data/CHELSA_bio10_10.tif") 
precip <- raster("datasets/climate_data/CHELSA_bio10_18.tif")
# are these the right variables?

res(temp)

# The spatial resolution of a raster refers the size of each cell in meters. 
# This size in turn relates to the area on the ground that the pixel represents.
# The higher the resolution for the same extent the crisper the image (and the larger the file size) 

# resolution = 0.008333333 0.008333333
# resolution of map: [1] 30 30
# > 30/0.008333333 =  3600

plot(temp, main = "Mean daily mean air temperatures of the warmest quarter (°C)")
plot(precip, main = "Mean monthly precipitation of the warmest quarter ((kg m-2)")
levelplot(precip)

# Load the coordinates of the cropped shrub map
coords <- read.csv("datasets/berner_data/shrub_rsample_00.csv") %>% 
  dplyr::select(long, lat)

# Climatologies:
# Create SpatialPoints (sp) object of unique coordinates
coords_sp <- SpatialPoints(coords)

# create raster stack
chelsa.stack <- stack(precip, temp)

# Extract variables values for each pair of coordinates
chelsa.extract <- raster::extract(chelsa.stack, coords_sp, df = TRUE) # extract coords 

# COMBINED DATAFRAMES 

# Convert the SpatialPoints (sp) object into a dataframe 
coord.df <- as.data.frame(coords_sp)

# Reassign the 'ID' to the coordinates dataframe
coord.df$ID <- row.names(coord.df)
coord.df$ID <- as.numeric(coord.df$ID) # Make numeric

# Merge the two dataframes: extracted CHELSA variables and the coordinates
coord.chelsa.combo <- left_join(chelsa.extract, coord.df, by = c("ID" = "ID"))

# loading the shrub biomass df
biomass.df <- read.csv("datasets/berner_data/shrub_rsample_00.csv") %>%
  rename(ID = X) %>%
  dplyr::select(ID, biomass)

# merging biomass df with climate df
coord.chelsa.combo.1 <- left_join(coord.chelsa.combo, biomass.df, by = c("ID" = "ID"))

# Modify some of the variables to more useful values
coord.chelsa.combo.2 <- coord.chelsa.combo.1 %>% 
  mutate(CHELSA_bio10_10 = CHELSA_bio10_10/10) # Divide by 10 to get to degC

# Rename the variables to shorter column headings
coord.chelsa.combo.3 <- coord.chelsa.combo.2 %>% 
  rename(CH_TempMeanSummer = CHELSA_bio10_10,
         CH_PrecipMeanSummer = CHELSA_bio10_18)


# EXPORT TO CSV

# Export the dataframe to combine with ITEX data
write.csv(coord.chelsa.combo.3, "datasets/climate_data/coord_chelsa_combo.csv")

# MODELLING -----
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

# model: biomass ~ temp
model_3 <- lmer(biomass ~ CH_TempMeanSummer + (1|strip), data = coord.chelsa.combo.3)
summary(model_3)

# scatter: biomass ~ temp
(scatter_temp <- ggplot(coord.chelsa.combo.3, aes(x = CH_TempMeanSummer, y = biomass)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm") +
    theme_shrub())


# model: biomass ~ precip
model_4 <- lmer(biomass ~ CH_PrecipMeanSummer + (1|strip), data = coord.chelsa.combo.3)
summary(model_4)

# scatter: biomass ~precip
(scatter_precip <- ggplot(coord.chelsa.combo.3, aes(x = CH_PrecipMeanSummer, y = biomass)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm") +
    theme_classic())


# model: biomass ~ temp*precip
model_5 <- lmer(biomass ~ CH_TempMeanSummer*CH_PrecipMeanSummer + (1|strip), data = coord.chelsa.combo.3)
summary(model_5)

## To display: Categorise precipitation dry moist wet: 3 lines in plot with temp on the x and biomass on y and points coloured by moist level



##############################################################################

# COPY OF CHELSA raster data extraction ------
# Joseph Everest (with help from M. Garcia Criado and J. Assmann)
# February 2021, adapted October 2021
# Modified by Erica Zaja - 01/02/2022

# PACKAGES 

# Load required packages
library(tidyverse)
library(raster)
library(rgdal)
library(rasterVis)
library(sp)
library(broom)


# DATA IMPORT 

# Load the ITEX coordinates
itex <- read.csv("scripts/josephjeverest/FuncDiv_v2/data/output_01_itex_coords.csv") %>% 
  dplyr::select(-X)

# Climatologies:
# CHELSA climatologies (.tif) are stored locally on a hard drive and on the Team Shrub Data Store due to their size
# Contact Joe Everest (joseph.everest@ed.ac.uk) for more information on access or visit https://chelsa-climate.org/downloads/

# Defining the filepath to the climatology files
folderpath.chelsa <- ("E:/CHELSA/") # Hard drive
# folderpath.chelsa <- ("T:/Climate_Data/Chelsa/CHELSA_updated_2021/") # Team Shrub Data Store
filenames.chelsa <- list.files(folderpath.chelsa, pattern = "*.tif")
filepath.chelsa = paste0(folderpath.chelsa, filenames.chelsa)


# EXTRACTION 

# Create SpatialPoints (sp) object of unique coordinates
itex.coord <- SpatialPoints(itex)

# create raster stack
chelsa.stack <- stack(filepath.chelsa)

# Extract variables values for each pair of coordinates
chelsa.extract <- raster::extract(chelsa.stack, itex.coord, df = TRUE) # extract coords from the itex


# COMBINED DATAFRAMES

# Convert the SpatialPoints (sp) object into a dataframe 
itex.coord.df <- as.data.frame(itex.coord)

# Reassign the 'ID' to the ITEX coordinates dataframe
itex.coord.df$ID <- row.names(itex.coord.df)
itex.coord.df$ID <- as.numeric(itex.coord.df$ID) # Make numeric

# Merge the two dataframes: extracted CHELSA variables and the ITEX coordinates
coord.chelsa.combo.1 <- left_join(chelsa.extract, itex.coord.df, by = c("ID" = "ID"))


# FORMAT THE DATAFRAME 

# Modify some of the variables to more useful values
coord.chelsa.combo.2 <- coord.chelsa.combo.1 %>% 
  mutate(CHELSA_bio10_05 = CHELSA_bio10_05/10, # Divide by 10 to get to degC
         CHELSA_bio10_06 = CHELSA_bio10_06/10, # Divide by 10 to get to degC
         CHELSA_bio10_10 = CHELSA_bio10_10/10, # Divide by 10 to get to degC
         CHELSA_bio10_11 = CHELSA_bio10_11/10, # Divide by 10 to get to degC
         CHELSA_bio10_12_minus_bio10_18 = CHELSA_bio10_12 - CHELSA_bio10_18) %>% # Snow = annual precip. - summer precip.
  dplyr::select(-ID, -CHELSA_bio10_18) %>% # No longer need summer precip. values as calculated snow already
  relocate(LONG, LAT, before = CHELSA_bio10_04)

# Rename the variables to shorter column headings
coord.chelsa.combo.3 <- coord.chelsa.combo.2 %>% 
  rename(CH_TempMaxSummer = CHELSA_bio10_04,
         CH_TempMinWinter = CHELSA_bio10_06,
         CH_TempMeanSummer = CHELSA_bio10_10,
         CH_TempMeanWinter = CHELSA_bio10_11,
         CH_TempSeason = CHELSA_bio10_04,
         CH_PrecipAnnual = CHELSA_bio10_12,
         CH_PrecipSeason = CHELSA_bio10_15,
         CH_SnowAnnual = CHELSA_bio10_12_minus_bio10_18)


# EXPORT TO CSV

# Export the dataframe to combine with ITEX data
write.csv(coord.chelsa.combo.3, "scripts/josephjeverest/FuncDiv_v2/data/output_03_chelsa.csv")


#####################################################
# OTHER (Random) -----
## eg. code to make site/block/plot categorical from https://ourcodingclub.github.io/tutorials/model-design/ 

