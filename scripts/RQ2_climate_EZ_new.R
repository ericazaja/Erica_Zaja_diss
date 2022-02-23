##%######################################################%##
#                                                          #
####         RQ2: SHRUB BIOMASS VS CLIMATE              #### 
#               Erica Zaja - 22/10/2021                   ##
#                                                         #
##%######################################################%##

# RQ2: how does biomass vary with temp and precip?
# Does shrub biomass increase with temperature/precipitation/the interaction between them?

# Data information
# Shrub data from Berner et al 2018
# Climate data from CHELSA 2022
# Temperature climatologies: mean daily mean air temperatures of the warmest quarter (bio10) (°C). Offset -273.15
# Precipitation climatologies: mean monthly precipitation amount of the warmest quarter (bio18) (kg m-2)

# LOADING LIBRARIES -----
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)
library(lme4)
library(sjPlot)

# LOADING DATA ------
temp <- raster("datasets/climate_data/CHELSA_bio10_10.tif") 
precip <- raster("datasets/climate_data/CHELSA_bio10_18.tif")

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
plot(precip, main = "Mean monthly precipitation of the warmest quarter ((kg m-2)")
levelplot(precip)

# EXTRACTION ------
# Loading the coordinates of the cropped shrub map
coords <- read.csv("datasets/berner_data/r3_rsample_00.csv") %>% 
  dplyr::select(long, lat) # keeping lat and long

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
biomass.df <- read.csv("datasets/berner_data/r3_rsample_00.csv") %>%
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
# write.csv(coord.chelsa.combo.c, "datasets/climate_data/coord_chelsa_combo_new.csv")

# THEME ----
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

# DATA MANIPULATION ----
str(coord.chelsa.combo.c)
unique(coord.chelsa.combo.c$gridcell)
# making grid cell into a factor
coord.chelsa.combo.c$gridcell <- as.factor(as.character(coord.chelsa.combo.c$gridcell))

# MODELLING ----

# Model 3 ----
# biomass ~ temp + random effect gridcell
model_3 <- lmer(biomass ~ CH_TempMeanSummer + (1|gridcell), data = coord.chelsa.combo.c)
summary(model_3)
# total variance: 
# variance for gridcell =  
# amount of variance explained by random effect: 
# I.e. differences between grid cells explain of the variance 
# that’s “left over” after the variance explained by our fixed effect (mean summer temperature).
# estimate for temperature (exp variable =   ) i.e. temperature negatively impacts biomass
# significant effect of temp on biomass 

# Checking model 3 assumptions
plot(model_3)
qqnorm(resid(model_3))
qqline(resid(model_3))  # points fall nicely onto the line - good!

# Output table model 3 
stargazer(model_3, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") 
# temperature significant
# shrub biomass decreases with mean summer temp


# Extracting model predictions 
pred_model_3 <- ggpredict(model_3, terms = c("CH_TempMeanSummer"))  # this gives overall predictions for the model
# write.csv(pred_model_3, file = "datasets/pred_model_3.csv")

# Plot the predictions 
(biomass_vs_temp <- (ggplot(pred_model_3) + 
                   geom_line(aes(x = x, y = predicted)) +          # slope
                   geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                               fill = "lightgrey", alpha = 0.5) +  # error band
                   geom_point(data = coord.chelsa.combo.c,                      # adding the raw data 
                              aes(x = CH_TempMeanSummer, y = biomass), size = 0.5) + 
                   labs(x = "\nMean summer temperature (°C)", y = "Shrub biomass (kg/m2)\n", 
                        title = "Shrub biomass decreases with temperature\n") + 
                   theme_shrub()))

# ggsave(file = "output/figures/biomass_vs_temp.png")

# Quick scatter: biomass ~ temp
(scatter_temp <- ggplot(coord.chelsa.combo.3, aes(x = CH_TempMeanSummer, y = biomass)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm") +
    theme_shrub())


# Model 4  -----
# biomass ~ precip +  random effect gridcell
model_4 <- lmer(biomass ~ CH_PrecipMeanSummer + (1|gridcell), data = coord.chelsa.combo.c)
summary(model_4)
# total variance: 4202 + 10485  =14687
# variance for gridcell =   4202 
# amount of variance explained by random effect:  4202 /14687 = 0.2861034= ~29%
# I.e. differences between grid cells explain ~29% of the variance 
# that’s “left over” after the variance explained by our fixed effect (mean precip).
# estimate for precip (exp variable =  2.860*** ) i.e. precip positively impacts biomass
# significant effect of precip on biomass  = 2.860*** 

# Checking model 4 assumptions 
plot(model_4)
qqnorm(resid(model_4))
qqline(resid(model_4))  # points fall nicely onto the line - good!

# Output table model 4 
stargazer(model_4, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
# precipitation significant 
# shrub biomass increases with mean summer precip

# Extracting model predictions 
pred_model_4 <- ggpredict(model_4, terms = c("CH_PrecipMeanSummer"))  # this gives overall predictions for the model
# write.csv(pred_model_4, file = "datasets/pred_model_4.csv")

# Plot the predictions 
(biomass_vs_precip <- (ggplot(pred_model_4) + 
                   geom_line(aes(x = x, y = predicted)) +          # slope
                   geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                               fill = "lightgrey", alpha = 0.5) +  # error band
                   geom_point(data = coord.chelsa.combo.c,                      # adding the raw data 
                              aes(x = CH_PrecipMeanSummer, y = biomass), size = 0.5) + 
                   labs(x = "\nMean summer precipitation (kg/m2)", y = "Shrub biomass (kg/m2)\n", 
                        title = "Shrub biomass increases with precipiation\n") + 
                   theme_shrub()))

# ggsave(file = "output/figures/biomass_vs_precip.png")

# Quick scatter: biomass ~precip
(scatter_precip <- ggplot(coord.chelsa.combo.c, aes(x = CH_PrecipMeanSummer, y = biomass)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm") +
    theme_classic())


# Model 5 ----
# biomass ~ temp*precip + random effect gridcell

# To display interaction: categorise precipitation 'dry', 'moist', 'wet': 
# Plot 3 lines in plot with temp on the x and biomass on y and points coloured by moisture level
range(coord.chelsa.combo.c$CH_PrecipMeanSummer)
# 55 (min) 174 (max precip (kg m-2))
# 174-55 = 119
# 119/3 = 39.66667
# 55 + 40 = 95 
# 95 + 40 = 135
# 135+40 = 175
# 55 (dry), 114.5 (moist), 174 (wet)
mean(coord.chelsa.combo.c$CH_PrecipMeanSummer)
# 101.8274

coord.chelsa.combo.d <- coord.chelsa.combo.c %>% 
  mutate(moisture = case_when(CH_PrecipMeanSummer >= 55 & CH_PrecipMeanSummer < 95 ~ "dry",
                              CH_PrecipMeanSummer >= 95 & CH_PrecipMeanSummer < 135  ~ "moist",
                              CH_PrecipMeanSummer >= 135 & CH_PrecipMeanSummer <= 174  ~ "wet"))


unique(coord.chelsa.combo.d$moisture)
coord.chelsa.combo.d$moisture <- as.factor(as.character(coord.chelsa.combo.d$moisture)) # moisture as factor
str(coord.chelsa.combo.d)

# write.csv(coord.chelsa.combo.d, file = "datasets/climate_data/coord.chelsa.combo.d.csv")

# Model 5a: biomass Vs temp*moisture
model_5a <- lmer(biomass ~ CH_TempMeanSummer*moisture + (1|gridcell), data = coord.chelsa.combo.d)
summary(model_5a)
# NOT significant interaction effect 


# model 5a output table 
stargazer(model_5a, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
pred_model_5a <- ggpredict(model_5a, terms = c("CH_TempMeanSummer", "moisture"))  # this gives overall predictions for the model
#write.csv(pred_model_5a, file = "datasets/pred_model_5a.csv")

plot_model(model_5a, type = "pred", terms = c("CH_TempMeanSummer", "moisture"))

# Plot the predictions 
(biomass_vs_moist_temp<- (ggplot(pred_model_5a) + 
                         geom_line(aes(x = x, y = predicted, group = group, colour = group)) +          # slope
                         geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, 
                                     fill = group), alpha = 0.5) +  # error band
                         geom_point(data = coord.chelsa.combo.d,                      # adding the raw data 
                                    aes(x = CH_TempMeanSummer, y = biomass, colour = moisture), size = 0.5) + 
                         labs(x = "\nMean summer temperature (degC)", y = "Shrub biomass (kg/m2)\n", 
                              title = "") + 
                         theme_shrub()))

# ggsave(file = "output/figures/biomass_vs_moist_temp.png")


# Model 5b: biomass Vs temp*precip
# Plot the predictions 
model_5b <- lmer(biomass ~ CH_TempMeanSummer*CH_PrecipMeanSummer+ (1|gridcell), data = coord.chelsa.combo.d)
summary(model_5b)
# NOT significant interaction

stargazer(model_5b, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


# Extracting model predictions 
pred_model_5b <- ggpredict(model_5b, terms = c("CH_TempMeanSummer", "CH_PrecipMeanSummer"))  # this gives overall predictions for the model
# write.csv(pred_model_5b, file = "datasets/pred_model_5b.csv")

plot_model(model_5b, type = "pred", terms = c("CH_TempMeanSummer", "CH_PrecipMeanSummer"))

# scatter: biomass ~ precip*temp
(scatter_precip <- ggplot(coord.chelsa.combo.d, aes(x = CH_TempMeanSummer, y = biomass, colour = moisture)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm") +
    theme_classic())

# END -----
