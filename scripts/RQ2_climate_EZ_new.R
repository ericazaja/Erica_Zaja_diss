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
library(gridExtra)
library(ggpubr)

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
plot(precip, main = "Mean monthly precipitation of the warmest quarter (kg m-2)")
precip_raster <- levelplot(precip)
temp_raster <- levelplot(temp)

# EXTRACTION ------
# Loading the coordinates of the cropped shrub map
coords <- read.csv("datasets/berner_data/r3_rsample_00.csv") %>% 
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
                                 axis.title.x = element_text(face="bold", size=18),
                                 axis.text.x  = element_text(vjust=0.5, size=15, colour = "black"), 
                                 axis.title.y = element_text(face="bold", size=18),
                                 axis.text.y  = element_text(vjust=0.5, size=15, colour = "black"),
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
# biomass ~ temp 
model_3 <- lm(biomass ~ CH_TempMeanSummer, data = coord.chelsa.combo.c)
summary(model_3)
# F-statistic: 993.7 on 1 and 9573 DF,  p-value: < 2.2e-16

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
# shrub biomass increases with mean summer temp

# Extracting model predictions 
pred_model_3 <- ggpredict(model_3, terms = c("CH_TempMeanSummer"))  # this gives overall predictions for the model
# write.csv(pred_model_3, file = "datasets/pred_model_3.csv")

# Plot the predictions 
(biomass_vs_temp <- (ggplot(pred_model_3) + 
                    geom_point(data = coord.chelsa.combo.c,                      # adding the raw data 
                                  aes(x = CH_TempMeanSummer, y = biomass), color = '#2980B9', size = 0.5) + 
                   geom_line(aes(x = x, y = predicted)) +          # slope
                   geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                               fill = "lightgrey", alpha = 0.5) +  # error band
                   labs(x = "\nMean summer temperature (°C)", y = "Shrub biomass (kg/m2)\n") +  
                        # title = "Shrub biomass increases with temperature\n") + 
                   theme_shrub()))

ggsave(file = "output/figures/biomass_vs_temp.png")

# Quick scatter: biomass ~ temp
(scatter_temp <- ggplot(coord.chelsa.combo.c, aes(x = CH_TempMeanSummer, y = biomass)) +
    geom_point(color="skyblue", size = 0.1) +
    geom_smooth(method = "lm", color = "black") +
    annotate(geom = "text", x = 10.5, y = 900, label="(a)", size = 10) +
    annotate(geom = "text", x = 8, y = 700, label="slope = 59.678*** ", size = 6) +
    labs(x = "\nMean summer temperature (°C)", y = "Shrub biomass (kg/m2)\n") + 
         # title = "Shrub biomass increases with temperature\n") + 
    theme_shrub())

ggsave(file = "output/figures/scatter_temp.png")

# Model 4  -----
# biomass ~ precip 
model_4 <- lm(biomass ~ CH_PrecipMeanSummer, data = coord.chelsa.combo.c)
summary(model_4)
# F-statistic:  1746 on 1 and 9573 DF,  p-value: < 2.2e-16

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
              geom_point(data = coord.chelsa.combo.c,                      # adding the raw data 
              aes(x = CH_PrecipMeanSummer, y = biomass), color = '#2980B9',  size = 0.5) + 
                   geom_line(aes(x = x, y = predicted)) +          # slope
                   geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                               fill = "lightgrey", alpha = 0.5) +  # error band
                   labs(x = "\nMean summer precipitation (kg/m2)", y = "Shrub biomass (kg/m2)\n")+ 
                        # title = "Shrub biomass increases with precipiation\n") + 
                   theme_shrub()))

ggsave(file = "output/figures/biomass_vs_precip.png")

# Quick scatter: biomass ~precip
(scatter_precip <- ggplot(coord.chelsa.combo.c, aes(x = CH_PrecipMeanSummer, y = biomass)) +
    geom_point(color="skyblue", size = 0.1) +
    geom_smooth(method = "lm", color = "black") +
    annotate(geom = "text", x = 135, y = 900, label="(b)", size = 10) +
     annotate(geom = "text", x = 125, y = 700, label="slope =  3.90713*** ", size = 6) +
    labs(x = "\nMean summer precipitation (kg/m2)", y = "Shrub biomass (kg/m2)\n") +
         # title = "Shrub biomass increases with precipiation\n") + 
    theme_shrub())

ggsave(file = "output/figures/scatter_precip.png")

# Panel  -----
# panel of scatters 
panel_title <- text_grob("Shrub biomass increases with mean summer temperature and precipitation",
                         size = 18, face = "bold")

(panel_scatter <- grid.arrange(arrangeGrob(scatter_temp, scatter_precip,
                                 ncol = 2)) )# Sets number of panel columns
                            # top = panel_title  # Adding panel title
                          

# Panel of model predictions
(panel_model_pred<- grid.arrange(arrangeGrob(biomass_vs_temp, biomass_vs_precip,
                                          ncol = 2),  # Sets number of panel columns
                              top = panel_title  # Adding panel title
)) 

ggsave(panel_scatter, file = "output/figures/panel_scatter.png", width = 18, height = 9)
# ggsave(panel_model_pred, file = "output/figures/panel_model_pred.png", width = 18, height = 9)



# Model 5 ----
# biomass ~ temp*precip

# To display interaction: categorise precipitation 'dry', 'moist', 'wet': 
# Plot 3 lines in plot with temp on the x and biomass on y and points coloured by moisture level
range(coord.chelsa.combo.c$CH_PrecipMeanSummer)
quantile(coord.chelsa.combo.c$CH_PrecipMeanSummer)
# 0%  25%  50%  75% 100% 
# 60   78   86   93  136 
# 55 (min) 174 (max precip (kg m-2))
# 174-55 = 119
# 119/3 = 39.66667
# 55 + 40 = 95 
# 95 + 40 = 135
# 135+40 = 175
# 55 (dry), 114.5 (moist), 174 (wet)
mean(coord.chelsa.combo.c$CH_PrecipMeanSummer)
# 86.44856

coord.chelsa.combo.d <- coord.chelsa.combo.c %>% 
  mutate(moisture = case_when(CH_PrecipMeanSummer < 78 ~ "dry",
                              CH_PrecipMeanSummer >= 78 & CH_PrecipMeanSummer < 93 ~ "moist",
                              CH_PrecipMeanSummer >= 93 ~ "wet"))



unique(coord.chelsa.combo.d$moisture)
coord.chelsa.combo.d$moisture <- as.factor(as.character(coord.chelsa.combo.d$moisture)) # moisture as factor
str(coord.chelsa.combo.d)

write.csv(coord.chelsa.combo.d, file = "datasets/climate_data/coord.chelsa.combo.d.csv")

# Model 5a: biomass Vs temp*moisture
model_5a <- lm(biomass ~ CH_TempMeanSummer*moisture , data = coord.chelsa.combo.d)
summary(model_5a)
#F-statistic: 529 on 5 and 9569 DF,  p-value: < 2.2e-16

# model 5a output table 
stargazer(model_5a, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
pred_model_5a <- ggpredict(model_5a, terms = c("CH_TempMeanSummer", "moisture"))  # this gives overall predictions for the model
#write.csv(pred_model_5a, file = "datasets/pred_model_5a.csv")

plot_model(model_5a, type = "pred", terms = c("CH_TempMeanSummer", "moisture"))

coord.chelsa.combo.d$moisture <- factor(coord.chelsa.combo.d$moisture,levels=c("dry", "moist", "wet"),
                                         labels = c("dry", "moist", "wet"),
                                         ordered = T)


# Plot the predictions 
(biomass_vs_moist_temp<- (ggplot(pred_model_5a) + 
                         geom_line(aes(x = x, y = predicted, group = group, colour = group)) +          # slope
                         geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, 
                                     fill = group), alpha = 0.5) +
                           scale_fill_manual(values = c("brown", "skyblue", "blue4"), name = "Moisture level")+
                         geom_point(data = coord.chelsa.combo.d,                      # adding the raw data 
                                    aes(x = CH_TempMeanSummer, y = biomass, colour = moisture), size = 0.3) + 
                           scale_color_manual(values = c("brown", "skyblue", "blue4"), name = "Moisture level")+
                         labs(x = "\nMean summer temperature (°C)", y = "Shrub biomass (kg/m2)\n", 
                              title = "") + 
                         theme_shrub()+
                           theme(legend.text = element_text(size= 12),
                                 legend.title = element_text(size=15))))

ggsave(file = "output/figures/biomass_vs_moist_temp.png")


# Model 5b: biomass Vs temp*precip
# Plot the predictions 
model_5b <- lm(biomass ~ CH_TempMeanSummer*CH_PrecipMeanSummer, data = coord.chelsa.combo.d)
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

(biomass_vs_temp<- (ggplot(pred_model_5b) + 
                            geom_line(aes(x = x, y = predicted, group = group, colour = group)) +          # slope
                            geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, 
                                            fill = group), alpha = 0.5) +  # error band
                            geom_point(data = coord.chelsa.combo.d,                      # adding the raw data 
                                       aes(x = CH_TempMeanSummer, y = biomass), size = 0.3) + 
                            labs(x = "\nMean summer temperature (degC)", y = "Shrub biomass (kg/m2)\n", 
                                 title = "") + 
                            theme_shrub())) # makes dry moist wet categories

ggsave(file = "output/figures/biomass_vs_temp.png")

# scatter: biomass ~ precip*temp
(scatter_precip <- ggplot(coord.chelsa.combo.d, aes(x = CH_TempMeanSummer, y = biomass, colour = moisture)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm") +
    theme_classic())


# Checking temp ~ lat -----
temp_lat_model <- lm(CH_TempMeanSummer~ latitude, data=coord.chelsa.combo.c)
summary(temp_lat_model) 
#F-statistic:  1146 on 1 and 9573 DF,  p-value: < 2.2e-16***
# temp decreases with latitude. I.e. northern latitudes colder

(scatter_lat_temp <- ggplot(coord.chelsa.combo.c, aes(x =latitude , y = CH_TempMeanSummer )) +
   geom_point(size = 0.3, color="skyblue") +
   geom_smooth(method = "lm", colour = "black") +
    labs(x = "\nLatitude", y = "Mean summer temperature (°C)\n")+ 
    annotate(geom = "text", x = 70, y = 11, label="(a)", size = 10) +
    annotate(geom = "text", x = 69.9, y = 7, label="slope = -1.24338*** ", size = 6) +
   theme_shrub())
  #+ theme(axis.title.y =element_text(size=12), 
                        axis.title.x = element_text(size=12)))

ggsave(file = "output/figures/scatter_lat_temp.png")

# Checking temp ~ long -----
temp_long_model <- lm(CH_TempMeanSummer ~ longitude, data=coord.chelsa.combo.c)
summary(temp_long_model) 
#F-statistic: 172.3 on 1 and 9573 DF,  p-value: < 2.2e-16 ***
# temp increases with longitude. 

(scatter_long_temp <- ggplot(coord.chelsa.combo.c, aes(x =longitude , y = CH_TempMeanSummer )) +
    geom_point(size = 0.3, color = "skyblue") +
    geom_smooth(method = "lm", color="black") +
    labs(x = "\nLongitude", y = "Mean summer temperature (°C)\n")+ 
    annotate(geom = "text", x = -141, y = 11, label="(b)", size = 10) +
    annotate(geom = "text", x = -142, y = 7, label="slope = 0.061870*** ", size = 6) +
    theme_shrub())
  #+ theme(axis.title.y =element_text(size=12), 
                         #axis.title.x = element_text(size=12)))

ggsave(file = "output/figures/scatter_long_temp.png")

# Checking precip ~ lat -----
precip_lat_model <- lm(CH_PrecipMeanSummer ~ latitude, data=coord.chelsa.combo.c)
summary(precip_lat_model) 
#F-statistic: 3.321e+04 on 1 and 9573 DF,  p-value: < 2.2e-16***
# precip decreases with latitude too

(scatter_lat_precip <- ggplot(coord.chelsa.combo.c, aes(x =latitude , y = CH_PrecipMeanSummer )) +
    geom_point(size = 0.3, color = "skyblue") +
    geom_smooth(method = "lm", color="black") +
    labs(x = "\nLatitude", y = "Mean summer precipitation (kg/m2)\n")+ 
    annotate(geom = "text", x = 70, y = 125, label="(c)", size = 10) +
    annotate(geom = "text", x = 69.9, y = 100, label="slope = -65.5282*** ", size = 6) +
    theme_shrub() )
  #+ theme(axis.title.y =element_text(size=12), 
          #axis.title.x = element_text(size=12)))

ggsave(file = "output/figures/scatter_lat_precip.png")

# Checking precip ~ long -----
precip_long_model <- lm(CH_PrecipMeanSummer~ longitude, data=coord.chelsa.combo.c)
summary(precip_long_model) 
# F-statistic: 214.5 on 1 and 9573 DF,  p-value: < 2.2e-16***
# precip increases wih long 

(scatter_long_precip<- ggplot(coord.chelsa.combo.c, aes(x =longitude , y = CH_PrecipMeanSummer )) +
    geom_point(size = 0.3, color = "skyblue") +
    geom_smooth(method = "lm", color="black") +
    labs(x = "\nLongitude", y = "Mean summer precipitation (kg/m2)\n")+ 
    annotate(geom = "text", x = -141.5, y = 125, label="(d)", size = 10) +
    annotate(geom = "text", x = -141.5, y = 70, label="slope = 1.34762*** ", size = 6) +
    theme_shrub())
  #+ theme(axis.title.y =element_text(size=12), 
                         #axis.title.x = element_text(size=12)))
    
ggsave(file = "output/figures/scatter_long_precip.png")

# Panel -----
(panel_temp_precip_coords <- grid.arrange(arrangeGrob(scatter_lat_temp, scatter_long_temp,
                                           scatter_lat_precip, scatter_long_precip,
                                           ncol = 2))) # Sets number of panel columns

ggsave(panel_temp_precip_coords, file="output/figures/panel_temp_precip_coords.png", height = 16, width = 15)

#  top = panel_title  # Adding panel title



# END -----
