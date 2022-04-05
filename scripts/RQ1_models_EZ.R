##%######################################################%##
#                                                          #
###              RQ1: SPATIAL ANALYSIS                   ###
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##

### PART 2: MODELLING
## RQ1: How is shrub biomass distributed in the focal study area?
# Colour palette credit: David Nichols (WONG colour blind friendly palette)

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
library(tidyverse)
library(lme4)
library(Require)
library(SpaDES.tools)
library(geosphere)
library(dplyr)
library(ggeffects)
library(stargazer)
library(factoextra)
library(corrplot)
library(MuMIn)
library(performance)
library(png)
library(patchwork)


## LOADING DATA -----

# Loading the random sample dataset (1/3 of pixels)
r3_rsample_002 <- read_csv("datasets/berner_data/r3_rsample_002.csv")

# THEME ----

theme_shrub <- function(){ theme(legend.position = "right", 
                                 axis.title.x = element_text(size=25),
                                 axis.text.x  = element_text(vjust=0.5, size=20, colour = "black"), 
                                 axis.title.y = element_text(size=25),
                                 axis.text.y  = element_text(vjust=0.5, size=20, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}



## DATA MANIPULATION ----

# Exploring data
hist(r3_rsample_002$biomass) # distribution 
str(r3_rsample_002) # lat and long and biomass numeric
range(r3_rsample_002$biomass) # 9.820163 1003.684387

##  MODELLING ----
# Standardising lat and long (explanatory variables)
r3_rsample_002$latitude <- scale(r3_rsample_002$latitude, center = TRUE, scale = TRUE)
r3_rsample_002$longitude <- scale(r3_rsample_002$longitude, center = TRUE, scale = TRUE)

# MODEL 1. biomass vs lat ----
model_1 <- lm(biomass~latitude, data = r3_rsample_002)
summary(model_1)
# F-statistic: 639.3 on 1 and 3190 DF,  p-value: < 2.2e-16
# slope =  -49.079***

# Null model
model_1_null <- lm(biomass~1, data = r3_rsample_002)
AIC(model_1, model_1_null) # delta AIC indicates very diff models

# Checking model 1 assumptions 
plot(model_1)
qqnorm(resid(model_1))
qqline(resid(model_1))  # points fall nicely onto the line - good!

# Output table model 1
stargazer(model_1, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
predictions_1 <- as.data.frame(predict(model_1, newdata = r3_rsample_002, interval = "confidence")) # this gives overall predictions for the model
model_1_lat <- cbind(r3_rsample_002, predictions_1)

# Plot the predictions 
(predictions_biomass_vs_lat <- (ggplot(model_1_lat, aes(latitude, fit)) + 
                      geom_point(data = model_1_lat, aes(x = latitude, y = biomass), colour =  "#006146", size = 0.5) +
                      stat_smooth(method=lm, colour = "black", size = 2)+
                      geom_line(aes(y=lwr),  color = "#F96E00", linetype = "dashed", size = 0.5)+
                      geom_line(aes(y=upr), color = "#F96E00", linetype = "dashed", size = 0.5)+
                       # annotate(geom = "text", x = 2, y = 1100, label="(a)", size = 15) +
                        annotate(geom = "text", x = 1, y = 800, label="slope =  -49.079*** ", size = 10) +
                      xlab("\nScaled latitude") +
                      ylab(bquote("Shrub biomass "*(g~m^-2)*""))+ 
                      theme_shrub()+
                      theme(axis.title.x =element_text(size=25),
                              axis.title.y =element_text(size=25),
                              axis.text.x = element_text(size=25, hjust = 1),
                              axis.text.y = element_text(size=25, hjust = 1) )) )

# ggsave(file = "output/figures/predictions_biomass_vs_lat.png")

# adding icon
lat_logo <- readPNG("lat_icon.png")
raster_lat_logo <- as.raster(lat_logo)
(predictions_biomass_vs_lat <- predictions_biomass_vs_lat + annotation_raster(raster_lat_logo, 0.5, 1.5, 800, 1200))
# ggsave(file = "output/figures/predictions_biomass_vs_temp.png")

# Quick scatter to check predictions plotted well
(scatter_lat <- ggplot(r3_rsample_002, aes(x = latitude, y = biomass))+
    geom_point(color="#70B1A6", size = 0.1) +
    geom_smooth(method = lm, color ='black', fill = "grey", se=TRUE)+
    labs(x = "\nLatitude", y = "Shrub biomass (g/m2)\n") +
    annotate(geom = "text", x = 2, y = 1100, label="(a)", size = 10) +
    annotate(geom = "text", x = 1, y = 800, label="slope =  -49.079*** ", size = 6) +
    # title = "Shrub biomass decreases with latitude\n") + 
    theme_shrub())

# ggsave(file = "output/figures/biomass_vs_lat_scatter.png")

# MODEL 2. biomass vs long ----
model_2 <- lm(biomass~longitude, data = r3_rsample_002)
summary(model_2)
# F-statistic: 110.9 on 1 and 3190 DF,  p-value: < 2.2e-16***
# slope -22.021

# null model
model_2_null <- lm(biomass~1, data = r3_rsample_002)
AIC(model_2, model_2_null) # delta AIC indicates very different models

# Checking model 2 assumptions 
plot(model_2)
qqnorm(resid(model_2))
qqline(resid(model_2))  # points fall nicely onto the line - good!

# Output table model 2 
stargazer(model_2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
predictions_2 <- as.data.frame(predict(model_2, newdata = r3_rsample_002, interval = "confidence")) # this gives overall predictions for the model
model_2_long <- cbind(r3_rsample_002, predictions_2)

# Plot the predictions 
(predictions_biomass_vs_long <- (ggplot(model_2_long, aes(longitude, fit)) + 
                      geom_point(data = model_2_long, aes(x= longitude, y = biomass), colour =  "#006146", size = 0.5) +
                      stat_smooth(method=lm, colour = "black", size = 2)+
                      geom_line(aes(y=lwr),  color = "#F96E00", linetype = "dashed", size = 0.5)+
                      geom_line(aes(y=upr), color = "#F96E00", linetype = "dashed", size = 0.5)+
                       # annotate(geom = "text", x = 2, y = 1100, label="(b)", size = 15) +
                        annotate(geom = "text", x = 1, y = 800, label="slope = -22.021*** ", size = 10) +
                        xlab("\nScaled longitude") +
                        ylab(bquote("Shrub biomass "*(g~m^-2)*""))+ 
                        theme_shrub()+
                        theme(axis.title.x =element_text(size=25),
                              axis.title.y =element_text(size=25),
                              axis.text.x = element_text(size=25, hjust = 1),
                              axis.text.y = element_text(size=25, hjust = 1) )) )

# ggsave(file = "output/figures/predictions_biomass_vs_long.png")

# adding icon
long_logo <- readPNG("long_icon.png")
raster_long_logo <- as.raster(long_logo)
(predictions_biomass_vs_long <- predictions_biomass_vs_long + annotation_raster(raster_long_logo, 0, 2, 900, 1150))
ggsave(file = "output/figures/predictions_biomass_vs_temp.png")

# Quick scatter to check predictions plotted well
(scatter_lon <- ggplot(r3_rsample_002, aes(x = longitude, y = biomass)) +
    geom_point(color="skyblue", size = 0.01) +
    geom_smooth(method = lm, colour='black') +
    labs(x = "\nLongitude", y = "Shrub biomass (kg/m2)\n") +  
    annotate(geom = "text", x = 2, y = 1250, label="(b)", size = 15) +
    annotate(geom = "text", x = 1, y = 900, label="slope = -22.021*** ", size = 10) +
    # title = "Shrub biomass decreases with longitude\n") + 
    theme_shrub())

# ggsave(file = "output/figures/biomass_vs_long_scatter.png")

# Making panel
# Panel of scatters 
(panel_latlong_predictions <- grid.arrange(arrangeGrob(predictions_biomass_vs_lat, predictions_biomass_vs_long,
                                           ncol = 2))) # Sets number of panel columns

# ggsave(panel_latlong_predictions, file = "output/figures/panel_latlong_predictions.png", width = 18, height = 9)

# BIOMASS LEVELS ----

# Exploring dataset
mean(r3_rsample_002$biomass) 
#  267.5807 g/m2 mean biomass
range(r3_rsample_002$biomass)
#  9.820163 1003.684387
quantile(r3_rsample_002$biomass)
# 0%         25%         50%         75%        100% 
#  9.820163  170.630482  257.406342  347.062210 1003.684387 

# Categorising into high-medium-low level using quantiles (25% and 75%)
r3_rsample_categ <- r3_rsample_002 %>%
  mutate(biomass_level = case_when (biomass < 170.630482     ~ 'Low', # lower than 25% quantile
                                    biomass > 170.630482    & biomass < 347.062210 ~ 'Medium', 
                                    biomass > 347.062210 ~ 'High')) %>% 
 dplyr::select(latitude, longitude, biomass, biomass_level)

# Making biomass level a factor
r3_rsample_categ$biomass_level <- as.factor(as.character(r3_rsample_categ$biomass_level))
# reordeing factor levels 
r3_rsample_categ$biomass_level <- factor(r3_rsample_categ$biomass_level,levels = c("Low", "Medium", "High"),
                          labels = c("Low", "Medium", "High"),
                          ordered = T)

# HISTOGRAM of biomass ----
(hist_high_medium_low <- r3_rsample_categ %>%
    ggplot(aes(x = biomass, fill = biomass_level)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 60) +
    geom_vline(aes(xintercept = mean(biomass)),            
               colour = "black", linetype = "dashed", size = 1) +
    annotate(geom = "text", x = 450, y = 200, label="mean = 267.6", size = 14) +
    geom_curve(aes(x = 470, y = 210, xend = mean(biomass) + 2, yend = 210),
               arrow = arrow(length = unit(0.07, "inch")), size = 1,
               color = "grey30", curvature = 0.3) +
    ylab("Frequency\n") +
    xlab(bquote("Shrub biomass "*(g~m^-2)*""))+ 
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    theme_shrub()+
    theme(axis.title.x =element_text(size=25),
          axis.title.y =element_text(size=25),
          axis.text.x = element_text(size=25, hjust = 1),
          axis.text.y = element_text(size=25, hjust = 1),
          legend.text = element_text(size=20),
          legend.title = element_text(size=25),
          legend.position = "bottom"))

# ggsave(file = "output/figures/hist_high_medium_low.png")

# adding shrub logo
shrub_logo <- readPNG("team_shrub_logo.png")
raster_logo <- as.raster(shrub_logo)
(histogram <- hist_high_medium_low + annotation_raster(raster_logo, 750, 900, 150, 200))
# ggsave(file = "output/figures/histogram.png")

##################################################################### END -----




