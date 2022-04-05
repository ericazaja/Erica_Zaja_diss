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


# Extra model:  biomass vs long*lat  (not used in final diss)
model_2a <- lm(biomass~longitude*latitude, data = r3_rsample_002)
summary(model_2a)
# F-statistic: 353.7 on 3 and 3188 DF,  p-value: < 2.2e-16***
# checking correlation betwee lat and long
cor.test( r3_rsample_002$latitude,r3_rsample_002$longitude, method = "pearson")
# t = -11.449, df = 3190, p-value < 2.2e-16
# lat and long are correlated 
# Does not mean much: as latitude increases longitude decreases


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

# Random slope and intercept biomass level across latitudes ----
# Standardising explanatory variables
r3_rsample_categ$latitude <- scale(r3_rsample_categ$latitude, center = TRUE, scale = TRUE)
r3_rsample_categ$longitude <- scale(r3_rsample_categ$longitude, center = TRUE, scale = TRUE)

level_rs <- lmer(biomass ~ latitude + (1 + latitude|biomass_level), data = r3_rsample_categ)
summary(level_rs)
#  latitude estimate:  -16.298    
r.squaredGLMM(level_rs) # same as below
r2_nakagawa(level_rs)

# null model
level_rs_null <- lm(biomass ~ 1, data = r3_rsample_categ)
AIC(level_rs, level_rs_null)

stargazer(level_rs, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Random intercepts and slopes 
predict_levels <- ggpredict(level_rs , terms = c("latitude", "biomass_level"), type = "re") 
(levels_rand_slopes <- ggplot(predict_levels, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE, size=1.5)  +
    scale_colour_manual(values=c("#F0E442", "#E69F00", "#009E73"), name = "Biomass level") + 
    theme(legend.position = "bottom") +
    annotate(geom = "text", x = 2, y = 510, label="(a)", size = 15) +
    xlab("\nScaled latitude") +
    ylab(bquote("Shrub biomass "*(g~m^-2)*""))+ 
    theme_shrub()+ 
    theme(legend.text=element_text(size=25),
          legend.title=element_text(size=30)))

ggsave(levels_rand_slopes, file = "output/figures/levels_rand_slopes.png")


# Random slope and intercept biomass level across longitudes ----
level_rs_long <- lmer(biomass ~ longitude + (1 + longitude|biomass_level), data = r3_rsample_categ)
summary(level_rs_long)
# long  estimate:  3.245  (not sig)
r.squaredGLMM(level_rs_long) # same as below
r2_nakagawa(level_rs_long)

# null model
level_rs_long_null <- lm(biomass ~ 1, data = r3_rsample_categ)
AIC(level_rs_long, level_rs_long_null)

stargazer(level_rs_long, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Random intercepts and slopes 
predict_levels_long <- ggpredict(level_rs_long , terms = c("longitude", "biomass_level"), type = "re") 

(levels_rand_slopes_long <- ggplot(predict_levels_long, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE, size = 1.5)  +
    scale_colour_manual(values=c("#F0E442", "#E69F00", "#009E73"), name = "Biomass level") + 
    theme(legend.position = "bottom") +
    xlab("\nScaled longitude") +
    ylab(bquote("Shrub biomass "*(g~m^-2)*""))+ 
    annotate(geom = "text", x = 2, y = 510, label="(b)", size = 15) +
    theme_shrub()+ 
    theme(legend.text=element_text(size=25),
          legend.title=element_text(size=30)))
          

ggsave(levels_rand_slopes_long, file = "output/figures/levels_rand_slopes_long.png")

(panel_slopes_levels <- ggarrange(levels_rand_slopes, levels_rand_slopes_long,
                                                  ncol = 2, common.legend = TRUE, legend="bottom"))# Sets number of panel columns


ggsave(panel_slopes_levels, file = "output/figures/panel_slopes_levels.png", height = 10, width=18)

# Filter for high/medium/low biomass separately
# 1. HIGH  ----
r3_high_biomass <- r3_rsample_categ %>% filter (biomass_level == "High")
model_lat_high <- lm(biomass~latitude, data = r3_high_biomass )
summary(model_lat_high)
# F-statistic: 119.7 on 1 and 796 DF,  p-value: < 2.2e-16***

# null model 
model_lat_high_null <- lm(biomass~1, data = r3_high_biomass )
AIC(model_lat_high, model_lat_high_null)

(scatter_high_lat <- ggplot(r3_high_biomass, aes(x = latitude, y = biomass)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLatitude", y = "High biomass (g/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))

model_long_high <- lm(biomass~longitude, data = r3_high_biomass )
summary(model_long_high) 
# F-statistic: 177.2 on 1 and 2393 DF,  p-value: < 2.2e-16***

# null model 
model_long_high_null <- lm(biomass~1, data = r3_high_biomass )
AIC(model_long_high, model_long_high_null)

(scatter_high_long <- ggplot(r3_high_biomass, aes(x = longitude, y = biomass)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLongitude", y = "High biomass (kg/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))

# 2. MEDIUM -----
r3_med_biomass <- r3_rsample_categ %>% filter (biomass_level == "Medium")
model_lat_med <- lm(biomass~latitude, data = r3_med_biomass )
summary(model_lat_med)
# F-statistic: 207.7 on 1 and 4787 DF,  p-value: < 2.2e-16***

# null model
model_lat_med_null <- lm(biomass~1, data = r3_med_biomass )
AIC(model_lat_med, model_lat_med_null)

(scatter_med_lat <- ggplot(r3_med_biomass, aes(x = latitude, y = biomass)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLatitude", y = "Medium biomass (kg/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))

model_long_med <- lm(biomass~longitude, data = r3_med_biomass )
summary(model_long_med) 
#F-statistic: 358.1 on 1 and 4787 DF,  p-value: < 2.2e-16***

# null model
model_long_med_null <- lm(biomass~1, data = r3_med_biomass )
AIC(model_long_med, model_long_med_null)

(scatter_med_long <- ggplot(r3_med_biomass, aes(x = longitude, y = biomass)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLongitude", y = "Medium biomass (kg/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))


# 3. LOW -----
r3_low_biomass <- r3_rsample_categ %>% filter (biomass_level == "Low")
model_lat_low <- lm(biomass~latitude, data = r3_low_biomass )
summary(model_lat_low)
# F-statistic: 22.18 on 1 and 2393 DF,  p-value: 2.624e-06***

# null model
model_lat_low_null <- lm(biomass~1, data = r3_low_biomass )
AIC(model_lat_low, model_lat_low_null)

(scatter_low_lat <- ggplot(r3_low_biomass, aes(x = latitude, y = biomass)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLatitude", y = "Low biomass (kg/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))

model_long_low <- lm(biomass~longitude, data = r3_low_biomass )
summary(model_long_low) 
# F-statistic: 79.16 on 1 and 2393 DF,  p-value: < 2.2e-16***

# null model
model_long_low_null <- lm(biomass~1, data = r3_low_biomass )
AIC(model_long_low, model_long_low_null)

(scatter_low_long <- ggplot(r3_low_biomass, aes(x = longitude, y = biomass)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    labs(x = "\nLongitude", y = "Low biomass (kg/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))


(scatter <- ggplot(r3_rsample_categ, aes(x = latitude, y = biomass, colour = biomass_level)) +
    geom_point(color='#8DCCB8', size = 0.1) +
    geom_smooth(method = "lm", colour='black') +
    facet_wrap(~biomass_level)+
    labs(x = "\nLongitude", y = "Low biomass (kg/m2)\n") + 
    theme_shrub() + theme(axis.title.y =element_text(size=12), 
                          axis.title.x = element_text(size=12)))
                          



# Panel latlong levels -----
(panel_latlong_levels <- grid.arrange(arrangeGrob(scatter_low_lat, scatter_low_long,
                                                  scatter_med_lat, scatter_med_long,
                                                  scatter_high_lat, scatter_high_long,
                                           ncol = 2))) # Sets number of panel columns

ggsave(panel_latlong_levels,  file="output/figures/panel_latlong_levels.png", height = 16, width = 15)

# Kmeans ----
# Kmeans clustering: Biomass level ~ lat 
set.seed(99)
clusters <- kmeans(na.omit(r3_rsample_categ$biomass), centers = 3, nstart = 25)
cluster.df <- as.data.frame(clusters$cluster)
cluster_plot <- fviz_cluster(clusters, data = r3_rsample_categ)

r3_rsample_categ_clust <- r3_rsample_categ %>%
  add_column(cluster = clusters$cluster)

# adding cluster column to dataframe

r3_rsample_categ_clust$cluster <- as.factor(as.character(r3_rsample_categ_clust$cluster ))
str(r3_rsample_categ_clust$cluster)

r3_rsample_categ_clust <- r3_rsample_categ_clust %>%
  mutate(cluster_level = case_when (cluster == "2" ~ 'Low', # 1 = low level
                                      cluster == "1" ~ 'Medium', # 2 = medium level
                                      cluster == "3" ~ 'High')) 
r3_rsample_categ_clust$cluster_level <- as.factor(as.character(r3_rsample_categ_clust$cluster_level ))

# reordeing factor levels 
r3_rsample_categ_clust$cluster_level <- factor(r3_rsample_categ_clust$cluster_level,
                                               levels=c("Low","Medium", "High"),
                                         labels = c("Low","Medium", "High"),
                                         ordered = T)

(scatter_high_medium_low_lat <- ggplot(r3_rsample_categ_clust) +
    geom_point(aes(x = latitude, y = biomass, colour = cluster_level), size = 0.3) +
   scale_colour_manual(values = c("Low"= "tan", "Medium" = "yellow", "High"= "green4"), name = "Biomass level")+
     geom_smooth(aes(x = latitude, y = biomass), colour = "black", method = "lm") +
    facet_wrap(~cluster_level) +
   #  scale_fill_manual(name = "Biomass level", values=c( "tan", "yellow", "green4")) +
    labs(x= "\nLatitude", y = "Shrub biomass (kg/m2)\n") +
    theme_shrub()+
    theme(axis.text.x = element_text(vjust=0.5, angle = 45, size=12, colour = "black"), 
))

ggsave(file = "output/figures/scatter_high_medium_low_lat.png")

(scatter_high_medium_low_long <- ggplot(r3_rsample_categ_clust) +
    geom_point(aes(x = longitude, y = biomass, colour = cluster_level), size = 0.3) +
    scale_colour_manual(values = c("Low"= "tan", "Medium" = "yellow", "High"= "green4"), name = "Biomass level")+
    geom_smooth(aes(x = longitude, y = biomass), colour = "black", method = "lm") +
    facet_wrap(~cluster_level) +
    #  scale_fill_manual(name = "Biomass level", values=c( "tan", "yellow", "green4")) +
    labs(x= "\nLongitude", y = "Shrub biomass (kg/m2)\n") +
    theme_shrub() +
  theme(axis.text.x = element_text(vjust=0.5, angle = 45, size=12, colour = "black"), 
  ))


ggsave(file = "output/figures/scatter_high_medium_low_long.png")
dev.off()

# Plotting kmeans
copy_raster <- r3_latlong_agg
# Now replace raster cell values with 
# array
copy_raster[] <- clusters$cluster
plot(copy_raster, main = "Kmeans", col = viridis_pal(option = "D")(3))




# END -----




