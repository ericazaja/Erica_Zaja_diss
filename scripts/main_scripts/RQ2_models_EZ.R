##%######################################################%##
#                                                          #
####         RQ2: SHRUB BIOMASS VS CLIMATE              #### 
#               Erica Zaja - 22/10/2021                   ##
#                                                         #
##%######################################################%##

# RQ2: how does biomass vary with temperature and precipitation?
# PART 2: MODELLING ----

# Libraries
library(sjPlot)

# Loading dataset 
coord.chelsa.combo.c <- read_csv("datasets/climate_data/coord_chelsa_combo_new.csv")

# THEME -----
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

# DATA MANIPULATION ----
range(coord.chelsa.combo.c$CH_TempMeanSummer) # 7 11
range(coord.chelsa.combo.c$CH_PrecipMeanSummer) # 61 131
glimpse(coord.chelsa.combo.c)
str(coord.chelsa.combo.c)
unique(coord.chelsa.combo.c$gridcell)

# MODELLING ----
# Standardising explanatory variables
coord.chelsa.combo.c$CH_TempMeanSummer <- scale(coord.chelsa.combo.c$CH_TempMeanSummer, center = TRUE, scale = TRUE)
coord.chelsa.combo.c$CH_PrecipMeanSummer <- scale(coord.chelsa.combo.c$CH_PrecipMeanSummer, center = TRUE, scale = TRUE)

# MODEL 3: biomass ~ temp  ----
model_3 <- lm(biomass ~ CH_TempMeanSummer, data = coord.chelsa.combo.c)
summary(model_3)
# F-statistic: 355.5 on 1 and 3190 DF,  p-value: < 2.2e-16
# slope =  38.033  ***

# Checking model 3 assumptions
plot(model_3)
qqnorm(resid(model_3))
qqline(resid(model_3))  # points fall nicely onto the line - good!

# Output table model 3 
stargazer(model_3, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") 

# Extracting model predictions 
predictions_3 <- as.data.frame(predict(model_3, newdata = coord.chelsa.combo.c, interval = "confidence")) # this gives overall predictions for the model
model_3_preds <- cbind(coord.chelsa.combo.c, predictions_3)

# Plot the predictions 
(predictions_biomass_vs_temp <- (ggplot(model_3_preds, aes(CH_TempMeanSummer, fit)) + 
                                  geom_point(data = model_3_preds, aes(x= CH_TempMeanSummer, y =biomass), colour = "#006146", size = 0.5) +
                                  stat_smooth(method=lm, colour = "black", size = 2)+
                                  geom_line(aes(y=lwr),  color = "#F96E00", linetype = "dashed", size = 0.5)+
                                  geom_line(aes(y=upr), color = "#F96E00", linetype = "dashed", size = 0.5)+
                                   #annotate(geom = "text", x = 2.5, y = 1100, label="(a)", size = 15) +
                                   annotate(geom = "text", x = 0, y = 800, label="slope = 38.033*** ", size = 10) +
                                  xlab("Scaled mean summer temperature (°C)") +
                                  ylab(bquote("Shrub biomass "*(g~m^-2)*"")) + 
                                         theme_shrub()+
                                   theme(axis.title.x =element_text(size=25, face = "plain"),
                                         axis.title.y =element_text(size=25),
                                         axis.text.x = element_text(size=25, hjust = 1),
                                         axis.text.y = element_text(size=25, hjust = 1) )) )



# ggsave(file = "output/figures/predictions_biomass_vs_temp.png")

# adding temp logo
temp_logo <- readPNG("temp_logo.png")
raster_temp_logo <- as.raster(temp_logo)
(predictions_biomass_vs_temp <- predictions_biomass_vs_temp + annotation_raster(raster_temp_logo, -4.5, -2.3, 900, 1150))
# ggsave(file = "output/figures/predictions_biomass_vs_temp.png")

# Quick scatter: biomass ~ temp
(scatter_temp <- ggplot(coord.chelsa.combo.c, aes(x = CH_TempMeanSummer, y = biomass)) +
    geom_point(color="skyblue", size = 0.1) +
    geom_smooth(method = "lm", color = "black") +
    annotate(geom = "text", x = 2.5, y = 1200, label="(a)", size = 10) +
    annotate(geom = "text", x = 0, y = 800, label="slope = 36.433*** ", size = 6) +
    labs(x = "\nMean summer temperature (°C)", y = "Shrub biomass (g/m2)\n") + 
         # title = "Shrub biomass increases with temperature\n") + 
    theme_shrub())

# ggsave(file = "output/figures/scatter_temp.png")

# MODEL 4: biomass ~ precip  ----
model_4 <- lm(biomass ~ CH_PrecipMeanSummer, data = coord.chelsa.combo.c)
summary(model_4)
# F-statistic: 585.1 on 1 and 3190 DF,  p-value: < 2.2e-16
# slope = 47.290***


# Checking model 4 assumptions 
plot(model_4)
qqnorm(resid(model_4))
qqline(resid(model_4))  # points fall nicely onto the line - good!

# Output table model 4 
stargazer(model_4, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
predictions_4 <- as.data.frame(predict(model_4, newdata = coord.chelsa.combo.c, interval = "confidence")) # this gives overall predictions for the model
model_4_preds <- cbind(coord.chelsa.combo.c, predictions_4)

# Plot the predictions 
(predictions_biomass_vs_precip <- (ggplot(model_4_preds, aes(CH_PrecipMeanSummer, fit)) + 
                                   geom_point(data = model_4_preds, aes( x= CH_PrecipMeanSummer, y = biomass),colour = "#006146", size = 0.5) +
                                   stat_smooth(method=lm, colour = "black", size = 2)+
                                     geom_line(aes(y=lwr),  color = "#F96E00", linetype = "dashed", size = 0.5)+
                                     geom_line(aes(y=upr), color = "#F96E00", linetype = "dashed", size = 0.5)+
                                     #annotate(geom = "text", x = 4, y = 1000, label="(b)", size = 15) +
                                     annotate(geom = "text", x = 3, y = 700, label="slope = 47.290*** ", size = 10) +
                                    xlab(bquote("\nScaled mean summer precipitation "*(g~m^-2)*""))+
                                    ylab(bquote("Shrub biomass "*(g~m^-2)*""))+
                                    theme_shrub()+
                                     theme(axis.title.x =element_text(size=25),
                                           axis.title.y =element_text(size=25),
                                           axis.text.x = element_text(size=25, hjust = 1),
                                           axis.text.y = element_text(size=25, hjust = 1) )) )


                                     
# ggsave(file = "output/figures/predictions_biomass_vs_precip.png")

# adding rain logo
rain_logo <- readPNG("rain_logo.png")
raster_rain_logo <- as.raster(rain_logo)
(predictions_biomass_vs_precip <- predictions_biomass_vs_precip + annotation_raster(raster_rain_logo, -2, 0, 750, 1000))
ggsave(file = "output/figures/predictions_biomass_vs_precip.png")

# Quick scatter: biomass ~precip
(scatter_precip <- ggplot(coord.chelsa.combo.c, aes(x = CH_PrecipMeanSummer, y = biomass)) +
    geom_point(color="skyblue", size = 0.1) +
    geom_smooth(method = "lm", color = "black") +
    annotate(geom = "text", x = 4, y = 1000, label="(b)", size = 10) +
    annotate(geom = "text", x = 3, y = 700, label="slope =  46.655*** ", size = 6) +
    labs(x = "\nMean summer precipitation (g/m2)", y = "Shrub biomass (kg/m2)\n") +
         # title = "Shrub biomass increases with precipiation\n") + 
    theme_shrub())

# ggsave(file = "output/figures/scatter_precip.png")

# Panel 
(panel_model_pred<- grid.arrange(arrangeGrob(predictions_biomass_vs_temp, predictions_biomass_vs_precip,
                                          ncol = 2)))# Sets number of panel columns
                              
# ggsave(panel_model_pred, file = "output/figures/panel_model_pred.png", width = 20, height = 10)


# MODEL 5: biomass ~ temp*precip ----

# To display interaction: categorise precipitation into'low', 'medium', 'high': 
# Plot 3 lines in plot with temp on the x and biomass on y and points coloured by precipitation level
range(coord.chelsa.combo.c$CH_PrecipMeanSummer)

# scaling explanatory variables
coord.chelsa.combo.c$CH_TempMeanSummer <- scale(coord.chelsa.combo.c$CH_TempMeanSummer, center = TRUE, scale = TRUE)
coord.chelsa.combo.c$CH_PrecipMeanSummer <- scale(coord.chelsa.combo.c$CH_PrecipMeanSummer, center = TRUE, scale = TRUE)

# quantiles
quantile(coord.chelsa.combo.c$CH_PrecipMeanSummer)
#0%         25%         50%         75%        100% 
#-2.21492909 -0.70752311 -0.03756489  0.54864855  4.14967397 
mean(coord.chelsa.combo.c$CH_PrecipMeanSummer)
# 86.44856 (not standardised)

# Categorising into precipitation levels using quantiles
coord.chelsa.combo.d <- coord.chelsa.combo.c %>% 
  mutate(precip_level = case_when(CH_PrecipMeanSummer < -0.71467025~ "Low",
                              CH_PrecipMeanSummer >= -0.71467025 & CH_PrecipMeanSummer < 0.56418805  ~ "Medium",
                              CH_PrecipMeanSummer >= 0.56418805  ~ "High"))

unique(coord.chelsa.combo.d$precip_level) # 3 precip levels

# Making precipitation level a factor
coord.chelsa.combo.d$precip_level <- as.factor(as.character(coord.chelsa.combo.d$precip_level)) # moisture as factor
str(coord.chelsa.combo.d)

# saving dataframe
write.csv(coord.chelsa.combo.d, file = "datasets/climate_data/coord.chelsa.combo.d.csv")

# Model 5a: biomass Vs temp*precipitation level
model_5a <- lm(biomass ~ CH_TempMeanSummer*precip_level, data = coord.chelsa.combo.d)
summary(model_5a)
# F-statistic: 174.6 on 5 and 3186 DF,  p-value: < 2.2e-16

# model 5a output table 
stargazer(model_5a, type = "html",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "", out = "output/tables/interaction.htm")

# Extract predictions
predictions_5 <- as.data.frame(predict(model_5a, newdata = coord.chelsa.combo.d, interval = "confidence")) # this gives overall predictions for the model
model_5_preds <- cbind(coord.chelsa.combo.d, predictions_5)

# reordeing factor levels 
model_5_preds$precip_level <- factor(model_5_preds$precip_level,levels=c("Low", "Medium", "High"),
                                            labels = c("Low", "Medium", "High"),
                                            ordered = T)
# Plot the predictions 
(predictions_interaction<- (ggplot(model_5_preds, aes(CH_TempMeanSummer, fit, group = precip_level)) + 
                                   geom_point(aes(x = CH_TempMeanSummer, y = biomass, colour = precip_level), size =0.5) +
                              scale_colour_manual(values = c("#DC9902", "#46AAE2", "#003654"), name = "Precipitation level")+
                                   stat_smooth(method=lm, aes(colour = precip_level), size=1.5)+
                                  geom_line(aes(y=lwr,  color = precip_level), linetype = "dashed", size=0.5)+
                                   geom_line(aes(y=upr, color = precip_level), linetype = "dashed",  size=0.5)+
                              xlab("\nScaled mean summer temperature (°C)") +
                              ylab(bquote("Shrub biomass "*(g~m^-2)*"")) + 
                              #annotate(geom = "text", x = 2, y = 1000, label="(c)", size = 15) +
                                   theme_shrub()+ theme(legend.text = element_text(size= 20),
                                                        legend.title = element_text(size=25), 
                                                        legend.position = "right")))

 ggsave(filename = "output/figures/predictions_interaction.png")

# adding rain logo
(predictions_interaction <- predictions_interaction+ annotation_raster(raster_rain_logo, -3.8, -2, 750, 1000))
(predictions_interaction <- predictions_interaction+ annotation_raster(raster_temp_logo, -3.2, -1, 750, 1000))

# ggsave(filename = "output/figures/predictions_interaction.png")

# saving model outputs
tab_model(model_5a, file = "output/tables/model_5a.html")
webshot("output/tables/model_5a.html", "output/tables/model_5a.png")
 
# ASSUMPTION CHECK -----

coord.chelsa.combo.e <- coord.chelsa.combo.c %>%
  dplyr::select(CH_PrecipMeanSummer,CH_TempMeanSummer, longitude, latitude, biomass )

# making correlation heat map using Spearman correlation
# only keeping significant relationships 
corrplot(cor(coord.chelsa.combo.e, method="s"), sig.level = 0.05, insig = "blank") 
rcorr(as.matrix(coord.chelsa.combo.e))

############################################# END -----
