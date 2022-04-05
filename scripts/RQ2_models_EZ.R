##%######################################################%##
#                                                          #
####         RQ2: SHRUB BIOMASS VS CLIMATE              #### 
#               Erica Zaja - 22/10/2021                   ##
#                                                         #
##%######################################################%##

# RQ2: how does biomass vary with temperature and precipitation?
# PART 2: MODELLING ----

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
coord.chelsa.combo.c <- read_csv("datasets/climate_data/coord_chelsa_combo_new.csv")
range(coord.chelsa.combo.c$CH_TempMeanSummer)
range(coord.chelsa.combo.c$CH_PrecipMeanSummer)
glimpse(coord.chelsa.combo.c)
str(coord.chelsa.combo.c)
unique(coord.chelsa.combo.c$gridcell)
# making grid cell into a factor
coord.chelsa.combo.c$gridcell <- as.factor(as.character(coord.chelsa.combo.c$gridcell))

# MODELLING ----

# Model 3 ----
# Standardising explanatory variables
coord.chelsa.combo.c$CH_TempMeanSummer <-scale(coord.chelsa.combo.c$CH_TempMeanSummer, center = TRUE, scale = TRUE)
coord.chelsa.combo.c$CH_PrecipMeanSummer <-scale(coord.chelsa.combo.c$CH_PrecipMeanSummer, center = TRUE, scale = TRUE)

# biomass ~ temp 
model_3 <- lm(biomass ~ CH_TempMeanSummer, data = coord.chelsa.combo.c)
summary(model_3)
# F-statistic: 355.5 on 1 and 3190 DF,  p-value: < 2.2e-16
# slope =  38.033  ***

model_3_null<- lm(biomass ~1, data = coord.chelsa.combo.c)
AIC(model_3, model_3_null)


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
predictions_3 <- as.data.frame(predict(model_3, newdata = coord.chelsa.combo.c, interval = "confidence")) # this gives overall predictions for the model
model_3_preds <- cbind(coord.chelsa.combo.c, predictions_3)
cols <- colors_wong

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



ggsave(file = "output/figures/predictions_biomass_vs_temp.png")

# adding temp logo
temp_logo <- readPNG("temp_logo.png")
raster_temp_logo <- as.raster(temp_logo)
(predictions_biomass_vs_temp <- predictions_biomass_vs_temp + annotation_raster(raster_temp_logo, -4.5, -2.3, 900, 1150))
ggsave(file = "output/figures/predictions_biomass_vs_temp.png")



# Quick scatter: biomass ~ temp
(scatter_temp <- ggplot(coord.chelsa.combo.c, aes(x = CH_TempMeanSummer, y = biomass)) +
    geom_point(color="skyblue", size = 0.1) +
    geom_smooth(method = "lm", color = "black") +
    annotate(geom = "text", x = 2.5, y = 1200, label="(a)", size = 10) +
    annotate(geom = "text", x = 0, y = 800, label="slope = 36.433*** ", size = 6) +
    labs(x = "\nMean summer temperature (°C)", y = "Shrub biomass (g/m2)\n") + 
         # title = "Shrub biomass increases with temperature\n") + 
    theme_shrub())

ggsave(file = "output/figures/scatter_temp.png")

# Model 4  -----
# biomass ~ precip 
model_4 <- lm(biomass ~ CH_PrecipMeanSummer, data = coord.chelsa.combo.c)
summary(model_4)
# F-statistic:  1746 on 1 and 9573 DF,  p-value: < 2.2e-16
# slope = 47.290***

AIC(model_4, model_3_null)

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


                                     
ggsave(file = "output/figures/predictions_biomass_vs_precip.png")

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

ggsave(file = "output/figures/scatter_precip.png")

# Panel  -----
# panel of scatters 
panel_title <- text_grob("Shrub biomass increases with mean summer temperature and precipitation",
                         size = 18, face = "bold")

(panel_scatter <- grid.arrange(arrangeGrob(scatter_temp, scatter_precip,
                                 ncol = 2)) )# Sets number of panel columns
                            # top = panel_title  # Adding panel title
                          
ggsave(panel_scatter, file = "output/figures/panel_scatter.png", width = 18, height = 9)

# Panel of model predictions
(panel_model_pred<- grid.arrange(arrangeGrob(predictions_biomass_vs_temp, predictions_biomass_vs_precip,
                                          ncol = 2)))# Sets number of panel columns
                              
ggsave(panel_model_pred, file = "output/figures/panel_model_pred.png", width = 20, height = 10)


# Model 5 ----
# biomass ~ temp*precip

# To display interaction: categorise precipitation 'dry', 'moist', 'wet': 
# Plot 3 lines in plot with temp on the x and biomass on y and points coloured by moisture level
range(coord.chelsa.combo.c$CH_PrecipMeanSummer)
coord.chelsa.combo.c$CH_TempMeanSummer <-scale(coord.chelsa.combo.c$CH_TempMeanSummer, center = TRUE, scale = TRUE)
coord.chelsa.combo.c$CH_PrecipMeanSummer <-scale(coord.chelsa.combo.c$CH_PrecipMeanSummer, center = TRUE, scale = TRUE)

quantile(coord.chelsa.combo.c$CH_PrecipMeanSummer)
#0%         25%         50%         75%        100% 
#-2.21492909 -0.70752311 -0.03756489  0.54864855  4.14967397 
mean(coord.chelsa.combo.c$CH_PrecipMeanSummer)
# 86.44856

coord.chelsa.combo.d <- coord.chelsa.combo.c %>% 
  mutate(precip_level = case_when(CH_PrecipMeanSummer < -0.71467025~ "Low",
                              CH_PrecipMeanSummer >= -0.71467025 & CH_PrecipMeanSummer < 0.56418805  ~ "Medium",
                              CH_PrecipMeanSummer >= 0.56418805  ~ "High"))



unique(coord.chelsa.combo.d$precip_level)
coord.chelsa.combo.d$precip_level <- as.factor(as.character(coord.chelsa.combo.d$precip_level)) # moisture as factor
str(coord.chelsa.combo.d)

write.csv(coord.chelsa.combo.d, file = "datasets/climate_data/coord.chelsa.combo.d.csv")

# Model 5a: biomass Vs temp*moisture
model_5a <- lm(biomass ~ CH_TempMeanSummer*precip_level, data = coord.chelsa.combo.d)
summary(model_5a)
# F-statistic: 174.6 on 5 and 3186 DF,  p-value: < 2.2e-16

# reordeing factor levels 
coord.chelsa.combo.d$precip_level <- factor(coord.chelsa.combo.d$precip_level,levels=c("Low", "Medium", "High"),
                                         labels = c("Low", "Medium", "High"),
                                         ordered = T)

model_5a_null <- lm(biomass ~ 1, data = coord.chelsa.combo.d)
AIC(model_3, model_4, model_5a, model_5a_null)

# model 5a output table 
stargazer(model_5a, type = "html",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "", out = "output/tables/interaction.htm")

# Extract predictions
predictions_5 <- as.data.frame(predict(model_5a, newdata = coord.chelsa.combo.d, interval = "confidence")) # this gives overall predictions for the model
model_5_preds <- cbind(coord.chelsa.combo.d, predictions_5)


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
                                                        legend.position = "none")))

ggsave(filename = "output/figures/predictions_interaction.png")

# adding rain logo
(predictions_interaction <- predictions_interaction+ annotation_raster(raster_rain_logo, -3.8, -2, 750, 1000))
(predictions_interaction <- predictions_interaction+ annotation_raster(raster_temp_logo, -3.2, -1, 750, 1000))

ggsave(filename = "output/figures/predictions_interaction.png")




# this BELOW is to plot scatter with interaction: Need NOT standardised climate variables
# Extracting model predictions 
str(coord.chelsa.combo.d)
pred_model_5a <- ggpredict(model_5a, terms = c("CH_TempMeanSummer", "moisture"))  # this gives overall predictions for the model
#write.csv(pred_model_5a, file = "datasets/pred_model_5a.csv")

plot_model(model_5a, type = "pred", terms = c("CH_TempMeanSummer", "moisture"))

coord.chelsa.combo.d$moisture <- factor(coord.chelsa.combo.d$moisture,levels=c("dry", "moist", "wet"),
                                         labels = c("dry", "moist", "wet"),
                                         ordered = T)


# Plot the predictions 
(biomass_vs_moist_temp<- (ggplot(predictions_5) + 
                         geom_line(aes(x = fit, y = predicted, group = group, colour = group)) +          # slope
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

# ASSUMPTION CHECK -----
coord.chelsa.combo.e <- coord.chelsa.combo.c %>%
  dplyr::select(CH_PrecipMeanSummer,CH_TempMeanSummer, longitude, latitude, biomass )

# correlation heat map
# only keeping significant relationships 
corrplot(cor(coord.chelsa.combo.e, method="s"), sig.level = 0.05, insig = "blank") 
rcorr(as.matrix(coord.chelsa.combo.e))

# standardise lat and long
coord.chelsa.combo.c$latitude <-scale(coord.chelsa.combo.c$latitude, center = TRUE, scale = TRUE)
coord.chelsa.combo.c$longitude<-scale(coord.chelsa.combo.c$longitude, center = TRUE, scale = TRUE)

# Checking temp ~ lat -----
temp_lat_model <- lm(CH_TempMeanSummer ~ latitude, data=coord.chelsa.combo.c)
summary(temp_lat_model) 
#F-statistic:  1146 on 1 and 9573 DF,  p-value: < 2.2e-16***
# temp decreases with latitude. I.e. northern latitudes colder
# -2.03669  ***

(scatter_lat_temp <- ggplot(coord.chelsa.combo.c, aes(x =latitude , y = CH_TempMeanSummer )) +
   geom_point(size = 0.3, color="skyblue") +
   geom_smooth(method = "lm", colour = "black") +
    labs(x = "\nLatitude", y = "Mean summer temperature (°C)\n")+ 
    annotate(geom = "text", x = 2, y = 2.5, label="(a)", size = 10) +
   annotate(geom = "text", x = 0, y = -2.5, label="slope = -3.270e-01*** ", size = 6) +
   theme_shrub()+ theme(axis.title.y =element_text(size=12), 
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
    annotate(geom = "text", x = 2, y = 2.5, label="(b)", size = 10) +
    annotate(geom = "text", x = 1, y = -2.5, label="slope = 0.061870*** ", size = 6) +
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
    annotate(geom = "text", x = 2, y = 4, label="(c)", size = 10) +
    annotate(geom = "text", x = 1, y = 2, label="slope = -65.5282*** ", size = 6) +
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
    annotate(geom = "text", x = 2, y = 4, label="(d)", size = 10) +
    annotate(geom = "text", x = 1, y = 2, label="slope = 1.34762*** ", size = 6) +
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

hist(coord.chelsa.combo.c$biomass)

## MODELS with ALL -----
model_all <- lm(biomass ~ CH_PrecipMeanSummer + CH_TempMeanSummer + latitude + longitude, data = coord.chelsa.combo.c)
summary(model_all) # Adjusted R-squared:  0.2819 

model_all_2 <- lm(biomass ~ CH_PrecipMeanSummer*CH_TempMeanSummer + latitude*longitude, data = coord.chelsa.combo.c)
summary(model_all_2) # Adjusted R-squared:  0.3275 

model_all_3 <- lm(biomass ~ CH_TempMeanSummer + latitude, data = coord.chelsa.combo.c)
summary(model_all_3) # Adjusted R-squared:  0.2027 

model_all_4 <- lm(biomass ~ CH_PrecipMeanSummer*CH_TempMeanSummer + latitude, data = coord.chelsa.combo.c)
summary(model_all_4) #Adjusted R-squared:  0.2226

model_all_5 <- lm(biomass ~ CH_PrecipMeanSummer*CH_TempMeanSummer + longitude, data = coord.chelsa.combo.c)
summary(model_all_5) # Adjusted R-squared:  0.3028 

model_all_6<- lm(biomass ~ CH_TempMeanSummer + latitude*longitude, data = coord.chelsa.combo.c)
summary(model_all_6) # Adjusted R-squared:  0.2806 

model_all_7<- lm(biomass ~ CH_PrecipMeanSummer + latitude*longitude, data = coord.chelsa.combo.c)
summary(model_all_7) #Adjusted R-squared:  0.2555 

model_all_8<- lm(biomass ~ CH_PrecipMeanSummer + latitude, data = coord.chelsa.combo.c)
summary(model_all_8) #Adjusted R-squared:  0.1759 

model_all_9<- lm(biomass ~ CH_PrecipMeanSummer + longitude, data = coord.chelsa.combo.c)
summary(model_all_9) #Adjusted R-squared:  0.202 

model_all_10<- lm(biomass ~ CH_TempMeanSummer + longitude, data = coord.chelsa.combo.c)
summary(model_all_10) #Adjusted R-squared:  0.1343 

model_all_11<- lm(biomass ~ CH_TempMeanSummer*longitude, data = coord.chelsa.combo.c)
summary(model_all_11) # Adjusted R-squared:  0.1344 

model_all_12<- lm(biomass ~ CH_TempMeanSummer*latitude, data = coord.chelsa.combo.c)
summary(model_all_12) # Adjusted R-squared:  0.2107 

model_all_13<- lm(biomass ~ CH_PrecipMeanSummer*longitude, data = coord.chelsa.combo.c)
summary(model_all_13) # Adjusted R-squared:  0.2223 

model_all_14<- lm(biomass ~ CH_PrecipMeanSummer*latitude, data = coord.chelsa.combo.c)
summary(model_all_14) # Adjusted R-squared:  0.1817 





# END -----
