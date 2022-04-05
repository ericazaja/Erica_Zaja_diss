##%######################################################%##
#                                                          #
####         RQ2: SHRUB BIOMASS VS CLIMATE              #### 
#               Erica Zaja - 22/10/2021                   ##
#                                                         #
##%######################################################%##

## DISCLAIMER: not used for final product

# EXTRA MODELS ----
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

# EXTRA ASSUMPTIONS CHECK ----
# standardise lat and long
coord.chelsa.combo.c$latitude <-scale(coord.chelsa.combo.c$latitude, center = TRUE, scale = TRUE)
coord.chelsa.combo.c$longitude<-scale(coord.chelsa.combo.c$longitude, center = TRUE, scale = TRUE)

# a. Checking temp ~ lat -----
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

# b. Checking temp ~ long -----
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

# c. Checking precip ~ lat -----
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

# d. Checking precip ~ long -----
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

# Panel 
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





