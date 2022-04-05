##%######################################################%##
#                                                          #
###              RQ1: SPATIAL ANALYSIS                   ###
#               Erica Zaja - 04/02/2022                    #
#                                                          #
##%######################################################%##


# Extra model:  biomass vs long*lat  (not used in final diss)
model_2a <- lm(biomass~longitude*latitude, data = r3_rsample_002)
summary(model_2a)
# F-statistic: 353.7 on 3 and 3188 DF,  p-value: < 2.2e-16***
# checking correlation betwee lat and long
cor.test( r3_rsample_002$latitude,r3_rsample_002$longitude, method = "pearson")
# t = -11.449, df = 3190, p-value < 2.2e-16
# lat and long are correlated 
# Does not mean much: as latitude increases longitude decreases

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



