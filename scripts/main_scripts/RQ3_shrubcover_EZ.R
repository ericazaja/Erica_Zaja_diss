##%######################################################%##
#                                                          #
####         RQ3: SHRUB COVER in ANWR -----               ##
#               Erica Zaja - 05/02/2022                   ##
#                                                         ##
##%######################################################%##

# RQ3: How has vegetation cover changed in the Arctic National Wildlife Refuge between 1996-2007? 
# colour palette by Wong 
# https://davidmathlogic.com/colorblind/#%23000000-%23E69F00-%2356B4E9-%23009E73-%23F7EA40-%230072B2-%23D55E00-%23CC79A7 

# LOADING LIBRARIES  ----
library(tidyverse)
library(cowplot)
library(brms)
library(ggpubr)
library(viridis)
library(ggtern)
library(lme4)
library(ggeffects)
library(sjPlot)  # to visualise model outputs
library(stargazer)
library(blmeco)
library(MuMIn)
library(sjmisc)
library(MASS)
library(sjmisc)
library(sjlabelled)


# LOADING DATA ----

load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/ITEX_EZ_diss.RData")
ANWR_veg <- read_csv("datasets/ITEX_data/ANWR_veg.csv")

# DATA WRANGLING ----

# Filtering shrub only data
ITEX_shrubs <-  ANWR_veg %>% filter (FuncGroup == "Shrub") 
unique(ITEX_shrubs$GENUS) # Unique genus names 
# [1] "Dryas"          "Salix"          "Vaccinium"      "Arctostaphylos" "Betula"         "Cassiope"       "Ledum"         
str(ITEX_shrubs)

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



# MODELLING -----

# 1. SHRUB COVER CHANGE ------

# Mean shrub cover per plot per year (right method - checked with Mariana)
ITEX_shrubs_mean <- ITEX_shrubs %>%
  group_by(SiteSubsitePlotYear) %>%
  mutate(mean_cover = mean(RelCover)) %>%
  ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ITEX_shrubs_mean_trim <- ITEX_shrubs_mean %>% 
  dplyr::select(PLOT, YEAR, LAT, LONG, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE)%>% 
  mutate(mean_cover_prop = mean_cover/100) %>%    # making into proportion data
  mutate(mean_cover_int = ceiling(mean_cover)) %>%   
  mutate(year_index = case_when (YEAR == 1996 ~ '1', YEAR == 1997 ~ '2', 
                                 YEAR == 1998 ~ '3', YEAR == 1999 ~ '4',
                                 YEAR == 2000 ~ '5', YEAR== 2001 ~ '6', 
                                 YEAR == 2002 ~ '7', YEAR == 2003 ~ '8',
                                 YEAR== 2004 ~ '9', YEAR == 2005 ~ '10',
                                 YEAR== 2006 ~ '11', YEAR == 2007 ~ '12')) 
ITEX_shrubs_mean_trim$year_index <- as.numeric(ITEX_shrubs_mean_trim$year_index)
ITEX_shrubs_mean_trim$YEAR <- as.factor(ITEX_shrubs_mean_trim$YEAR)

str(ITEX_shrubs_mean_trim)

# making plot categorical
ITEX_shrubs_mean_trim$PLOT <- as.factor(as.character(ITEX_shrubs_mean_trim$PLOT))
hist(ITEX_shrubs_mean_trim$mean_cover_prop) # right skewed

# Mean shrub cover change over time scatter plot
(shrub_mean_change <- (ggplot(ITEX_shrubs_mean_trim)+
                     geom_point(aes(x = YEAR, y = mean_cover_prop), colour = "green4", size = 1) +
                     geom_smooth(aes(x = YEAR, y = mean_cover_prop), colour= "black", method = "glm") + 
                     scale_x_continuous(breaks=c(1996, 1999, 2002,2005, 2007))+
                     labs(y = "Mean shrub % cover\n", x = "\nYear") + 
                  # annotate(geom = "text", x = 2007, y = 50, label="(a)", size = 10) +
                     theme_shrub()+
                     theme(axis.text.x = element_text(angle = 45))))

# ggsave(shrub_mean_change, file = "output/figures/shrub_mean_change.png")          

# Model 12 ----
# Shrub cover over time
unique(ITEX_shrubs_mean_trim$YEAR)

# Transform percentage cover to proportion (dividing by 100)
ITEX_shrubs_mean_trim <- ITEX_shrubs_mean_trim %>% mutate(cover_prop = mean_cover/100)
hist(ITEX_shrubs_mean_trim$mean_cover)


# glmer.nb, with plot and year as random effects
model_6b <- glmer.nb(mean_cover_int~ year_index  + (1|PLOT) + (1|YEAR), data = ITEX_shrubs_mean_trim)
summary(model_6b)
dispersion_glmer(model_6b)# 0.9659739

# glmer poisson,  with plot and year as random effects
model_6c <- glmer(mean_cover_prop ~ I(YEAR-1995) + (1|PLOT) + (1|YEAR), family = poisson, data = ITEX_shrubs_mean_trim)
summary(model_6c)
dispersion_glmer(model_6c)#0.1816644

# null model 
model_6_null <- glm.nb(mean_cover_prop ~1,  data = ITEX_shrubs_mean_trim)
# comparing AIC
AIC(model_6_null, model_6,model_6a, model_6b, model_6c)

# Checking model assumptions 
plot(model_6b)
qqnorm(resid(model_6b))
qqline(resid(model_6b))  # points fall nicely onto the line - good!

# Output table model 6 
stargazer(model_6, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
pred_model_6 <- ggpredict(model_6b, terms = c("year_index"))
# this gives overall predictions for the model
pred_model_6a <- ggpredict(model_6b, terms = c("YEAR"))
g <-ggeffect(model_6b)
random_effect_terms <- insight::find_random(model_6b, split_nested = TRUE, flatten = TRUE)
ggeffect(model_6b, "year_index",ci.lvl = 0.95)

install.packages("ggeffects")
# write.csv(pred_model_6, file = "datasets/pred_model_6.csv")

# Plot the predictions 
(shrub_cover_ANWR <- (ggplot(pred_model_6) + 
                        geom_line(aes(x = x, y = predicted)) +          # slope
                        # geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                        # fill = "lightgrey", alpha = 0.5) +  # error band
                        geom_point(data = ITEX_shrubs_mean_trim,                      # adding the raw data 
                                   aes(x = YEAR, y = mean_cover, colour = PLOT), size = 0.5) + 
                        labs(x = "\nYear", y = "Shrub cover (%)\n", 
                             title = "Shrub % cover increase in the ANWR\n") + 
                        theme_shrub()
))


ggsave(file = "output/figures/shrub_cover_ANWR.png")

### 2. SHRUB GENUS -----

# different shrub species
unique(ITEX_shrubs$GENUS) 
# [1] "Dryas"          "Salix"          "Vaccinium"      "Arctostaphylos" "Betula"         "Cassiope"      
# [7] "Ledum"

# Mean shrub genus cover per plot per year
ITEX_shrub_sp <- ITEX_shrubs %>%
  group_by(SiteSubsitePlotYear, GENUS) %>%
  mutate(genus_cover = sum(RelCover)) %>%
  ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ITEX_shrubs_sp_trim <- ITEX_shrub_sp  %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, GENUS, genus_cover) %>% 
  distinct(SiteSubsitePlotYear, genus_cover, .keep_all = TRUE)  %>% 
  mutate(genus_cover_prop = genus_cover/100)

# making genus a factor
ITEX_shrubs_sp_trim$GENUS <- as.factor(as.character(ITEX_shrubs_sp_trim$GENUS ))
# making year a factor
ITEX_shrubs_sp_trim$YEAR <- as.numeric(ITEX_shrubs_sp_trim$YEAR)

hist(ITEX_shrubs_sp_trim$genus_cover) # right skewed
str(ITEX_shrubs_sp_trim)

# scatter plot of different shrub genus change over time
(facet_scatter_shrub_genus <- (ggplot(ITEX_shrubs_sp_trim, aes(x = YEAR, y = genus_cover))+
                                 geom_point(size = 0.6, aes(colour=GENUS)) +
                                 scale_colour_manual(values = c("#DC9902", "#000000", "#46AAE2", "#003654", "#D55E00", "#009E73","#CC79A7", "#000000"))+
                                 geom_smooth(method = lm, aes(colour= GENUS, fill = GENUS), show.legend = FALSE))+
    scale_fill_manual(values = c("#DC9902", "#000000", "#46AAE2", "#003654", "#D55E00", "#009E73","#CC79A7", "#000000"))+
    #scale_fill_manual(values = c("green4", "green3", "red", "red4", "brown", "blue4", 'blue3' ))+ 
                                 facet_wrap(~ GENUS, scales = "free_y") +
                                 scale_x_continuous(breaks=c(1996, 1999, 2002,2005, 2007))+
                                 labs(y = "Mean cover (%) \n", x = "\nYear") +
                                 theme_shrub()+
                                 theme(axis.text.x  = element_text(vjust=0.5, size=20, angle= 45, 
                                                                   colour = "black"), 
                                       legend.position = "none",
                                       axis.title.x = element_text(size=25),
                                       axis.title.y = element_text(size=25),
                                       strip.text.x = element_text(size = 25, face = "italic" )))

dev.off()
# ggsave(file = "output/figures/facet_scatter_shrub_genus.png")

# MODEL(s) 13 ----
lmer_shrub_sp_2a <- glmer.nb(genus_cover_prop~I(YEAR-1995) + (1|GENUS) + (1|PLOT), data = ITEX_shrubs_sp_trim)
summary(lmer_shrub_sp_2)
dispersion_glmer(lmer_shrub_sp_2) # 0.2425216
r.squaredGLMM(lmer_shrub_sp_2) 
AIC(lmer_shrub_sp_null, lmer_shrub_sp_2)

# Comparing different models:
lmer_shrub_sp_2 <- glmer.nb(genus_cover_prop~I(YEAR-1995) + GENUS + (1|YEAR) + (1|PLOT), data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_3 <- glmer.nb(genus_cover_prop~I(YEAR-1995) + (1|GENUS) + (1|YEAR) + (1|PLOT), data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_0 <- glmer.nb(genus_cover_prop~I(YEAR-1995) + (1|YEAR) + (1|PLOT), data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_1 <- glmer.nb(genus_cover_prop~I(YEAR-1995)+ (1|GENUS) , data = ITEX_shrubs_sp_trim)
lmer_shrub_sp <- glmer.nb(genus_cover_prop~I(YEAR-1995)*GENUS + (1|YEAR) + (1|PLOT), data = ITEX_shrubs_sp_trim)
dispersion_glmer(lmer_shrub_sp_2) #0.2723795
dispersion_glmer(lmer_shrub_sp_0)# 0.2750381
r.squaredGLMM(lmer_shrub_sp_2)
r.squaredGLMM(lmer_shrub_sp_0)
summary(lmer_shrub_sp_0) 

# Output tables
tab_model(lmer_shrub_sp_3, file = "output/tables/lmer_shrub_sp_3.html")
webshot("output/tables/lmer_shrub_sp_3.html", "output/tables/lmer_shrub_sp_3.png")

tab_model(lmer_shrub_sp_0, file = "output/tables/lmer_shrub_sp_0.html")
webshot("output/tables/lmer_shrub_sp_0.html", "output/tables/lmer_shrub_sp_0.png")

# Output table 
stargazer(lmer_shrub_sp_2,
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "", 
          type = "html", out = "output/tables/lmer_shrub_sp.html")


# Trying model with random intercepts/slopes
lmer_shrub_sp_rand <- glmer.nb(genus_cover~YEAR + (1+YEAR|GENUS) + (1|YEAR), data = ITEX_shrubs_sp_trim) 
# NB. doesn't converge with glmer.nb and with glmer we have overdispersion

# null model
lmer_shrub_sp_null <- glm.nb(genus_cover_prop~1, data = ITEX_shrubs_sp_trim)

# comparing AIC values
AIC(lmer_shrub_sp_null, lmer_shrub_sp_2, lmer_shrub_sp_3, lmer_shrub_sp_0, lmer_shrub_sp)

# Plotting fixed effects
(fe.effects <- plot_model(lmer_shrub_sp_2, show.values = TRUE))

# Plotting random effects
(re.effects <- plot_model(lmer_shrub_sp , type = "re", show.values = TRUE))


#model predicions
p <- ggpredict(lmer_shrub_sp_2, c("YEAR", "GENUS"))
plot(p) 

# 3. SHRUB COVER IN SPACE  ----
# standardise lat and long
ITEX_shrubs_mean_trim$LAT <- scale(ITEX_shrubs_mean_trim$LAT , center = TRUE, scale = TRUE)
ITEX_shrubs_mean_trim$LONG <- scale(ITEX_shrubs_mean_trim$LONG , center = TRUE, scale = TRUE)
hist(ITEX_shrubs_mean_trim$mean_cover) # distribution
glimpse(ITEX_shrubs_mean_trim$mean_cover_prop)

# Shrub cover vs latitude 
shrub_lat <- glm.nb(mean_cover ~ LAT, data = ITEX_shrubs_mean_trim)
summary(shrub_lat)
# Null deviance: 230.24 
# Residual deviance:  136.82 
# Pseudo R2 = 1-(136.82 /230.24)
# [1] 0.4057505 latitude explains ~40 % in variation in shrub cover
plot(shrub_lat)

# null model
shrub_lat_null <- glm.nb(mean_cover ~ 1, data = ITEX_shrubs_mean_trim)
AIC(shrub_lat, shrub_lat_null)

# Output table
stargazer(shrub_lat, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Plotting scatter
(cover_lat_scatter <- (ggplot(ITEX_shrubs_mean_trim))+
    geom_point(aes(x = LAT, y = mean_cover), colour= "#009E73", size = 1) +
    geom_smooth(aes(x = LAT, y = mean_cover),colour = "black", method = "glm", fill="#009E73", size = 2) + 
    labs(y = "Mean shrub % cover\n", x = "\nScaled latitude") +
   # annotate(geom = "text", x = 1, y = 60, label="(a)", size = 10) +
    annotate(geom = "text", x = 0.5, y = 20, label="slope = -0.447*** ", size = 10) +
     theme_shrub())

# ggsave(file = "output/figures/cover_lat_scatter.png")

# adding icon
lat_logo <- readPNG("lat_icon.png")
raster_lat_logo <- as.raster(lat_logo)
(cover_lat_scatter <- cover_lat_scatter + annotation_raster(raster_lat_logo, 0.5, 1, 30, 60))
ggsave(file = "output/figures/cover_lat_scatter.png")

# Extracting model predictions 
predictions_10 <- as.data.frame(predict(shrub_lat, newdata = ITEX_shrubs_mean_trim, interval = "confidence")) # this gives overall predictions for the model
model_10_preds <- cbind(ITEX_shrubs_mean_trim, predictions_10)

# write.csv(pred_model_10, file = "datasets/pred_model_10.csv")

# Plot the predictions 
(plot_model_shrub_lat <- (ggplot(model_10_preds, aes(LAT, fit)) +
                            geom_point(aes(x = LAT, y = mean_cover)         # adding the raw data 
                                       ,colour = "green4", size = 1) + 
                            stat_smooth(method=lm, colour = "black")+
                            geom_line(aes(y=lwr,  color = "grey"), linetype = "dashed")+
                            geom_line(aes(y=upr, color = "grey"), linetype = "dashed")+
                            annotate(geom = "text", x = 1, y = 60, label="(a)", size = 10) +
                            annotate(geom = "text", x = 0.5, y = 20, label="slope = -4.419*** ", size = 6) +
                            labs(x = "\nLatitude", y = "Shrub cover (%)\n", 
                                 title = "") + 
                            # scale_x_continuous(scale_x_continuous(breaks = 1996:2007))+ 
                            theme_shrub() + theme(legend.position = "none")))

ggsave(file = "output/figures/plot_model_shrub_lat.png")

# Shrub cover vs longitude
shrub_long <- glm.nb(mean_cover ~ LONG, data = ITEX_shrubs_mean_trim)
summary(shrub_long)

plot(shrub_long)

# Output table
stargazer(shrub_long, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
# F-statistic: 55.18 on 1 and 143 DF,  p-value: 9.123e-12***

# scatter
(cover_long_scatter <- (ggplot(ITEX_shrubs_mean_trim))+
    geom_point(aes(x = LONG, y = mean_cover), colour = "skyblue", size = 2) +
    geom_smooth(aes(x = LONG, y = mean_cover), colour = "black", method = "lm") + 
    labs(y = "Mean shrub % cover\n", x = "\nLongitude") +
    annotate(geom = "text", x = 1, y = 60, label="(b)", size = 10) +
    annotate(geom = "text", x = 0.5, y = 20, label="slope = -4.419*** ", size = 6) +
    theme_shrub())


# ggsave(file = "output/figures/cover_long_scatter.png")

# Extracting model predictions 
predictions_11 <- as.data.frame(predict(shrub_long, newdata = ITEX_shrubs_mean_trim, interval = "confidence")) # this gives overall predictions for the model
model_11_preds <- cbind(ITEX_shrubs_mean_trim, predictions_11)

# write.csv(pred_model_10, file = "datasets/pred_model_10.csv")

# Plot the predictions 
(plot_model_shrub_long <- (ggplot(model_11_preds, aes(LONG, fit)) +
                            geom_point(aes(x = LONG, y = mean_cover)         # adding the raw data 
                                       ,colour = "green4", size = 1) + 
                            stat_smooth(method=lm, colour = "black")+
                            geom_line(aes(y=lwr,  color = "grey"), linetype = "dashed")+
                            geom_line(aes(y=upr, color = "grey"), linetype = "dashed")+
                             annotate(geom = "text", x = 1, y = 60, label="(b)", size = 10) +
                             annotate(geom = "text", x = 0.5, y = 20, label="slope = -4.419*** ", size = 6) +
                            labs(x = "\nLongitude", y = "Shrub cover (%)\n", 
                                 title = "") + 
                            # scale_x_continuous(scale_x_continuous(breaks = 1996:2007))+ 
                            theme_shrub()+ theme(legend.position = "none")))

# ggsave(file = "output/figures/plot_model_shrub_long.png")


# Panel ----
(panel_cover_latlong_pred <- grid.arrange(arrangeGrob(plot_model_shrub_lat,plot_model_shrub_long,
                                                 ncol = 2)) )# Sets number of panel columns

ggsave(panel_cover_latlong_pred, file = "output/figures/panel_cover_latlong_pred.png", height = 10, width = 20)


############################################################### END -----           

