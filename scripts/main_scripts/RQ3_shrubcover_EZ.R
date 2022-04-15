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
unique(ITEX_shrubs$YEAR) # Unique genus names 
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
  mutate(sum_cover = sum(RelCover)) %>%
  ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ITEX_shrubs_mean_trim <- ITEX_shrubs_mean %>% 
  dplyr::select(PLOT, YEAR, LAT, LONG, SUBSITE, SiteSubsitePlotYear, SiteSubsitePlot, sum_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, sum_cover, .keep_all = TRUE)%>% 
  mutate(sum_cover_prop = sum_cover/100) %>%    # making into proportion data
  mutate(sum_cover_int = floor(sum_cover)) %>%   
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
hist(ITEX_shrubs_mean_trim$sum_cover_int) # right skewed
glimpse(ITEX_shrubs_mean_trim)

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

# write.csv(pred_model_6, file = "datasets/pred_model_6.csv")

# Plot the predictions 
(shrub_preds <- ggplot(pred_model_6, aes(x = x, y = predicted)) +
    stat_smooth(method = "lm")  +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
    geom_point(data = ITEX_shrubs_mean_trim,                      # adding the raw data (scaled values)
               aes(x = year_index, y = mean_cover_int))+
    labs(x = "\nYear (indexed)", y = "Mean shrub cover (%)\n")+
    theme_shrub()) 


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
  dplyr::select(PLOT, YEAR, SUBSITE, SiteSubsitePlotYear, SiteSubsitePlot, GENUS, genus_cover) %>% 
  distinct(SiteSubsitePlotYear, genus_cover, .keep_all = TRUE)  %>% 
  mutate(genus_cover_prop = genus_cover/100)  %>%
  mutate(genus_cover_int = round(genus_cover)) %>%   
  mutate(year_index = case_when (YEAR == 1996 ~ '1', YEAR == 1997 ~ '2', 
                                 YEAR == 1998 ~ '3', YEAR == 1999 ~ '4',
                                 YEAR == 2000 ~ '5', YEAR== 2001 ~ '6', 
                                 YEAR == 2002 ~ '7', YEAR == 2003 ~ '8',
                                 YEAR== 2004 ~ '9', YEAR == 2005 ~ '10',
                                 YEAR== 2006 ~ '11', YEAR == 2007 ~ '12')) 


# making genus a factor
ITEX_shrubs_sp_trim$GENUS <- as.factor(as.character(ITEX_shrubs_sp_trim$GENUS ))
# making year a factor
ITEX_shrubs_sp_trim$YEAR <- as.numeric(ITEX_shrubs_sp_trim$YEAR)
ITEX_shrubs_sp_trim$year_index <- as.numeric(ITEX_shrubs_sp_trim$year_index)

hist(ITEX_shrubs_sp_trim$genus_cover_int) # right skewed
str(ITEX_shrubs_sp_trim)

# Dividing two subsites
ANWR_Atigun_shrub <- ITEX_shrubs_sp_trim %>% filter(SUBSITE %in% c("ATIGUN-A", "ATIGUN-B", "ATIGUN-C"))
ANWR_Jago_shrub <- ITEX_shrubs_sp_trim %>% filter(SUBSITE %in% c("JAGO-A", "JAGO-B"))

# making year numeric
ANWR_Atigun_shrub$year_index <- as.numeric(ANWR_Atigun_shrub$year_index)
ANWR_Jago_shrub$year_index <- as.numeric(ANWR_Jago_shrub$year_index)

# Atigun 
glm_atigun_shrub <- glm.nb(genus_cover_int~year_index+GENUS, data = ANWR_Atigun_shrub)
summary(glm_atigun_shrub)
# Output tables
tab_model(glm_atigun_shrub, file = "output/tables/glm_atigun_shrub.html")
webshot("output/tables/glm_atigun_shrub.html", "output/tables/glm_atigun_shrub.png")

check_overdispersion(glm_atigun_shrub) # no over.


# Jago
glm_jago_shrub <- glm.nb(genus_cover_int~year_index+GENUS, data = ANWR_Jago_shrub)
summary(glm_jago_shrub)
tab_model(glm_jago_shrub, file = "output/tables/glm_jago_shrub.html")
webshot("output/tables/glm_jago_shrub.html", "output/tables/glm_jago_shrub.png")

check_overdispersion(glm_jago_shrub) # no over.


# extracting predictions
atigun_shrub_preds <- ggpredict(glm_atigun_shrub, terms = c("year_index", "GENUS"), type = "re") %>% 
  rename(GENUS = group)

jago_shrub_preds <- ggpredict(glm_jago_shrub, terms = c("year_index", "GENUS"), type = "re")%>% 
  rename(GENUS = group)

# plotting predictions
# Atigun ----
(atigun_shrub <- ggplot(atigun_shrub_preds, aes(x = x, y = predicted, colour = GENUS)) +
   stat_smooth(method = "glm", aes(colour = GENUS, fill =GENUS), size = 1.5) +
   facet_wrap(~GENUS, ncol = 2, scales = "free_y"))+
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = GENUS), alpha = 0.1) +
   geom_point(data = ANWR_Atigun_shrub ,                      
              aes(x = year_index, y = genus_cover_int, colour = GENUS), size = 2.5) +
  scale_x_continuous(breaks=c(2,4,6,8,10))+
  scale_colour_manual(values = c("#DC9902", "#003654",  "#009E73","#CC79A7"))+
  scale_colour_manual(values = c("#DC9902", "#003654",  "#009E73","#CC79A7"))+
  scale_x_continuous(breaks=c(2,4,6,8,10,12))+
  labs(y = "Predicted cover (%) \n", x = "\nYear (indexed)") +
  theme_shrub()+
  theme(axis.text.x  = element_text(vjust=0.5, size=20, angle= 0, 
                                    colour = "black"), 
        legend.position = "none",
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        strip.text.x = element_text(size = 25, face = "italic" ))


ggsave(file = "output/figures/atigun_shrub.png")


# Jago ----
(jago_shrub <- ggplot(jago_shrub_preds, aes(x = x, y = predicted, colour = GENUS)) +
   stat_smooth(method = "glm", aes(colour = GENUS, fill = GENUS), size = 1.5) +
   facet_wrap(~GENUS, ncol = 3, scales = "free_y") +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = GENUS), alpha = 0.1, show.legend = FALSE) +
   geom_point(data = ANWR_Jago_shrub ,                      
              aes(x = year_index, y = genus_cover_int, colour = GENUS), size = 2.5))+
  scale_colour_manual(values = c("#DC9902", "#000000", "#46AAE2", "#003654", "#D55E00", "#009E73","#CC79A7", "#000000"))+
  scale_fill_manual(values = c("#DC9902", "#000000", "#46AAE2", "#003654", "#D55E00", "#009E73","#CC79A7", "#000000"))+
  scale_x_continuous(breaks=c(2,4,6,8,10))+
  labs(y = "Predicted cover (%) \n", x = "\nYear (indexed)") +
  theme_shrub()+
  theme(axis.text.x  = element_text(vjust=0.5, size=20, angle= 0, 
                                    colour = "black"), 
        legend.position = "none",
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        strip.text.x = element_text(size = 25, face = "italic" ))


ggsave(file = "output/figures/jago_shrub.png")


# scatter plot of different shrub genus change over time
(facet_scatter_shrub_genus <- (ggplot(ITEX_shrubs_sp_trim, aes(x = YEAR, y = genus_cover))+
                                 geom_point(size = 0.6, aes(colour=GENUS)) +
                                 scale_colour_manual(values = c("#DC9902", "#000000", "#46AAE2", "#003654", "#D55E00", "#009E73","#CC79A7", "#000000"))+
                                 geom_smooth(method = lm, aes(colour= GENUS, fill = GENUS), show.legend = FALSE))+
                                 scale_fill_manual(values = c("#DC9902", "#000000", "#46AAE2", "#003654", "#D55E00", "#009E73","#CC79A7", "#000000"))+
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

# Comparing different models:
lmer_shrub_sp_2a <- glmer.nb(genus_cover_prop~I(YEAR-1995) + (1|GENUS) + (1|PLOT), data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_2 <- glmer.nb(genus_cover_int~year_index + GENUS + (1|YEAR) + (1|PLOT), data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_3 <- glmer.nb(genus_cover_prop~I(YEAR-1995) + (1|GENUS) + (1|YEAR) + (1|PLOT), data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_0 <- glmer.nb(genus_cover_prop~I(YEAR-1995) + (1|YEAR) + (1|PLOT), data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_1 <- glmer.nb(genus_cover_prop~I(YEAR-1995)+ (1|GENUS) , data = ITEX_shrubs_sp_trim)
lmer_shrub_sp <- glmer.nb(genus_cover_int~year_index*GENUS + (1|YEAR) + (1|PLOT), data = ITEX_shrubs_sp_trim) # doesnt converge
dispersion_glmer(lmer_shrub_sp_2) #0.2723795
dispersion_glmer(lmer_shrub_sp_0)# 0.2750381
r.squaredGLMM(lmer_shrub_sp_2)
r.squaredGLMM(lmer_shrub_sp_0)
summary(lmer_shrub_sp_2) 

# Using model with genus fixed effect
summary(lmer_shrub_sp_2)
dispersion_glmer(lmer_shrub_sp_2) # 1.015989
r.squaredGLMM(lmer_shrub_sp_2) 
AIC(lmer_shrub_sp_null, lmer_shrub_sp_2)

# Output tables
tab_model(lmer_shrub_sp_2, file = "output/tables/lmer_shrub_sp_2.html")
webshot("output/tables/lmer_shrub_sp_2.html", "output/tables/lmer_shrub_sp_2.png")

tab_model(lmer_shrub_sp_0, file = "output/tables/lmer_shrub_sp_0.html")
webshot("output/tables/lmer_shrub_sp_0.html", "output/tables/lmer_shrub_sp_0.png")

# Output table 
stargazer(lmer_shrub_sp_2,
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "", 
          type = "html", out = "output/tables/lmer_shrub_sp.html")


# Trying model with random intercepts/slopes
lmer_shrub_sp_rand <- glmer.nb(genus_cover_int~year_index + (1+YEAR|GENUS) + (1|YEAR), data = ITEX_shrubs_sp_trim) 
# NB. doesn't converge with glmer.nb and with glmer we have overdispersion

# null model
lmer_shrub_sp_null <- glm.nb(genus_cover_int~1, data = ITEX_shrubs_sp_trim)

# comparing AIC values
AIC(lmer_shrub_sp_null, lmer_shrub_sp_2)

# Plotting fixed effects
(fe.effects <- plot_model(lmer_shrub_sp_2, show.values = TRUE))

# Plotting random effects
(re.effects <- plot_model(lmer_shrub_sp , type = "re", show.values = TRUE))


#model predicions
p <- ggpredict(lmer_shrub_sp_2, c("year_index", "GENUS"))
p$group <- as.factor(as.character(p$group))
str(p)

# Plot the predictions 
(genera_predictions <- ggplot(p, aes(x = x, y = predicted)) +
   stat_smooth(method = "lm", colour = "black", size = 1.5) +
    # facet_wrap(~ GENUS, scales = "free_y") +
    geom_ribbon(data = p, aes(ymin = conf.low, ymax = conf.high), alpha = 0)) +
    geom_point(data = ITEX_shrubs_sp_trim,                
               aes(x = year_index, y = genus_cover_int, colour = GENUS), size = 2)+
    scale_colour_manual(values = c("#DC9902", "#000000", "#46AAE2", "#003654", "#D55E00", "#009E73","#CC79A7", "#000000"))+
    #geom_smooth(data = ITEX_shrubs_sp_trim, method = lm, aes(colour = GENUS, fill = GENUS), show.legend = FALSE)+
   #scale_fill_manual(values = c("#DC9902", "#000000", "#46AAE2", "#003654", "#D55E00", "#009E73","#CC79A7", "#000000"))+
  scale_x_continuous(breaks=1:13)+
  labs(x = "\nYear (indexed)", y = "Mean cover (%)\n")+
    theme_shrub()+
    theme(axis.text.x  = element_text(vjust=0.5, size=20, angle= 0, 
                                      colour = "black"), 
          legend.position = "right",
          axis.title.x = element_text(size=25),
          axis.title.y = element_text(size=25),
          legend.text = element_text(size=20),
          legend.title = element_text(size=25),
          strip.text.x = element_text(size = 25, face = "italic" )) +
          guides(color = guide_legend(override.aes = list(size = 3)))

ggsave(file = "output/figures/genera_predictions.png")


# 3. SHRUB COVER IN SPACE  ----

## dividing subsites

ANWR_Atigun_shrub_lat <- ITEX_shrubs_mean_trim %>% filter(SUBSITE %in% c("ATIGUN-A", "ATIGUN-B", "ATIGUN-C"))
ANWR_Jago_shrub_lat <- ITEX_shrubs_mean_trim %>% filter(SUBSITE %in% c("JAGO-A", "JAGO-B"))

# standardise lat and long
ITEX_shrubs_mean_trim$LAT <- scale(ITEX_shrubs_mean_trim$LAT , center = TRUE, scale = TRUE)
ITEX_shrubs_mean_trim$LONG <- scale(ITEX_shrubs_mean_trim$LONG , center = TRUE, scale = TRUE)
hist(ITEX_shrubs_mean_trim$sum_cover_int) # normal distribution

# standardising lat and long
ANWR_Atigun_shrub_lat$LAT <- scale(ANWR_Atigun_shrub_lat$LAT , center = TRUE, scale = TRUE)
ANWR_Atigun_shrub_lat$LONG <- scale(ANWR_Atigun_shrub_lat$LONG , center = TRUE, scale = TRUE)
ANWR_Jago_shrub_lat$LAT <- scale(ANWR_Jago_shrub_lat$LAT , center = TRUE, scale = TRUE)
ANWR_Jago_shrub_lat$LONG <- scale(ANWR_Jago_shrub_lat$LONG , center = TRUE, scale = TRUE)

# Shrub cover vs latitude at Atigun 
shrub_lat <- glm.nb(sum_cover_int ~ LAT, data = ANWR_Atigun_shrub_lat)
summary(shrub_lat)

length(unique(ANWR_Atigun_shrub_lat$LAT))
plot(shrub_lat)

# Output table
stargazer(shrub_lat, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Plotting scatter (ATIGUN)
(cover_lat_scatter <- (ggplot(ANWR_Atigun_shrub_lat))+
    geom_point(aes(x = LAT, y = sum_cover_int), colour= "#009E73", size = 1) +
    geom_smooth(aes(x = LAT, y = sum_cover_int),colour = "#009E73", method = "glm", fill="#009E73", alpha = 0.1, size = 2) + 
    labs(y = "Shrub % cover\n", x = "\nScaled latitude") +
   # annotate(geom = "text", x = 1, y = 60, label="(a)", size = 10) +
    annotate(geom = "text", x = 0, y = 60, label="slope = -0.245*** ", size = 10) +
     theme_shrub())

ggsave(file = "output/figures/cover_lat_scatter.png")

# adding icon
lat_logo <- readPNG("lat_icon.png")
raster_lat_logo <- as.raster(lat_logo)
(cover_lat_scatter <- cover_lat_scatter + annotation_raster(raster_lat_logo, 0.5, 1, 30, 60))
ggsave(file = "output/figures/cover_lat_scatter.png")

# Extracting model predictions 
predictions_10 <- as.data.frame(predict(shrub_lat, newdata = ANWR_Atigun_shrub_lat, interval = "confidence")) # this gives overall predictions for the model
model_10_preds <- cbind(ANWR_Atigun_shrub_lat, predictions_10)

# write.csv(pred_model_10, file = "datasets/pred_model_10.csv")

# Plot the predictions 
(plot_model_shrub_lat <- (ggplot(model_10_preds, aes(LAT, fit)) +
                            geom_point(aes(x = LAT, y = sum_cover_int)         # adding the raw data 
                                       ,colour = "green4", size = 1) + 
                            stat_smooth(method=lm, colour = "black")+
                            geom_line(aes(y=lwr,  color = "grey"), linetype = "dashed")+
                            geom_line(aes(y=upr, color = "grey"), linetype = "dashed")+
                            #annotate(geom = "text", x = 1, y = 60, label="(a)", size = 10) +
                           # annotate(geom = "text", x = 0.5, y = 20, label="slope = -4.419*** ", size = 6) +
                            labs(x = "\nLatitude", y = "Shrub cover (%)\n", 
                                 title = "") + 
                            # scale_x_continuous(scale_x_continuous(breaks = 1996:2007))+ 
                            theme_shrub() + theme(legend.position = "none")))

ggsave(file = "output/figures/plot_model_shrub_lat.png")

# Shrub cover vs longitude
shrub_long <- glm.nb(sum_cover_int ~ LONG, data = ANWR_Jago_shrub_lat)
summary(shrub_long)

plot(shrub_long)

# Output table
stargazer(shrub_long, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
# F-statistic: 55.18 on 1 and 143 DF,  p-value: 9.123e-12***

# scatter
(cover_long_scatter <- (ggplot(ANWR_Atigun_shrub_lat))+
    geom_point(aes(x = LONG, y = sum_cover_int), colour = "skyblue", size = 2) +
    geom_smooth(aes(x = LONG, y = sum_cover_int), colour = "black", method = "lm") + 
    labs(y = "Mean shrub % cover\n", x = "\nLongitude") +
    #annotate(geom = "text", x = 1, y = 60, label="(b)", size = 10) +
    #annotate(geom = "text", x = 0.5, y = 20, label="slope = -4.419*** ", size = 6) +
    theme_shrub())

str(ANWR_Atigun_shrub_lat)


# ggsave(file = "output/figures/cover_long_scatter.png")

# Extracting model predictions 

predictions_11 <- as.data.frame(predict(shrub_lat, newdata =ANWR_Atigun_shrub_lat, interval = "confidence")) # this gives overall predictions for the model
model_11_preds <- cbind(ANWR_Atigun_shrub_lat, predictions_11)

# write.csv(pred_model_10, file = "datasets/pred_model_10.csv")

# Plot the predictions 
(plot_model_shrub_long <- (ggplot(model_11_preds, aes(LAT, fit)) +
                            geom_point(aes(x = LAT, y = sum_cover_int)         # adding the raw data 
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

