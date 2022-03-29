##%######################################################%##
#                                                          #
####         RQ3: SHRUB COVER in ANWR -----               ##
#               Erica Zaja - 05/02/2022                   ##
#                                                         ##
##%######################################################%##

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

# colour palette by Wong 
# https://davidmathlogic.com/colorblind/#%23000000-%23E69F00-%2356B4E9-%23009E73-%23F7EA40-%230072B2-%23D55E00-%23CC79A7 
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

# Mean shrub cover per plot per year
#  THIS IS THE RIGHT METHOD:
ITEX_shrubs_mean <- ITEX_shrubs %>%
  group_by(SiteSubsitePlotYear) %>%
  mutate(mean_cover = mean(RelCover)) %>%
  ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ITEX_shrubs_mean_trim <- ITEX_shrubs_mean %>% 
  dplyr::select(PLOT, YEAR, LAT, LONG, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE)%>% 
  mutate(mean_cover_prop = mean_cover/100)

str(ITEX_shrubs_mean_trim)
ITEX_shrubs_mean_trim$PLOT <- as.factor(as.character(ITEX_shrubs_mean_trim$PLOT))
hist(ITEX_shrubs_mean_trim$mean_cover_prop)


# Mean shrub cover change over time  
(shrub_mean_change <- (ggplot(ITEX_shrubs_mean_trim)+
                     geom_point(aes(x = YEAR, y = mean_cover_prop), colour = "green4", size = 1) +
                     geom_smooth(aes(x = YEAR, y = mean_cover_prop), colour= "black", method = "glm") + 
                     scale_x_continuous(breaks=c(1996, 1999, 2002,2005, 2007))+
                     labs(y = "Mean shrub % cover\n", x = "\nYear") + 
                  # annotate(geom = "text", x = 2007, y = 50, label="(a)", size = 10) +
                     theme_shrub()+
                     theme(axis.text.x = element_text(angle = 45))))

ggsave(shrub_mean_change, file = "output/figures/shrub_mean_change.png")          

# Model 6----
# Shrub cover over time
unique(ITEX_shrubs_mean_trim$YEAR)

# Transform percentage cover to proportion (dividing by 100)
ITEX_shrubs_mean_trim <- ITEX_shrubs_mean_trim %>% mutate(cover_prop = mean_cover/100)
hist(ITEX_shrubs_mean_trim$mean_cover)

# mixed effect model with plot and year as random effects
model_6 <- glmer.nb(mean_cover ~ I(YEAR-1995) + (1|PLOT) + (1|YEAR), data = ITEX_shrubs_mean_trim)
summary(model_6)
dispersion_glmer(model_6)# 0.9665275

model_6b <- glmer.nb(mean_cover_prop ~ I(YEAR-1995)  + (1|PLOT) + (1|YEAR), data = ITEX_shrubs_mean_trim)
summary(model_6b)
dispersion_glmer(model_6b)# 0.2243548

model_6a <- glmer(mean_cover ~ I(YEAR-1995) + (1|PLOT) + (1|YEAR), family = poisson, data = ITEX_shrubs_mean_trim)
summary(model_6a)
dispersion_glmer(model_6a)# 1.579284 overdispersed

model_6c <- glmer(mean_cover_prop ~ I(YEAR-1995) + (1|PLOT) + (1|YEAR), family = poisson, data = ITEX_shrubs_mean_trim)
summary(model_6c)
dispersion_glmer(model_6c)#0.1816644

# null
model_6_null <- glm.nb(mean_cover_prop ~1,  data = ITEX_shrubs_mean_trim)
AIC(model_6_null, model_6,model_6a, model_6b, model_6c)

# Checking model 6 assumptions 
plot(model_6a)
qqnorm(resid(model_6a))
qqline(resid(model_6a))  # points fall nicely onto the line - good!
dispersion_glmer(model_6a) #0.9665275

# Output table model 6 
stargazer(model_6, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


# Extracting model predictions 
pred_model_6 <- ggpredict(model_6, terms = c("YEAR", "PLOT"))
# this gives overall predictions for the model
pred_model_6a <- ggpredict(model_6, terms = c("YEAR"))

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
# shrub species
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

ITEX_shrubs_sp_trim$GENUS <- as.factor(as.character(ITEX_shrubs_sp_trim$GENUS ))
hist(ITEX_shrubs_sp_trim$genus_cover)
ITEX_shrubs_sp_trim$YEAR <- as.numeric(ITEX_shrubs_sp_trim$YEAR)
str(ITEX_shrubs_sp_trim)

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
ggsave(file = "output/figures/facet_scatter_shrub_genus.png")

# Model ----
hist(ITEX_shrubs_sp_trim$genus_cover)

lmer_shrub_sp_2 <- glmer.nb(genus_cover_prop~I(YEAR-1995) + (1|GENUS) + (1|PLOT), data = ITEX_shrubs_sp_trim)
summary(lmer_shrub_sp_2)
AIC(lmer_shrub_sp_null, lmer_shrub_sp_2)

lmer_shrub_sp_4 <- glmer.nb(genus_cover_prop~I(YEAR-1995) + GENUS + (1|YEAR), data = ITEX_shrubs_sp_trim)

lmer_shrub_sp_2 <- glmer.nb(genus_cover_prop~I(YEAR-1995) + GENUS + (1|YEAR), data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_3 <- glmer.nb(genus_cover_prop~I(YEAR-1995) + (1|GENUS) + (1|YEAR), data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_0 <- glmer.nb(genus_cover_prop~I(YEAR-1995) + (1|YEAR), data = ITEX_shrubs_sp_trim)
dispersion_glmer(lmer_shrub_sp_2) #0.2425216
dispersion_glmer(lmer_shrub_sp_3)#0.2723795
dispersion_glmer(lmer_shrub_sp_0)# 0.2750381
r.squaredGLMM(lmer_shrub_sp_2) 
r.squaredGLMM(lmer_shrub_sp_3)
r.squaredGLMM(lmer_shrub_sp_0)
summary(lmer_shrub_sp_0) 

tab_model(lmer_shrub_sp_3, file = "output/tables/lmer_shrub_sp_3.html")
webshot("output/tables/lmer_shrub_sp_3.html", "output/tables/lmer_shrub_sp_3.png")

# best model below:
lmer_shrub_sp_0 <- glmer.nb(genus_cover_prop~I(YEAR-1995)+ (1|GENUS), data = ITEX_shrubs_sp_trim)
summary(lmer_shrub_sp_0)
r.squaredGLMM(lmer_shrub_sp_0)

tab_model(lmer_shrub_sp_0, file = "output/tables/lmer_shrub_sp_0.html")
webshot("output/tables/lmer_shrub_sp_0.html", "output/tables/lmer_shrub_sp_0.png")

lmer_shrub_sp_2a <- glmer(genus_cover_prop~I(YEAR-1995) + GENUS + (1|YEAR), family = "poisson", data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_3a <- glmer(genus_cover_prop~I(YEAR-1995) + (1|GENUS) + (1|YEAR), family = "poisson", data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_0a <- glmer(genus_cover_prop~I(YEAR-1995) + (1|YEAR), family = "poisson", data = ITEX_shrubs_sp_trim)
dispersion_glmer(lmer_shrub_sp_2a) #0.2316012
dispersion_glmer(lmer_shrub_sp_3a)#0.2369076
dispersion_glmer(lmer_shrub_sp_0a)#0.2512536

# null model
lmer_shrub_sp_null <- glm.nb(genus_cover_prop~1, data = ITEX_shrubs_sp_trim)

AIC(lmer_shrub_sp_null, lmer_shrub_sp_2,lmer_shrub_sp_2a, lmer_shrub_sp_3,lmer_shrub_sp_3a, lmer_shrub_sp_0,lmer_shrub_sp_0a, lmer_shrub_sp)

## NB BEST model is lmer_shrub_sp_0 because lowest AIC (), but doesnt take genus as effect. SO next best is genus as random effect

# mixed effect model with year as random effects
lmer_shrub_sp <- glmer.nb(genus_cover_prop~I(YEAR-1995)*GENUS + (1|YEAR), data = ITEX_shrubs_sp_trim)
summary(lmer_shrub_sp)
r.squaredGLMM(lmer_shrub_sp)
dispersion_glmer(lmer_shrub_sp_2)
str(ITEX_shrubs_sp_trim)
plot(lmer_shrub_sp_2)

AIC(lmer_shrub_sp, lmer_shrub_sp_2,lmer_shrub_sp_3, lmer_shrub_sp_rand)
qqnorm(resid(lmer_shrub_sp))
qqline(resid(lmer_shrub_sp)) 


print(lmer_shrub_sp, correlation=TRUE)
stargazer(lmer_shrub_sp_3, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") 

# Output table model 7 
stargazer(lmer_shrub_sp,
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "", 
          type = "html", out = "output/tables/lmer_shrub_sp.html")

# Extracting model predictions 
pred_model_shrub_sp <- ggpredict(lmer_shrub_sp, terms = c("YEAR", "GENUS"))  # this gives overall predictions for the model
# write.csv(pred_model_9, file = "datasets/pred_model_9.csv")
pred_model_shrub_sp_1 <- ggpredict(lmer_shrub_sp, terms = "YEAR")
pred_model_shrub_sp_2 <- ggpredict(lmer_shrub_sp, terms = "GENUS")
str(ITEX_shrub_sp)

# assumptions
plot(lmer_shrub_sp)
qqnorm(resid(lmer_shrub_sp))
qqline(resid(lmer_shrub_sp)) 
dispersion_glmer(lmer_shrub_sp) #1.019987



# Plot the predictions 
(plot_model_shrub_sp <- (ggplot(pred_model_shrub_sp) + 
                           geom_line(aes(x = x, y = predicted, colour = group) +          # slope
                                       #geom_ribbon(aes(ymin = predicted - std.error, ymax = predicted + std.error), 
                                       #fill = "lightgrey", alpha = 0.5) +  # error band
                                       geom_point(data = ITEX_shrubs_sp_trim, aes(x = YEAR, y = genus_cover), size = 0.5) + 
                                       facet_wrap(~GENUS) +
                                       labs(x = "Year", y = "Shrub species cover (%)", 
                                            title = "Shrub species cover (%) in the ANWR") + 
                                       theme_shrub())))
# wrong

# trying diff graph
ITEX_shrubs_sp_trim$Predicted <- predict(lmer_shrub_sp, ITEX_shrubs_sp_trim)

# plot predicted values
ggplot(ITEX_shrubs_sp_trim, aes(YEAR, Predicted)) +
  facet_wrap(~GENUS) +
  geom_point(aes(x = YEAR, y = genus_cover, colour= GENUS), size = .5) +
  geom_smooth(aes(y = Predicted, colour = GENUS), linetype = "solid", 
              se = T, method = "lm") +
  guides(color=guide_legend(override.aes=list(fill=NA))) +  
  theme_shrub() + 
  xlab("Year")


# Plotting fixed effects
(fe.effects <- plot_model(lmer_shrub_sp , show.values = TRUE))

# Plotting random effects
(re.effects <- plot_model(lmer_shrub_sp , type = "re", show.values = TRUE))

# Random slopes ----
predict_sp <- ggpredict(lmer_shrub_sp , terms = c("YEAR", "GENUS"), type = "re") 

(pred_plot2 <- ggplot(predict_sp, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    # scale_y_continuous(limits = c(0, )) +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Predicted mean % cover\n"))

# Model with random slopes per genus
lmer_shrub_sp_rand <- glmer(genus_cover~YEAR + (1+YEAR|GENUS) + (1|YEAR), data = ITEX_shrubs_sp_trim) # doesnt converge with glmer.nb and with glmer we have overdispersion
summary(lmer_shrub_sp_rand )
dispersion_glmer(lmer_shrub_sp_rand) # 8.605041!!
plot(lmer_shrub_sp_rand)
qqnorm(resid(lmer_shrub_sp_rand))
qqline(resid(lmer_shrub_sp_rand)) 

predictions_rs_ri <- ggpredict(lmer_shrub_sp_rand , terms = c("YEAR", "GENUS"), type = "re")
(genus_rand_slopes <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    scale_colour_manual(values = c("green4", "green3", "red", "red4", "brown", "blue4", 'blue3'), name = "Shrub genus")+
    scale_x_continuous(breaks=1997:2009)+
  theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Mean shrub genus cover (%)\n")+
    theme_shrub()+ 
    theme(axis.text.x = element_text(angle = 45)))

stargazer(lmer_shrub_sp_rand, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

ggsave(file = "output/figures/genus_rand_slopes.png")

# SEPARATE models per genus ----
# NB. none significant 
# They are same as  facet but separate 

# Salix sp.
Salix <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Salix") 
salix_model <- glmer.nb(genus_cover ~ I(YEAR-1995)+ (1|YEAR), data = Salix)
summary(salix_model) # not sig
stargazer(salix_model, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
hist(Salix$genus_cover)

(salix_plot <- ggplot(Salix, aes(x = YEAR, y = genus_cover)) +
   geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n")) 

# Dryas sp.
Dryas <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Dryas") 
dryas_model <- glmer.nb(genus_cover ~ I(YEAR-1995) + (1|YEAR), data = Dryas)
summary(dryas_model)# not sig

stargazer(dryas_model, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

(dryas_plot <- ggplot(Dryas, aes(x = YEAR, y = genus_cover)) +
    geom_point(colour = "green4", size = 1)+
    stat_smooth(method = "lm", colour = 'black', fill = 'yellow4')  +
    scale_x_continuous(breaks=c(1996, 1999, 2002,2005, 2007))+
    labs(x = "\nYear", y = "Dryas % cover\n")+ 
    theme_shrub()+
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 45)))

  
# Vaccinium sp.
Vaccinium <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Vaccinium") 
hist(Vaccinium$genus_cover)
vacc_model <- glmer.nb(genus_cover ~ YEAR + (1|YEAR), data = Vaccinium)
summary(vacc_model)# not sig
(vacc_plot <- ggplot(Vaccinium, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# Arctostaphylos sp.
Arctostaphylos <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Arctostaphylos") 
arcto_model <- glmer.nb(genus_cover ~ YEAR + (1|YEAR), data = Arctostaphylos)
summary(arcto_model)# not sig
(arcto_plot <- ggplot(Arctostaphylos, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# Betula sp.
Betula <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Betula") 
betula_model <- glmer.nb(genus_cover ~ YEAR + (1|YEAR), data = Betula)
summary(betula_model)# not sig
(betula_plot <- ggplot(Betula, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# Cassiope sp.
Cassiope<-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Cassiope") 
cassiope_model <- glmer.nb(genus_cover ~ YEAR + (1|YEAR), data = Cassiope)
summary(cassiope_model)# not sig
(cassiope_plot <- ggplot(Cassiope, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# Ledum sp.
Ledum <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Ledum") 
ledum_model <- glmer.nb(genus_cover ~ YEAR + (1|YEAR), data = Ledum)
summary(ledum_model)# not sig
(ledum_plot <- ggplot(Ledum, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# Vary genus name to visualise distributions
hist(Salix$genus_cover)



# 3. SHRUB COVER IN SPACE  ----
# standardise lat and long
ITEX_shrubs_mean_trim$LAT <-scale(ITEX_shrubs_mean_trim$LAT , center = TRUE, scale = TRUE)
ITEX_shrubs_mean_trim$LONG <-scale(ITEX_shrubs_mean_trim$LONG , center = TRUE, scale = TRUE)
hist(ITEX_shrubs_mean_trim$mean_cover)
glimpse(ITEX_shrubs_mean_trim$mean_cover_prop)

# Shrub cover vs latitude 
shrub_lat <- glm.nb(mean_cover ~ LAT, data = ITEX_shrubs_mean_trim)
summary(shrub_lat)
length(unique(ITEX_shrubs_mean_trim$SiteSubsitePlot ))

shrub_lat_null <- glm.nb(mean_cover ~ 1, data = ITEX_shrubs_mean_trim)
AIC(shrub_lat, shrub_lat_null, shrub_long)
# Null deviance: 230.24 
# Residual deviance:  136.82 
# Pseudo R2 = 1-(136.82 /230.24)
# [1] 0.4057505 latitude explains ~40 % in variation in shrub cover

# mean shrub cover decreases with lat ( -0.44675 ***)

plot(shrub_lat)

stargazer(shrub_lat, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

(cover_lat_scatter <- (ggplot(ITEX_shrubs_mean_trim))+
    geom_point(aes(x = LAT, y = mean_cover), colour= "#009E73", size = 1) +
    geom_smooth(aes(x = LAT, y = mean_cover),colour = "black", method = "glm", fill="#009E73", size = 2) + 
    labs(y = "Mean shrub % cover\n", x = "\nScaled latitude") +
   # annotate(geom = "text", x = 1, y = 60, label="(a)", size = 10) +
    annotate(geom = "text", x = 0.5, y = 20, label="slope = -0.447*** ", size = 10) +
     theme_shrub())

ggsave(file = "output/figures/cover_lat_scatter.png")

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
shrub_long<- glm.nb(mean_cover ~ LONG, data = ITEX_shrubs_mean_trim)
summary(shrub_long)

plot(shrub_long)
stargazer(shrub_long, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
# F-statistic: 55.18 on 1 and 143 DF,  p-value: 9.123e-12***
# mean shrub cover decreases with longitude

(cover_long_scatter <- (ggplot(ITEX_shrubs_mean_trim))+
    geom_point(aes(x = LONG, y = mean_cover), colour = "skyblue", size = 2) +
    geom_smooth(aes(x = LONG, y = mean_cover), colour = "black", method = "lm") + 
    labs(y = "Mean shrub % cover\n", x = "\nLongitude") +
    annotate(geom = "text", x = 1, y = 60, label="(b)", size = 10) +
    annotate(geom = "text", x = 0.5, y = 20, label="slope = -4.419*** ", size = 6) +
    theme_shrub())


ggsave(file = "output/figures/cover_long_scatter.png")

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

ggsave(file = "output/figures/plot_model_shrub_long.png")



(panel_cover_latlong <- grid.arrange(arrangeGrob(cover_lat_scatter,cover_long_scatter,
                                           ncol = 2)) )# Sets number of panel columns

ggsave(panel_cover_latlong, file = "output/figures/panel_cover_latlong.png", height = 10, width = 20)

(panel_cover_latlong_pred <- grid.arrange(arrangeGrob(plot_model_shrub_lat,plot_model_shrub_long,
                                                 ncol = 2)) )# Sets number of panel columns

ggsave(panel_cover_latlong_pred, file = "output/figures/panel_cover_latlong_pred.png", height = 10, width = 20)


# LOGIC checks ----
# checking correlations
ITEX_shrubs_mean_trim_2 <- ITEX_shrubs_mean_trim %>% 
  dplyr::select(LAT, LONG, YEAR, mean_cover)
corrplot(cor(ITEX_shrubs_mean_trim_2))

ITEX_shrubs_sp_trim_2 <- ITEX_shrubs_sp_trim %>% 
  dplyr::select(YEAR, genus_cover)
corrplot(cor(ITEX_shrubs_sp_trim_2))

# END -----           

