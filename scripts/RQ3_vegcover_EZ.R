##%######################################################%##
#                                                          #
####         RQ3: VEGETATION COVER in ANWR -----          ##
#               Erica Zaja - 05/02/2022                   ##
#                                                         ##
##%######################################################%##

# RQ3: How has vegetation cover changed in the Arctic National Wildlife Refuge between 1996-2007? 

# LOADING LIBRARIES  ----
library(tidyverse)
library(cowplot)
library(brms)
library(ggpubr)
library(viridis)
library(ggtern)
library(lme4)
library(ggeffects)
library(sjPlot) 
library(stargazer)
library(MuMIn)
library(blmeco)
library(betareg)
library(emmeans)
library(lmtest)
library(sandwich)
library(webshot)

# LOADING DATA ----

load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/ITEX_EZ_diss.RData")

# DATA WRANGLING ----

# Exploration 
max(ITEX_EZ_diss$YEAR) # latest year: 2020
min(ITEX_EZ_diss$YEAR) # earliest year: 1981
unique(ITEX_EZ_diss$SITE) # Unique site names

# Retaining only Arctic National Wildlife Refuge (ANWR) site 
ANWR_veg <- ITEX_EZ_diss %>%
  filter(SITE=="ANWR") %>% na.omit()

# exploring the new dataset
range(ANWR_veg$YEAR) # Range of years of data: 1996-2007
length(unique(ANWR_veg$YEAR)) # 6 years
unique(ANWR_veg$YEAR) # Unique years
unique(ANWR_veg$PLOT) # Unique plot names (1 to 10)
length(unique(ANWR_veg$PLOT)) # 10 plots 

# How may unique plot and year combos
unique <- unique(ANWR_veg$SiteSubsitePlotYear) # 145

# Group the dataframe by year to see the number of plots per year
ANWR_plots <- ANWR_veg %>%
   group_by(YEAR) %>%
   summarise(plot.n = length(unique(SiteSubsitePlot))) %>% 
   ungroup() # different amount of plots each year

unique(ANWR_veg$FuncGroup) # Unique functional groups names
# [1] "Shrub"     "Lichen"    "Moss"      "Forb"      "Graminoid"
unique(ANWR_veg$GENUS) # Unique genus names

unique(ANWR_veg$gridcell) #"_68_-149"     "_69.5_-143.5"
# only 2 gridcells

# making smaller grid cells
ANWR_veg <- ANWR_veg %>% mutate(lat_grid = plyr::round_any(LAT, 0.1, f = floor)) %>% 
   mutate(lon_grid = ifelse(LONG >0, plyr::round_any(LONG, 0.1, f = floor), 
                            plyr::round_any(LONG, 0.1, f = ceiling))) %>%
   mutate(gridcell = paste0("_", lat_grid, "_", lon_grid))

unique(ANWR_veg$gridcell) #_68.4_-149.3" "_69.7_-143.6"
# still only 2 gridcells - not enough to use as random effect

# Making Genus, Site, Plot as factors (categorical)
ANWR_veg$GENUS <- as.factor(as.character(ANWR_veg$GENUS))
ANWR_veg$SITE <- as.factor(as.character(ANWR_veg$SITE))
ANWR_veg$PLOT <- as.factor(as.character(ANWR_veg$PLOT))
ANWR_veg$FuncGroup<- as.factor(as.character(ANWR_veg$FuncGroup))

str(ANWR_veg)
# saving new dataset
write.csv(ANWR_veg, file = "datasets/ITEX_data/ANWR_veg.csv")

# Loading full dataset
ANWR_veg <- read_csv("datasets/ITEX_data/ANWR_veg.csv")

# Creating separate datasets per functional group ----

# a. Filtering shrub only data
ITEX_shrubs <-  ANWR_veg %>% filter (FuncGroup == "Shrub") 
unique(ITEX_shrubs$GENUS) # Unique genus names 
# [1] "Dryas"          "Salix"          "Vaccinium"      "Arctostaphylos" "Betula"         "Cassiope"       "Ledum"         
str(ITEX_shrubs)

# b. Filtering graminoid only data 
ITEX_gram <-  ANWR_veg %>% filter (FuncGroup == "Graminoid") 

# c. Filtering forb only data
ITEX_forbs <-  ANWR_veg %>% filter (FuncGroup == "Forb")

# d. Filtering moss only data
ITEX_moss <-  ANWR_veg %>% filter (FuncGroup == "Moss")

# e. Filtering lichens only data
ITEX_lich <-  ANWR_veg %>% filter (FuncGroup == "Lichen")

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

# MODELLING mean cover of FUNCTIONAL GROUPS ----
unique(ANWR_veg$FuncGroup)  # checking I have all functional groups

# Visualising distribution with a histogram
(hist_all_veg <- ANWR_veg %>%
      ggplot(aes(x = RelCover, fill = FuncGroup)) +
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
      geom_vline(aes(xintercept = mean(RelCover)),            
                 colour = "black", linetype = "dashed", size = 1) +
      labs(x = "\nCover (%)", y = "Frequency\n") +
      scale_fill_manual(values=c( "#DC9902", "#46AAE2", "#003654", "#D55E00", "#009E73"), name = "Plant functional group")+
      theme_shrub() +
      theme(legend.text = element_text(size=20),
            legend.title = element_text(size=25)) )

# ggsave(file = "output/figures/hist_all_veg.png")

# making functional group a factor 
ANWR_veg$FuncGroup <- as.factor(as.character(ANWR_veg$FuncGroup))

# calculating mean cover of functional groups
ANWR_veg_fg <- ANWR_veg %>%
   group_by(SiteSubsitePlotYear, FuncGroup) %>%
   mutate(mean_cover = mean(RelCover)) %>%
   ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ANWR_veg_fg_trim <- ANWR_veg_fg %>% 
   dplyr::select(PLOT, YEAR, FuncGroup, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
   distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE)%>% 
   mutate(mean_cover_prop = mean_cover/100) # making into proportion data

# making func group a factor in the new dataset
ANWR_veg_fg_trim$FuncGroup <- as.factor(as.character(ANWR_veg_fg_trim$FuncGroup))
hist(ANWR_veg_fg_trim$mean_cover_prop) # checking proportion data distribution

# MODEL(s) 11 ----
# Trying and comparing different model syntaxes

# glmer.nb, functional group fixed effect, year and plot random effects
lmer_all_2a <- glmer.nb(mean_cover_prop~I(YEAR-1995) + FuncGroup + (1|YEAR) + (1|PLOT), data = ANWR_veg_fg_trim)
dispersion_glmer(lmer_all_2a)
summary(lmer_all_2a)
AIC(lmer_all_2a, lmer_all_null)
r.squaredGLMM(lmer_all_2a)
### THIS IS THE MODEL SELECTED FOR THE RESULTS

# glmer poisson, no functional group
lmer_all_0 <- glmer(mean_cover_prop~I(YEAR-1995) + (1|YEAR) + (1|PLOT), family = "poisson", data = ANWR_veg_fg_trim)
summary(lmer_all_0)
r.squaredGLMM(lmer_all_0)
dispersion_glmer(lmer_all_0)
plot(lmer_all_0)

# glmer poisson, functional group fixed effect 
lmer_all_2 <- glmer(mean_cover_prop~I(YEAR-1995) + FuncGroup + (1|YEAR) + (1|PLOT),family = "poisson", data = ANWR_veg_fg_trim)
summary(lmer_all_2) 
r.squaredGLMM(lmer_all_2)
dispersion_glmer(lmer_all_2)
plot(lmer_all_2)

#glm.nb, functional group fixed effect, model without year and plot random effects 
lmer_all_2a_try <- glm.nb(mean_cover_prop~I(YEAR-1995) + FuncGroup, data = ANWR_veg_fg_trim)
summary(lmer_all_2a_try)

# glmer.nb, functional group random effect, no year and plot random effects
lmer_all_3_try <- glmer.nb(mean_cover_prop~I(YEAR-1995) + (1|FuncGroup), data = ANWR_veg_fg_trim)
summary(lmer_all_3_try)

# glmer.nb, functional group random effect, year and plot random effects
lmer_all_3a <- glmer.nb(mean_cover_prop~I(YEAR-1995) + (1|FuncGroup) + (1|YEAR) + (1|PLOT), data = ANWR_veg_fg_trim)
dispersion_glmer(lmer_all_3a) #0.1845308
summary(lmer_all_3a)
r.squaredGLMM(lmer_all_3a)

# glmer poisson, functional group random effect, year and plot random effects
lmer_all_3b <- glmer(mean_cover_prop~I(YEAR-1995) + (1|FuncGroup) + (1|YEAR) + (1|PLOT), family = "poisson", data = ANWR_veg_fg_trim)
dispersion_glmer(lmer_all_3b) # 0.1595953

# glmer.nb, random slopes 
lmer_all_4 <- glmer.nb(mean_cover_prop~I(YEAR-1995) + (1+YEAR|FuncGroup) + (1|YEAR) + (1|PLOT), data = ANWR_veg_fg_trim)
summary(lmer_all_4)
r.squaredGLMM(lmer_all_4)

# glmer.nb with functional group interacting with year
lmer_all <- glmer.nb(mean_cover_prop~I(YEAR-1995)*FuncGroup + (1|YEAR) + (1|PLOT), data = ANWR_veg_fg_trim)
summary(lmer_all)
r.squaredGLMM(lmer_all)
AIC(lmer_all, lmer_all_null, lmer_all_4)
dispersion_glmer(lmer_all_2)# 0.9356298

# glmer poisson, with functional group interacting with year
lmer_all_2<- glmer(mean_cover~I(YEAR-1995)*FuncGroup + (1|YEAR) + (1|PLOT), family = poisson, data = ANWR_veg_fg_trim)
summary(lmer_all_2)
plot(lmer_all_2)
dispersion_glmer(lmer_all_4)# 1.477103

# saving model outputs
tab_model(lmer_all_2a, file = "output/tables/lmer_2a.html")
webshot("output/tables/lmer_2a.html", "output/tables/lmer_2a.png")

tab_model(lmer_all_2a_try, file = "output/tables/lmer_2a_try.html")
webshot("output/tables/lmer_2a_try.html", "output/tables/lmer_2a_try.png")

# null model
lmer_all_null <- glm.nb(mean_cover_prop~1, data = ANWR_veg_fg_trim)
glimpse(ANWR_veg_fg_trim)

# Model selection ----
# comparing AIC values 
AIC(lmer_all_null, lmer_all, lmer_all_0, lmer_all_2, lmer_all_2a, lmer_all_3, lmer_all_3a, lmer_all_3b, lmer_all_4)

# Model selected: glmer.nb with fixed effect f group and year and plot random, but AIC equivalent to null model


# DATA VISUALSATION -----
(scatter_fgroups <- (ggplot(ANWR_veg_fg_trim, aes(x = YEAR, y = mean_cover, colour = FuncGroup))+
                        geom_point(size = 0.5) +
                        geom_smooth(method = "lm", aes(colour= FuncGroup, fill = FuncGroup), show.legend = FALSE)) + 
    facet_wrap(~FuncGroup, scales = "free_y") +
    scale_x_continuous(breaks=c(1996, 1999, 2002,2005, 2007))+
    scale_colour_manual(values = c("#DC9902", "#46AAE2", "#003654", "#D55E00", "#009E73"))+
    scale_fill_manual(values = c("#DC9902", "#46AAE2", "#003654", "#D55E00", "#009E73"))+
    labs(y = "Mean cover (%) \n", x = "\nYear") +
    theme_shrub() +
    theme(axis.text.x  = element_text(vjust=0.5, size=20, angle= 45, colour = "black"),
          legend.position = "none",
          strip.text.x = element_text(size = 20),
          axis.title.x = element_text(size=25),
          axis.title.y = element_text(size=25),
          axis.text.y = element_text(size=25, hjust = 1)))

# ggsave(file = "output/figures/scatter_fgroups.png")

# Extracting model predictions of selected model 
pred_lmer_all_1 <- ggpredict(lmer_all_2a , terms = c("YEAR"))
pred_lmer_all_2 <- ggpredict(lmer_all_2a , terms = c("FuncGroup"))  # this gives overall predictions for the model
# write.csv(pred_model_10, file = "datasets/pred_model_10.csv")

# trying different graph
ANWR_veg_fg_trim$Predicted <- predict(lmer_all_2a, ANWR_veg_fg_trim)

# plot predicted values
ggplot(ANWR_veg_fg_trim, aes(YEAR, Predicted)) +
   facet_wrap(~FuncGroup) +
   geom_point(aes(x = YEAR, y = mean_cover, colour= FuncGroup), size = .5) +
   geom_smooth(aes(y = Predicted, colour= FuncGroup), linetype = "solid", 
               se = T, method = "lm") +
   guides(color=guide_legend(override.aes=list(fill=NA))) +  
   theme_shrub() + 
   xlab("Year") # ugly

# extracting model predictions
pred.mm <- ggpredict(lmer_all_2a, terms = c("YEAR"))

# Plotting fixed effects
(fe.effects <- plot_model(lmer_all_2a, show.values = TRUE))

# Plotting random effects
(re.effects <- plot_model(lmer_all_2a, type = "re", show.values = TRUE))


############################################################ END -----




