##%######################################################%##
#                                                          #
####         RQ3: VEGETATION COVER in ANWR -----          ##
#               Erica Zaja - 05/02/2022                   ##
#                                                         ##
##%######################################################%##

# RQ3: How has vegetation cover changed in the Arctic National Wildlife Refuge between 1996-2007? 
# colour palettes credits: Bang Wong and Paul Tol

# PART 1: FUNCTIONAL GROUPS ----

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

# MODELLING total cover change of FUNCTIONAL GROUPS ----
unique(ANWR_veg$FuncGroup)  # checking I have all functional groups

# Visualising distribution with a histogram
(hist_all_veg <- ANWR_veg %>%
      ggplot(aes(x = RelCover, fill = FuncGroup)) +
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
      geom_vline(aes(xintercept = mean(RelCover)),            
                 colour = "black", linetype = "dashed", size = 1) +
      labs(x = "\nCover (%)", y = "Frequency\n") +
      scale_fill_manual(values = c("#332288", "#117733", "#DDCC77", "#CC6677", "#882255"), name = "Functional group")+
      theme_shrub() +
      theme(legend.text = element_text(size=20),
            legend.title = element_text(size=25)) )

ggsave(file = "output/figures/hist_all_veg.png")

# making functional group a factor 
ANWR_veg$FuncGroup <- as.factor(as.character(ANWR_veg$FuncGroup))


# calculating tot cover of functional groups
ANWR_veg_fg <- ANWR_veg %>%
   group_by(SiteSubsitePlotYear, FuncGroup) %>%
   mutate(sum_cover = sum(RelCover)) %>%
   ungroup()

hist(ANWR_veg_fg$sum_cover)


# Shrinking the dataframe to retain one row per plot etc.
ANWR_veg_fg_trim <- ANWR_veg_fg %>% 
   dplyr::select(PLOT, YEAR, FuncGroup, SUBSITE, SiteSubsitePlotYear, SiteSubsitePlot, sum_cover, lat_grid, lon_grid, gridcell) %>% 
   distinct(SiteSubsitePlotYear, sum_cover, .keep_all = TRUE)%>% 
   mutate(sum_cover_int = round(sum_cover)) %>%   
   mutate(year_index = case_when (YEAR == 1996 ~ '1', YEAR == 1997 ~ '2', # index year
                                  YEAR == 1998 ~ '3', YEAR == 1999 ~ '4',
                                  YEAR == 2000 ~ '5', YEAR== 2001 ~ '6', 
                                  YEAR == 2002 ~ '7', YEAR == 2003 ~ '8',
                                  YEAR== 2004 ~ '9', YEAR == 2005 ~ '10',
                                 YEAR== 2006 ~ '11', YEAR == 2007 ~ '12')) 


# f group as factor in the new dataset
ANWR_veg_fg_trim$FuncGroup <- as.factor(as.character(ANWR_veg_fg_trim$FuncGroup))

# indexed year numeric
ANWR_veg_fg_trim$year_index <- as.numeric(ANWR_veg_fg_trim$year_index)

hist(ANWR_veg_fg_trim$sum_cover_int) # checking proportion data distribution
unique(ANWR_veg_fg_trim$SiteSubsitePlot)

# Dividing two subsites
ANWR_Atigun <- ANWR_veg_fg_trim %>% filter(SUBSITE %in% c("ATIGUN-A", "ATIGUN-B", "ATIGUN-C"))
ANWR_Jago <- ANWR_veg_fg_trim %>% filter(SUBSITE %in% c("JAGO-A", "JAGO-B"))

# making year numeric
ANWR_Atigun$year_index <- as.numeric(ANWR_Atigun$year_index)
ANWR_Jago$year_index <- as.numeric(ANWR_Jago$year_index)
str(ANWR_Atigun)

# MODEL(s) 11 ----

# Atigun model
glm_atigun <- glm.nb(sum_cover_int~year_index + FuncGroup, data = ANWR_Atigun)
summary(glm_atigun)

tab_model(glm_atigun, file = "output/tables/glm_atigun.html")
webshot("output/tables/glm_atigun.html", "output/tables/glm_atigun.png")

check_overdispersion(glm_poisson_atigun) # no over
glm_poisson_atigun <- glm(sum_cover_int~year_index + FuncGroup, family = "poisson", data = ANWR_Atigun)


# Jago 
glm_jago <- glm.nb(sum_cover_int~year_index+FuncGroup, data = ANWR_Jago)
summary(glm_jago)

glm_poisson <- glm(sum_cover_int~year_index + FuncGroup, family = "poisson", data = ANWR_Jago)
check_overdispersion(glm_poisson) #overdispersion

tab_model(glm_jago, file = "output/tables/glm_jago.html")
webshot("output/tables/glm_jago.html", "output/tables/glm_jago.png")

check_overdispersion(glm_jago) # some over.
performance(glm_jago)
plot(resid(glm_jago))

# extracting predictions Atigun
atigun_preds <- ggpredict(glm_atigun, terms = c("year_index", "FuncGroup"), type = "re") %>% 
   rename(FuncGroup = group)

# extracting predictions Jago
jago_preds <- ggpredict(glm_jago, terms = c("year_index", "FuncGroup"), type = "re") %>% 
rename(FuncGroup = group)

# plotting predictions
# Atigun f.groups ----
(atigun_fgroups <- ggplot(atigun_preds, aes(x = x, y = predicted, colour=FuncGroup))+
   stat_smooth(method = "glm", aes(colour = FuncGroup, fill = FuncGroup), size = 1.5) +
   facet_wrap(~FuncGroup, ncol = 3, scales = "free_y"))+
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = FuncGroup), alpha = 0.1) +
   geom_point(data = ANWR_Atigun, aes(x = year_index, y = sum_cover_int, colour = FuncGroup), size = 2.5) +
   scale_colour_manual(values = c("#332288", "#117733", "#DDCC77", "#CC6677", "#882255"))+
   scale_fill_manual(values = c("#332288", "#117733", "#DDCC77", "#CC6677", "#882255"))+
   scale_x_continuous(breaks=c(2,4,6,8,10,12))+
   labs(y = "Predicted cover (%) \n", x = "\nYear (indexed)") +
   theme_shrub()+
   theme(axis.text.x  = element_text(vjust=0.5, size=20, angle= 0, 
                                     colour = "black"), 
         legend.position = "none",
         axis.title.x = element_text(size=25),
         axis.title.y = element_text(size=25),
         strip.text.x = element_text(size = 25, face = "italic" ))

ggsave(file = "output/figures/atigun_fgroups.png")

# Jago f. groups----
(jago_fgroups <- ggplot(jago_preds, aes(x = x, y = predicted, colour=FuncGroup))+
    stat_smooth(method = "glm", aes(colour = FuncGroup, fill = FuncGroup), size = 1.5) +
    facet_wrap(~FuncGroup, ncol = 3, scales = "free_y"))+
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = FuncGroup), alpha = 0.1) +
   geom_point(data = ANWR_Jago, aes(x = year_index, y = sum_cover_int, colour = FuncGroup), size = 2.5) +
   scale_colour_manual(values = c("#332288", "#117733", "#DDCC77", "#CC6677", "#882255"))+
   scale_fill_manual(values = c("#332288", "#117733", "#DDCC77", "#CC6677", "#882255"))+
   scale_x_continuous(breaks=c(2,4,6,8,10,12))+
   labs(y = "Predicted cover (%) \n", x = "\nYear (indexed)") +
   theme_shrub()+
   theme(axis.text.x  = element_text(vjust=0.5, size=20, angle= 0, 
                                     colour = "black"), 
         legend.position = "none",
         axis.title.x = element_text(size=25),
         axis.title.y = element_text(size=25),
         strip.text.x = element_text(size = 25, face = "italic" ))

ggsave(file = "output/figures/jago_fgroups.png")

# DATA VISUALSATION -----
(scatter_fgroups <- (ggplot(ANWR_veg_fg_trim, aes(x = YEAR, y = mean_cover, colour = FuncGroup))+
                        geom_point(size = 0.5) +
                        geom_smooth(method = "lm", aes(colour= FuncGroup, fill = FuncGroup), show.legend = FALSE)) + 
    facet_wrap(~FuncGroup, scales = "free_y") +
    scale_x_continuous(breaks=c(1996, 1999, 2002,2005, 2007))+
    scale_colour_manual(values = c("#332288", "#117733", "#DDCC77", "#CC6677", "#882255"))+
    scale_fill_manual(values = c("#332288", "#117733", "#DDCC77", "#CC6677", "#882255"))+
    labs(y = "Mean cover (%) \n", x = "\nYear") +
    theme_shrub() +
    theme(axis.text.x  = element_text(vjust=0.5, size=20, angle= 45, colour = "black"),
          legend.position = "none",
          strip.text.x = element_text(size = 20),
          axis.title.x = element_text(size=25),
          axis.title.y = element_text(size=25),
          axis.text.y = element_text(size=25, hjust = 1)))

#ggsave(file = "output/figures/scatter_fgroups.png")


############################################################ END -----




