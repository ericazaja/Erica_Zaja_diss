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
library(sjPlot)  # to visualise model outputs
library(stargazer)

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

range(ANWR_veg$YEAR) # # Range of years of data: 1996-2007
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

ANWR_grids <- ANWR_veg %>%
   group_by(gridcell) %>%
   summarise(plot.n = length(unique(PLOT))) %>% 
   ungroup() # 10 plots in one gridcell and 10 in the other

# Making Genus, Site, Plot as factors (categorical)
ANWR_veg$GENUS <- as.factor(as.character(ANWR_veg$GENUS))
ANWR_veg$SITE <- as.factor(as.character(ANWR_veg$SITE))
ANWR_veg$PLOT <- as.factor(as.character(ANWR_veg$PLOT))
ANWR_veg$FuncGroup<- as.factor(as.character(ANWR_veg$FuncGroup))

str(ANWR_veg)
write.csv(ANWR_veg, file = "datasets/ITEX_data/ANWR_veg.csv")

# Separate datasets per functional group ----
# Filtering shrub only data
ITEX_shrubs <-  ANWR_veg %>% filter (FuncGroup == "Shrub") 
unique(ITEX_shrubs$GENUS) # Unique genus names 
# [1] "Dryas"          "Salix"          "Vaccinium"      "Arctostaphylos" "Betula"         "Cassiope"       "Ledum"         
str(ITEX_shrubs)

# Filtering graminoid only data 
ITEX_gram <-  ANWR_veg %>% filter (FuncGroup == "Graminoid") 

# Filtering forb only data
ITEX_forbs <-  ANWR_veg %>% filter (FuncGroup == "Forb")

# Filtering moss only data
ITEX_moss <-  ANWR_veg %>% filter (FuncGroup == "Moss")

# Filtering lichens only data
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

# MODELLING ----

# 1. GRAMINOID COVER ----
# Mean graminoid cover per plot per year
ITEX_gram_mean <- ITEX_gram %>%
   group_by(SiteSubsitePlotYear) %>%
   mutate(mean_cover = mean(RelCover)) %>%
   ungroup()

ITEX_gram$PLOT <- as.factor(as.character(ITEX_gram$PLOT))

# Shrinking the dataframe to retain one row per plot etc.
ITEX_gram_mean_trim <- ITEX_gram_mean %>% 
   dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
   distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE)

(graminoid_scatter <- (ggplot(ITEX_gram_mean_trim)+
                          geom_point(aes(x = YEAR, y = mean_cover, colour = PLOT), size = 2) +
                          geom_smooth(aes(x = YEAR, y = mean_cover), method = "lm") + 
                          labs(y = "Mean graminoid cover\n", x = "\nYear") +
                          scale_x_continuous(breaks=1997:2009)+
                          theme_shrub()+
                          theme(axis.text.x = element_text(angle = 45))))

# Model 7 ----
hist(ITEX_gram_mean_trim$mean_cover)
# Graminoid cover over time 
# mixed effect model with plot and year as random effects
model_7 <- glmer.nb(mean_cover~YEAR + (1|PLOT) + (1|YEAR), data = ITEX_gram_mean_trim)
summary(model_7)
# total variance: 19.36 + 59.00 =78.36
# variance for plot =  19.36
# amount of variance explained by random effect:  19.36 /78.36 = 0.2470648= ~25%
# I.e. differences between plots explain ~25% of the variance 
# that’s “left over” after the variance explained by our fixed effect (year).
# estimate for precip (exp variable = 0.0867)
# not significant effect of year on gram cover 

# Checking model 7 assumptions 
plot(model_7)
qqnorm(resid(model_7))
qqline(resid(model_7))  # points fall nicely onto the line - good!

# Output table model 7 
stargazer(model_7, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
pred_model_7 <- ggpredict(model_7, terms = c("YEAR"))  # this gives overall predictions for the model
# write.csv(pred_model_7, file = "datasets/pred_model_7.csv")

# Plot the predictions 
(gram_cover_ANWR <- (ggplot(pred_model_7) + 
                     geom_line(aes(x = x, y = predicted)) +          # slope
                     geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                                 fill = "lightgrey", alpha = 0.5) +  # error band
                     geom_point(data = ITEX_gram,                      # adding the raw data 
                                aes(x = YEAR, y = Mean_cover, colour = PLOT), size = 0.5) + 
                     labs(x = "\nYear", y = "Graminoid cover (%)\n", 
                          title = "Graminoid cover (%) constant in the ANWR\n") + 
                     theme_shrub()
))

# ggsave( file = "output/figures/gram_cover_ANWR.png")

# checking that overall graminoid cover change has same trend as mean cover change
# Total graminoid cover 
ITEX_gram_tot <- ITEX_gram %>%
   group_by(SiteSubsitePlotYear) %>%
   mutate(tot_cover = sum(FuncPlotCover)) %>%
   ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ITEX_gram_tot_trim <- ITEX_gram_tot  %>% 
   dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, tot_cover, lat_grid, lon_grid, gridcell) %>% 
   distinct(SiteSubsitePlotYear, tot_cover, .keep_all = TRUE)

ITEX_gram_tot_trim$PLOT <- as.factor(as.character(ITEX_gram_tot_trim$PLOT))
hist(ITEX_gram_tot_trim$tot_cover)

# Tot shrub cover change over time  
(gram_scatter_sum <- (ggplot(ITEX_gram_tot_trim)+
                          geom_point(aes(x = YEAR, y = tot_cover, colour = PLOT), size = 2) +
                          geom_smooth(aes(x = YEAR, y = tot_cover), method = "lm") + 
                         scale_x_continuous(breaks=1997:2009)+
                         labs(y = "Total graminoid % cover\n", x = "\nYear") + 
                          theme_shrub()+
                         theme(axis.text.x = element_text(angle = 45))))
# similar trend to mean


# 2. FORB COVER ----

# Mean forb cover per plot per year
ITEX_forb_mean <- ITEX_forbs %>%
   group_by(SiteSubsitePlotYear) %>%
   mutate(mean_cover = mean(RelCover)) %>%
   ungroup()

ITEX_forb_mean$PLOT <- as.factor(as.character(ITEX_forb_mean$PLOT))

# Shrinking the dataframe to retain one row per plot etc.
ITEX_forb_mean_trim <- ITEX_forb_mean %>% 
   dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
   distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE)

(forb_scatter <- (ggplot(ITEX_forb_mean_trim)+
   geom_point(aes(x = YEAR, y = mean_cover, colour = PLOT), size = 2) +
   geom_smooth(aes(x = YEAR, y = mean_cover), method = "lm") + 
      labs(y = "Mean forb cover\n", x = "\nYear") +
      scale_x_continuous(breaks=1997:2009)+
      theme_shrub() +
      theme(axis.text.x = element_text(angle = 45))))
# Forb cover decreasing

# Model 8 ----
# Forb cover over time 
# mixed effect model with plot and year as random effects
model_8 <- lmer(mean_cover~YEAR + (1|PLOT) + (1|YEAR), data = ITEX_forb_mean_trim)
summary(model_8)
# total variance: 5.687 + 13.076   =18.763
# variance for plot =   5.687 
# amount of variance explained by random effect:  5.687  /18.763 = 0.3030965= ~30%
# I.e. differences between plots explain ~30% of the variance 
# that’s “left over” after the variance explained by our fixed effect (year).
# estimate for year (exp variable = -0.201*** ) i.e. year negatively impats forb cover
# significant effect of year on forb cover  = -0.201***        

# Checking model 8 assumptions 
plot(model_8)
qqnorm(resid(model_8))
qqline(resid(model_8))  # points fall nicely onto the line - good!

# Output table model 8 
stargazer(model_8, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
# significant effect of year

# Extracting model predictions 
pred_model_8 <- ggpredict(model_8, terms = c("YEAR"))  # this gives overall predictions for the model
# write.csv(pred_model_8, file = "datasets/pred_model_8.csv")

# Plot the predictions 
(forb_cover_ANWR <- (ggplot(pred_model_8) + 
                     geom_line(aes(x = x, y = predicted)) +          # slope
                     geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                                 fill = "lightgrey", alpha = 0.5) +  # error band
                     geom_point(data = ITEX_forb_mean_trim,                      # adding the raw data 
                                aes(x = YEAR, y = mean_cover, colour = PLOT), size = 0.5) + 
                     labs(x = "\nYear", y = "Forb cover (%)\n", 
                          title = "Forb cover (%) decrease in the ANWR\n") + 
                     theme_shrub()
))

ggsave( file = "output/figures/forb_cover_ANWR.png")

# 3. MOSS COVER  ----
# Mean moss cover per plot per year
ITEX_moss_mean <- ITEX_moss %>%
   group_by(SiteSubsitePlotYear) %>%
   mutate(mean_cover = mean(RelCover)) %>%
   ungroup()

ITEX_moss_mean$PLOT <- as.factor(as.character(ITEX_moss_mean$PLOT))

# Shrinking the dataframe to retain one row per plot etc.
ITEX_moss_mean_trim <- ITEX_moss_mean %>% 
   dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
   distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE)

(moss_scatter <- (ggplot(ITEX_moss_mean_trim)+
    geom_point(aes(x = YEAR, y = mean_cover, colour = PLOT),size = 2) +
    geom_smooth(aes(x = YEAR, y = mean_cover), method = "lm") + 
       labs(y = "Mean moss cover\n", x = "\nYear") +
      theme_shrub()))
# Moss cover increasing 

# Model 9 ----
# Moss cover over time
# mixed effect model with plot and year as random effects
model_9 <- lmer(mean_cover~YEAR + (1|PLOT) + (1|YEAR), data = ITEX_moss_mean_trim)
summary(model_9)
# total variance: 15.09    + 61.07   =76.16
# variance for plot =   15.09
# amount of variance explained by random effect:  15.09  /76.16=0.1981355= ~20%
# I.e. differences between plots explain ~20% of the variance 
# that’s “left over” after the variance explained by our fixed effect (year).
# estimate for year (exp variable = 6.353e-01***  ) year positively impacts moss cover 
# significant effect of year on moss cover  = 0.635***        

# Checking model 9 assumptions 
plot(model_9)
qqnorm(resid(model_9))
qqline(resid(model_9))  # points fall nicely onto the line - good!

# Output table model 9
stargazer(model_9, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
# year significant

# Extracting model predictions 
pred_model_9 <- ggpredict(model_9, terms = c("YEAR"))  # this gives overall predictions for the model
#write.csv(pred_model_9, file = "datasets/pred_model_9.csv")

# Plot the predictions 
(moss_cover_ANWR <- (ggplot(pred_model_9) + 
                     geom_line(aes(x = x, y = predicted)) +          # slope
                     geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                                 fill = "lightgrey", alpha = 0.5) +  # error band
                     geom_point(data = ITEX_moss_mean_trim,                      # adding the raw data 
                                aes(x = YEAR, y = mean_cover, colour = PLOT), size = 0.5) + 
                     labs(x = "\nYear", y = "Moss cover (%)\n", 
                          title = "Moss cover (%) increase in the ANWR\n") + 
                     theme_shrub()
))

# ggsave( file = "output/figures/moss_cover_ANWR.png")


# 4. LICHEN COVER  ----
# Mean moss cover per plot per year
ITEX_lich_mean<- ITEX_lich %>%
   group_by(SiteSubsitePlotYear) %>%
   mutate(mean_cover = mean(RelCover)) %>%
   ungroup()

ITEX_lich_mean$PLOT <- as.factor(as.character(ITEX_lich_mean$PLOT))

# Shrinking the dataframe to retain one row per plot etc.
ITEX_lich_mean_trim <- ITEX_lich_mean %>% 
   dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
   distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE)


(lichen_scatter <- (ggplot(ITEX_lich_mean_trim))+
    geom_point(aes(x = YEAR, y = mean_cover, colour = PLOT), size = 2) +
    geom_smooth(aes(x = YEAR, y = mean_cover), method = "lm") + 
       labs(y = "Mean lichen cover\n", x = "\nYear") +
      theme_shrub())
## Lichen cover increasing

# Model 10 ----
# Lichen cover over time 
# mixed effect model with plot and year as random effects
model_10 <- lmer(mean_cover~YEAR + (1|PLOT) + (1|YEAR), data = ITEX_lich_mean_trim)
summary(model_10)
# total variance: 99.37     + 260.29  = 359.66
# variance for plot =   99.37
# amount of variance explained by random effect:  99.37  /359.66=0.2762887= ~28%
# I.e. differences between plots explain ~28% of the variance 
# that’s “left over” after the variance explained by our fixed effect (year).
# estimate for year (exp variable =  0.445** ) year positively impacts moss cover 
# significant effect of year on lichen cover  = 0.445**         

# Checking model 9 assumptions 
plot(model_10)
qqnorm(resid(model_10))
qqline(resid(model_10))  # points fall nicely onto the line - good!

# Output table model 9 
stargazer(model_10, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
# year significant

# Extracting model predictions 
pred_model_10 <- ggpredict(model_10, terms = c("YEAR"))  # this gives overall predictions for the model
# write.csv(pred_model_10, file = "datasets/pred_model_10.csv")

# Plot the predictions 
(lichen_cover_ANWR <- (ggplot(pred_model_10) + 
                     geom_line(aes(x = x, y = predicted)) +          # slope
                     geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                                 fill = "lightgrey", alpha = 0.5) +  # error band
                     geom_point(data = ITEX_lich_mean_trim,                      # adding the raw data 
                                aes(x = YEAR, y = mean_cover, colour = PLOT), size = 0.5) + 
                     labs(x = "\nYear", y = "\nLichen cover (%)", 
                          title = "Lichen cover (%) increase in the ANWR\n") + 
                      # scale_x_continuous(scale_x_continuous(breaks = 1996:2007))+ 
                     theme_shrub()))

# ggsave(file = "output/figures/lichen_cover_ANWR.png")
   
## Panel 
library(gridExtra)  # For making panels
library(ggpubr)  # For data visualisation formatting
(veg_panel <- grid.arrange(arrangeGrob(shrub_cover_ANWR, forb_cover_ANWR, 
                                       moss_cover_ANWR, lichen_cover_ANWR, 
                                       gram_cover_ANWR,nrow = 2)))

dev.off()

# 5. FUNCTIONAL GROUP ----

unique(ANWR_veg$FuncGroup)  # checking I have all functional groups

(hist_all_veg <- ANWR_veg %>%
      ggplot(aes(x = RelCover, fill = FuncGroup)) +
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
      geom_vline(aes(xintercept = mean(RelCover)),            
                 colour = "red", linetype = "dashed", size = 1) +
      labs(x = "\nCover (%)", y = "Frequency\n") +
      scale_fill_manual(values=c( "brown", "green4", "blue3", "yellow3", "red2"), name = "Functional group")+
      theme_shrub() +
      theme(legend.text = element_text(size=12),
            legend.title = element_text(size=15)) )

ggsave(file = "output/figures/hist_all_veg.png")

ANWR_veg$FuncGroup <- as.factor(as.character(ANWR_veg$FuncGroup))

ANWR_veg_fg <- ANWR_veg %>%
   group_by(SiteSubsitePlotYear, FuncGroup) %>%
   mutate(mean_cover = mean(RelCover)) %>%
   ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ANWR_veg_fg_trim <- ANWR_veg_fg %>% 
   dplyr::select(PLOT, YEAR, FuncGroup, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
   distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE)

ANWR_veg_fg_trim$FuncGroup <- as.factor(as.character(ANWR_veg_fg_trim$FuncGroup))
hist(ANWR_veg_fg_trim$mean_cover)

# Model 11 ----
# F.group fixed 
# mixed model with functional group as fixed effect
lmer_all <- glmer.nb(mean_cover~I(YEAR-1995)*FuncGroup + (1|YEAR) + (1|PLOT), data = ANWR_veg_fg_trim)
summary(lmer_all)
plot(lmer_all)
qqnorm(resid(lmer_all))
qqline(resid(lmer_all))  
dispersion_glmer(lmer_all)# 0.9356298

# Output table model 7 
stargazer(lmer_all,
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "", 
          type = "html", out = "output/tables/lmer_all.html")

# Extracting model predictions 
pred_lmer_all_1 <- ggpredict(lmer_all, terms = c("YEAR"))
pred_lmer_all_2 <- ggpredict(lmer_all, terms = c("FuncGroup"))  # this gives overall predictions for the model
# write.csv(pred_model_10, file = "datasets/pred_model_10.csv")

predics <- ggpredict(lmer_all, terms = c("YEAR", "FuncGroup"), type = "re")
(pred_plot2 <- ggplot(predics, aes(x = x, y = predicted, colour = group)) +
      stat_smooth(method = "lm", se = FALSE)  +
      scale_y_continuous(limits = c(0, 35)) +
      theme(legend.position = "bottom") +
      labs(x = "\nYear", y = "Cover\n"))

# trying diff graph
ANWR_veg_fg_trim$Predicted <- predict(lmer_all, ANWR_veg_fg_trim)

# plot predicted values
ggplot(ANWR_veg_fg_trim, aes(YEAR, Predicted)) +
   facet_wrap(~FuncGroup) +
   geom_point(aes(x = YEAR, y = mean_cover, colour= FuncGroup), size = .5) +
   geom_smooth(aes(y = Predicted, colour= FuncGroup), linetype = "solid", 
               se = T, method = "lm") +
   guides(color=guide_legend(override.aes=list(fill=NA))) +  
   theme_shrub() + 
   xlab("Year")


# extracting model predictions
pred.mm <- ggpredict(lmer_all_rand, terms = c("YEAR"))

# Plotting fixed effects
(fe.effects <- plot_model(lmer_all_rand, show.values = TRUE))

# Plotting random effects
(re.effects <- plot_model(lmer_all_rand, type = "re", show.values = TRUE))

# Random slopes 
predict <- ggpredict(lmer_all_rand, terms = c("YEAR", "FuncGroup"), type = "re") 

(pred_plot2 <- ggplot(predict, aes(x = x, y = predicted, colour = group)) +
      stat_smooth(method = "lm", se = FALSE)  +
      # scale_y_continuous(limits = c(0, )) +
      theme(legend.position = "bottom") +
      labs(x = "\nYear", y = "Predicted mean % cover\n"))
# all increasing? probably wrong


(scatter_fgroups <- (ggplot(ANWR_veg_fg_trim, aes(x = YEAR, y = mean_cover, colour = FuncGroup))+
                       geom_point(size = 0.5) +
                       geom_smooth(method = "lm") + 
                    facet_wrap(~FuncGroup, scales = "free_y") +
                       scale_x_continuous(breaks=c(1996, 1999, 2002,2005, 2007))+
                       scale_colour_manual(values = c("green4", "red", "red4", "brown", "blue4"))+
                       labs(y = "Mean vegetation cover\n", x = "\nYear") +
                       theme_shrub() +
                       theme(axis.text.x  = element_text(vjust=0.5, size=10, angle= 45, colour = "black"))))
            

ggsave(file = "output/figures/scatter_fgroups.png")

# Model with random slopes per f group
lmer_fgroup_rand <- lmer(mean_cover~I(YEAR-1995) + (1+I(YEAR-1995)|FuncGroup), data = ANWR_veg_fg_trim)
summary(lmer_fgroup_rand) # doesnt converge
head(ANWR_veg_fg_trim)

stargazer(lmer_fgroup_rand, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

predictions_fgroup_slopes <- ggpredict(lmer_fgroup_rand , terms = c("YEAR", "FuncGroup"), type = "re")

(fgroup_rand_slopes <- ggplot(predictions_fgroup_slopes, aes(x = x, y = predicted, colour = group)) +
      stat_smooth(method = "lm", se = FALSE)  +
      #scale_y_continuous(limits = c(0, 30)) +
     # scale_x_continuous(breaks=1997:2009)+
      theme(legend.position = "bottom") +
      labs(x = "\nYear", y = "Mean shrub genus cover (%)\n")+
      theme_shrub()+ 
      theme(axis.text.x = element_text(angle = 45)))

# END -----


##### COPY OF MARIANA's CLEANING SCRIPT ------
## Biodiversity dynamics across a warming tundra
## Mariana Garcia Criado
## Script 1. ITEX data cleaning - updated ITEX
## August 2021
## modified by Erica Zaja - 01/02/2022

### Data to be cleaned 
load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/ITEX_ALL.RData")
load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/full_itex_marianas_version_sept21.RData")
load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/perccov_all.RData")
load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/pfplot_all.RData")
load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/pfxy_all.RData")
load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/other_all.RData")
qhi_add <- read.csv("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/qhi_pf_fixed_forreal.csv")

## FUNCTIONS 
`%notin%` <- Negate(`%in%`)

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

# Check structure
glimpse(perccov_all)
glimpse(pfplot_all)
glimpse(pfxy_all)

# Add in QHI data
glimpse(qhi_add)
qhi_add2 <- dplyr::select(qhi_add, -c(X.1, X.2))

# Bind with the XY data
pfxy_all2 <- rbind(pfxy_all, qhi_add2)
glimpse(pfxy_all2)

## FILTERING 

# We need to clean up separately before converting to cover and then binding
# because the dataframes have different structures
unique(perccov_all$STATUS)
unique(pfplot_all$STATUS)
unique(pfxy_all2$STATUS)

unique(perccov_all$TREATMENT)
unique(pfplot_all$TREATMENT)
unique(pfxy_all2$TREATMENT)

unique(perccov_all$GFNARROWwalker)
unique(pfplot_all$GFNARROWwalker)
unique(pfxy_all2$GFNARROWwalker)

# Vector for target functional groups 
fg <- c("FORB", "SEVER", "SDECI", "SHRUBU", "SHRUB", "GRAMINOIDU", "GRASS", "SEDGE", "RUSH", "GRAMU", 
        "MOSSU", "LICHENU", "MACRO","MPLEU", "SPHAG", "BRYOPH", "LFRUT", "LCRUS", "LFOLI")

# Keep only control plots and alive vascular plants, create unique plotXyear ID
perccov_all2 <- perccov_all %>% filter(STATUS == "LIVE") %>% 
   filter(TREATMENT %in% c("CTL", "CONTROL")) %>% filter(GFNARROWwalker %in% fg) %>%
   unite(SiteSubsitePlotYear, c("SITE", "SUBSITE", "PLOT", "YEAR"), sep = ":", remove = FALSE) %>%
   unite(SiteSubsitePlot, c("SITE", "SUBSITE", "PLOT"), sep = ":", remove = FALSE) %>%
   unite(SiteSubsite, c("SITE", "SUBSITE"), sep = ":", remove = FALSE) %>%
   tidyr::replace_na(list(ABUNDANCE = 0)) %>% # STEPSTONES:TUNDRA1 and SADVENT:WET_PHOTO which are NA should be 0
   filter(SiteSubsite != "SADVENT:WET_PHOTO") # bad site I'm removing this here to avoid issues in the cover calc below

pfplot_all2 <- pfplot_all %>% filter(STATUS == "LIVE") %>% 
   filter(TREATMENT %in% c("CTL", "CONTROL")) %>% filter(GFNARROWwalker %in% fg) %>%
   unite(SiteSubsitePlotYear, c("SITE", "SUBSITE", "PLOT", "YEAR"), sep = ":", remove = FALSE) %>%
   unite(SiteSubsitePlot, c("SITE", "SUBSITE", "PLOT"), sep = ":", remove = FALSE) %>%
   unite(SiteSubsite, c("SITE", "SUBSITE"), sep = ":", remove = FALSE) %>% 
   dplyr::select(., -COVER_UNDERSTORY) %>% tidyr::replace_na(list(ABUNDANCE = 0)) # KANGERS should be 0 (investigated below)

# Investigate those with Abundance = NA
kanger.na <- pfplot_all2 %>% filter(SiteSubsite %in% c("KANGER:BASHFUL", "KANGER:DOPEY", "KANGER:SNEEZY"))
# When it's a 1 the info is in there, but there are no 0s. It's always the same species over the years so I think these are Abundance = 0

pfxy_all2b <- pfxy_all2 %>% filter(STATUS %in% c("LIVE", "Live", "Alive")) %>%
   filter(TREATMENT %in% c("CTL", "CONTROL")) %>% filter(GFNARROWwalker %in% fg) %>%
   unite(SiteSubsitePlotYear, c("SITE", "SUBSITE", "PLOT", "YEAR"), sep = ":", remove = FALSE) %>%
   unite(SiteSubsitePlot, c("SITE", "SUBSITE", "PLOT"), sep = ":", remove = FALSE) %>%
   unite(SiteSubsite, c("SITE", "SUBSITE"), sep = ":", remove = FALSE) %>%
   tidyr::replace_na(list(ABUNDANCE = 1)) #1 NA abundance value for QHI:HE only, should be 1 instead as no 0s are recorded


# There are some summed plots that don't belong in XY
summed <- pfxy_all2b %>% filter(HIT == "sum") %>% dplyr::select(., -c(X, Y, HIT))

# Remove from XY database
pfxy_all2c <- pfxy_all2b %>% filter(HIT != "sum" | is.na(HIT))

# add in to sum database
pfplot_all2b <- rbind(pfplot_all2, summed)


## COVER

# Do the cover values add up to 100?
cov <- perccov_all2 %>% filter(ValueType == "percent_cover") %>% 
   group_by(SiteSubsitePlotYear) %>% summarise(sum = sum(ABUNDANCE))
# Quite a lot of values over 100 so they need to be made proportional too so all values are comparable

#### Cover-equivalent

# Confirm that 1 row = 1 species
cov.test <- perccov_all2 %>% group_by(SiteSubsitePlotYear) %>% 
   mutate(NumberRows = n()) %>% mutate(NumberSpecies = length(unique(SPECIES_NAME))) %>%
   mutate(SameOrNot = ifelse(NumberRows == NumberSpecies, "Same", "Different")) %>% ungroup()

# Check if this is because of the missing species names or actually there are repeated species names
dif <- cov.test %>% filter(SameOrNot == "Different")

# Vector: these are sites that have empty species names so they are presumably different species and should not be summed
emp.vector <- c("KLUANE:PIKA:KluaneControl4:1996","KLUANE:PIKA:KluaneControl4:1997",
                "KLUANE:PIKA:KluaneControl4:1998", "KLUANE:PIKA:KluaneControl4:1999",
                "KLUANE:PIKA:KluaneControl7:1996", "KLUANE:PIKA:KluaneControl7:1997",
                "KLUANE:PIKA:KluaneControl7:1999", "SADVENT:MES_PHOTO:DCUA:2003",
                "SADVENT:MES_PHOTO:DDUA:2003", "SADVENT:MES_PHOTO:DEUA:2003", "SADVENT:MES_PHOTO:DFUA:2003")

# Add up values per species so we end up with only one row per species
dif2 <- dif %>% filter(SiteSubsitePlotYear %notin% emp.vector) %>% 
   filter(SiteSubsitePlotYear != "BARROW:CAREX_MOIST_MEADOW_MICROTOPO:BC02.5:1999") %>%
   group_by(SiteSubsitePlotYear, SPECIES_NAME) %>% mutate(AbundanceFixed = sum(ABUNDANCE)) %>% ungroup() %>%
   group_by(SiteSubsitePlotYear) %>% distinct(SPECIES_NAME, .keep_all = TRUE) %>% ungroup()

# One site has duplicate values: all records have exactly the same values twice
barrow.dup <- dif %>% filter(SiteSubsitePlotYear == "BARROW:CAREX_MOIST_MEADOW_MICROTOPO:BC02.5:1999") %>% 
   distinct(SPECIES_NAME, .keep_all = TRUE)

# Dataframes to merge:

# 1) Remove inconsistent plots from original dataset
perccov_all3 <- cov.test %>% filter(SameOrNot != "Different")

# 2) Kluane:Pika and Sadvent:Mes_Photo which have lots of empty species names, but they're probably different species
kl_sad <- dif %>% filter(SiteSubsitePlotYear %in% emp.vector)

# 3) Dataframe with added values
dif3 <- dif2 %>% mutate(ABUNDANCE = AbundanceFixed) %>% dplyr::select(., -AbundanceFixed)

# 4) Barrow with no duplicates (barrow.dup)

# Bind all three into one fixed cover dataset
perccov_fixed0 <- rbind(perccov_all3, kl_sad, dif3, barrow.dup)

# Keep relevant columns only
perccov_fixed <- perccov_fixed0 %>% dplyr::select(., -c(NumberRows, NumberSpecies, SameOrNot))


# Convert all values to relative cover
itex.cov <- perccov_fixed %>% group_by(SiteSubsitePlotYear) %>% 
   mutate(TotalAbundance = sum(ABUNDANCE)) %>%
   mutate(RelCover = (ABUNDANCE/TotalAbundance)*100) # 5733 obs

# Confirm that total cover values add up to 100 in every plotXyear
cov.check <- itex.cov %>% group_by(SiteSubsitePlotYear) %>% 
   mutate(TotalCover = sum(RelCover)) %>% 
   distinct(SiteSubsitePlotYear, .keep_all = TRUE)


#### Point-framing (summed) 

# Confirm that 1 row = 1 species
pfsum.test <- pfplot_all2b %>% group_by(SiteSubsitePlotYear) %>% 
   mutate(NumberRows = n()) %>% mutate(NumberSpecies = length(unique(SPECIES_NAME))) %>%
   mutate(SameOrNot = ifelse(NumberRows == NumberSpecies, "Same", "Different")) %>% ungroup()

# Check if this is because of the missing species names or actually there are repeated species names
dif.sum <- pfsum.test %>% filter(SameOrNot == "Different")

# Duplicates: exactly the same species and values on repeat
ab.vector <- c("ABISKO:PEATLAND:AA1:2000", "ABISKO:PEATLAND:AA1:2002", "ABISKO:PEATLAND:AA1:2004", "ABISKO:PEATLAND:AA1:2006", "ABISKO:PEATLAND:AA1:2008",
               "ABISKO:PEATLAND:AA2:2000", "ABISKO:PEATLAND:AA2:2002", "ABISKO:PEATLAND:AA2:2004", "ABISKO:PEATLAND:AA2:2006", "ABISKO:PEATLAND:AA2:2008",
               "ABISKO:PEATLAND:AA3:2000", "ABISKO:PEATLAND:AA3:2002", "ABISKO:PEATLAND:AA3:2004", "ABISKO:PEATLAND:AA3:2006", "ABISKO:PEATLAND:AA3:2008",
               "ABISKO:PEATLAND:AA4:2000", "ABISKO:PEATLAND:AA4:2002", "ABISKO:PEATLAND:AA4:2004", "ABISKO:PEATLAND:AA4:2006", "ABISKO:PEATLAND:AA4:2008",
               "ABISKO:PEATLAND:AA5:2000", "ABISKO:PEATLAND:AA5:2002", "ABISKO:PEATLAND:AA5:2004", "ABISKO:PEATLAND:AA5:2006", "ABISKO:PEATLAND:AA5:2008")

# Multiple plots have duplicate values: all records have exactly the same values twice
ab.dup <- dif.sum %>% filter(SiteSubsitePlotYear %in% ab.vector) %>% group_by(SiteSubsitePlotYear) %>%
   distinct(SPECIES_NAME, .keep_all = TRUE) %>% ungroup()

# Add up values per species so we end up with only one row per species
dif.sum2 <- dif.sum %>% filter(SiteSubsitePlotYear %notin% ab.vector) %>% 
   group_by(SiteSubsitePlotYear, SPECIES_NAME) %>% mutate(AbundanceFixed = sum(ABUNDANCE)) %>% ungroup() %>%
   group_by(SiteSubsitePlotYear) %>% distinct(SPECIES_NAME, .keep_all = TRUE) %>% ungroup()


# Dataframes to merge:

# 1) Remove inconsistent plots from original dataset
pfplot_all3 <- pfsum.test %>% filter(SameOrNot != "Different")

# 2) Dataframe with added values
dif.sum3 <- dif.sum2 %>% mutate(ABUNDANCE = AbundanceFixed) %>% dplyr::select(., -AbundanceFixed)

# 3) Abisko with no duplicates (ab.dup)

# Bind all three into one fixed cover dataset
pfsum_fixed0 <- rbind(pfplot_all3, dif.sum3, ab.dup)

# Keep relevant columns only
pfsum_fixed <- pfsum_fixed0 %>% dplyr::select(., -c(NumberRows, NumberSpecies, SameOrNot))


# Convert all values to relative cover
itex.pfsum <- pfsum_fixed %>% group_by(SiteSubsitePlotYear) %>% 
   mutate(TotalAbundance = sum(ABUNDANCE)) %>%
   mutate(RelCover = (ABUNDANCE/TotalAbundance)*100) # 15827 obs

# Confirm that total cover values add up to 100 in every plotXyear
pfsum.check <- itex.pfsum %>% group_by(SiteSubsitePlotYear) %>% 
   mutate(TotalCover = sum(RelCover)) %>% 
   distinct(SiteSubsitePlotYear, .keep_all = TRUE)


#### Point-framing (XY) 

# There are XY data that only have one coordinate - I'm filling in the NA cell so we avoid problems with cover calculation
na.x <- pfxy_all2c %>% filter(is.na(X)) # all filled in
na.y <- pfxy_all2c %>% filter(is.na(Y)) # 138814 obs

# Replace Y coords that are NA by 0s, create a unique coordinate
pfxy_all2d <- pfxy_all2c %>% tidyr::replace_na(list(Y = 0)) %>% 
   unite(XY, c("X", "Y"), sep = "_", remove = FALSE)

# STEP 1: Convert species abundance to presence/absence 
# (2D, not considering multiple hits of the same species at each xy coord, just 1)
pfxy_all_pa <- pfxy_all2d %>% 
   group_by(SiteSubsitePlotYear, XY) %>% distinct(SPECIES_NAME, .keep_all = TRUE) %>% 
   mutate(ABUNDANCE = ifelse(ABUNDANCE > 1, 1, ABUNDANCE))

# STEP 2: Calculate unique species hits per plot and total unique species hits per plot
pfxy_all_pa2 <- pfxy_all_pa %>% group_by(SiteSubsitePlotYear, SPECIES_NAME) %>% 
   mutate(UniqueSpHitsPlot = n()) %>% 
   distinct(SiteSubsitePlotYear, SPECIES_NAME, .keep_all = TRUE) %>% 
   ungroup() %>% dplyr::select(., -c(X, Y, XY, HIT)) %>% group_by(SiteSubsitePlotYear) %>%
   mutate(TotalUniqueSpHitsPlot = sum(UniqueSpHitsPlot)) %>% ungroup()

# STEP 3: Calculate cover per species
pfxy_all_cov <- pfxy_all_pa2 %>% mutate(RelCover = (UniqueSpHitsPlot/TotalUniqueSpHitsPlot)*100) #56797


# Confirm that total cover values add up to 100 in every plotXyear
pfxy.check <- pfxy_all_cov %>% group_by(SiteSubsitePlotYear) %>% 
   mutate(TotalCover = sum(RelCover)) %>% 
   distinct(SiteSubsitePlotYear, .keep_all = TRUE)


## BINDING 

# Keep same number of relevant columns
itex.cov.f <- itex.cov %>% dplyr::select(., -c(TotalAbundance, ABUNDANCE))
itex.pfsum.f <- itex.pfsum %>% dplyr::select(., -c(TotalAbundance, ABUNDANCE))
pfxy_all_cov.f <- pfxy_all_cov %>% dplyr::select(., -c(ABUNDANCE, UniqueSpHitsPlot, TotalUniqueSpHitsPlot))

# Bind all methods in one database
itex.all0 <- rbind(itex.cov.f, itex.pfsum.f, pfxy_all_cov.f) #78357

# replace NaN in cover by 0 (it's just 0/0) 
itex.all <- itex.all0 %>% mutate_at(vars(RelCover), ~replace(., is.nan(.), 0))


## SITE CHECKS 

# Remove everything in the southern hemisphere, Tibet and Mongolia out because they are more meadow than tundra
rmbl <- itex.all %>% filter(SITE == "RMBL")
sadv <- itex.all %>% filter(SITE == "SADVENT")

# Plots that had inconsistent surveyed areas or didn't identify species in the whole subsite
bad.subsites <- c("SADVENT:WET_PHOTO", "SADVENT:MES_PHOTO",
                  "ALEXFIORD:LEVDOLOMITE", "ALEXFIORD:LEVGRANITE", "SVERDRUP:SVERDRUP")

# Remove inconsistent sites
itex.all2 <- itex.all %>% filter(SiteSubsite %notin% bad.subsites) %>% filter(SITE != "TIBET") # 77969

# I have edited the metadata file manually so the subsites that didn't match up are similar now
# mainly just adding an extra site name to the subsite column so they link up
metadata0 <- read.csv("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/TVC_SITE_SUBSITE_UPDATED2020_edited.csv")

# Keep only relevant columns
metadata <- metadata0 %>% unite(SiteSubsite, c("SITE", "SUBSITE"), sep = ":", remove = FALSE) %>% 
   dplyr::select(SiteSubsite, COMMTYPE, LAT, LONG, ELEV, AZONE, SurveyedArea) %>% rename(MOISTURE = COMMTYPE)

# Merge with composition data
itex.full <- left_join(itex.all2, metadata, by = "SiteSubsite")

# Remove southern hemisphere plots and massive plot sizes
itex.full2 <- itex.full %>% filter(LAT > 0| is.na(LAT)) %>% 
   filter(SurveyedArea < 2.25) %>% 
   filter(SurveyedArea != "120") # 59101

# Check for missing NA
unique(itex.full2$MOISTURE)
unique(itex.full2$SurveyedArea)
unique(itex.full2$LAT)
unique(itex.full2$LONG)

# Which subsites didn't get metadata?
nomoist <- itex.full2 %>% filter(MOISTURE == "" | is.na(MOISTURE))
nom.abc <- sort(unique(nomoist$SiteSubsite))

# Missing moisture info:
#"AUYUITTUQ:OWL RIVER"         "BILLEFJORDEN:AWS"            "DISKO:DISTURBANCE"           "DISKO:DRYHEATH_FLUX"        
#"DISKO:WETFEN_FLUX"           "IGLOOLIK:IGLOOLIK"           "NIWOT:SADDLE_SHRUB_TUNDRA"   "NUUK:KOBBEFJORD"            
#"PYRAMIDEN:PYR"               "QUTTINIRPAAQ:TANQUARY_FIORD"

nolat <- itex.full2 %>% filter(is.na(LAT))
nolat.abc <- sort(unique(nolat$SiteSubsite))

nolong <- itex.full2 %>% filter(is.na(LONG))
nolong.abc <- sort(unique(nolong$SiteSubsite))

# Missing lat and long: "DISKO:DRYHEATH_FLUX" "DISKO:WETFEN_FLUX" 

nosize <- itex.full2 %>% filter(SurveyedArea == "" |is.na(SurveyedArea))
nosize.abc <- sort(unique(nosize$SiteSubsite))
# Missing surveyed area: "AUYUITTUQ:OWL RIVER" "BILLEFJORDEN:AWS"    "DISKO:DISTURBANCE"   "IGLOOLIK:IGLOOLIK"   "PYRAMIDEN:PYR" 

# I confirm that the missing info is not in the previous version of ITEX as these are new sites
# I am leaving DISKO with no coordinates as we know it's in Greenland. It will probably disappear from the models with latitude though.

## SPOT CHECKS 
hist(itex.full2$LAT) # makes sense
hist(itex.full2$LONG) # ok
unique(itex.full2$SiteSubsitePlot)
unique(itex.full2$SITE)
unique(itex.full2$RelCover)

# There is one Alexfiord "total" plot?
cas.all <- itex.full2 %>% filter(SiteSubsite == "ALEXFIORD:CASSIOPE_COVER")
# It's the only Cassiope plot there is so I don't think this is a summary of others + contains other species, so probably fine to leave it in.

# Niwot checks
niwot <- itex.full2 %>% filter(SiteSubsite == "NIWOT:WALKER_KLEIN")
unique(niwot$SiteSubsitePlot)

# these plots should not include the year 1993
klein.vector <- c("NIWOT:WALKER_KLEIN:75-4C", "NIWOT:WALKER_KLEIN:75-3C", "NIWOT:WALKER_KLEIN:75-5C", 
                  "NIWOT:WALKER_KLEIN:75-1C", "NIWOT:WALKER_KLEIN:75-2C")
klein <- filter(itex.full2, SiteSubsitePlot %in% klein.vector) # starts in 1995 so all good



## SPECIES NAMES 
spp <- unique(itex.full2$SPECIES_NAME)

# Identify empty species names
empty <- itex.full2 %>% filter(SPECIES_NAME == " ")

# Add morphospecies for empty cells - there are no NA cells
itex.full3 <- itex.full2 %>% 
   mutate(SPECIES_NAME = case_when(SPECIES_NAME == " " ~ paste0("XXX", GFNARROWwalker, ":", SITE), TRUE ~ SPECIES_NAME))

# Remove trailing white space so the same species are comparable
itex.full4 <- itex.full3 %>% mutate(SPECIES_NAME = str_trim(SPECIES_NAME))

# Check full species name again
unique(itex.full4$SPECIES_NAME)

# Standardise subspecies
itex.full4$SPECIES_NAME[itex.full4$SPECIES_NAME == "Ledum palustre subsp. groenlandicum"] <- "Ledum palustre"
itex.full4$SPECIES_NAME[itex.full4$SPECIES_NAME == "Tephroseris integrifolia subsp. atropurpurea"] <- "Tephroseris integrifolia"
itex.full4$SPECIES_NAME[itex.full4$SPECIES_NAME == "Silene uralensis subsp. apetala"] <- "Silene uralensis"
itex.full4$SPECIES_NAME[itex.full4$SPECIES_NAME == "Luzula spicata/confusa"] <- "XXXLUZULA"
itex.full4$SPECIES_NAME[itex.full4$SPECIES_NAME == "Eriophorum scheuchzeri/chamissonis"] <- "XXXERIOPHORUM"
itex.full4$SPECIES_NAME[itex.full4$SPECIES_NAME == "Cardamine bellidifolia subsp. alpina"] <- "Cardamine bellidifolia"
itex.full4$SPECIES_NAME[itex.full4$SPECIES_NAME == "Carex aquatilis var. minor"] <- "Carex aquatilis"
itex.full4$SPECIES_NAME[itex.full4$SPECIES_NAME == "Eriophorum angustifolium subsp. triste"] <- "Eriophorum angustifolium"
itex.full4$SPECIES_NAME[itex.full4$SPECIES_NAME == "Empetrum nigrum subsp. hermaphroditum"] <- "Empetrum nigrum"
itex.full4$SPECIES_NAME[itex.full4$SPECIES_NAME == "Dryas integrifolia x octopetala"] <- "XXXDRYAS"
itex.full4$SPECIES_NAME[itex.full4$SPECIES_NAME == "Salix arctica/arctophila"] <- "XXXSALIX"

# Convert genus/family species names to morphospecies
morp.vector <- scan(text = "Alchemilla
                 Anemone 
                 Antennaria 	
                 Arnica
                 Astragalus
                 Calamagrostis
                 Cardamine
                 Carex
                 Cyperaceae
                 Deschampsia
                 Draba
                 Dryas
                 Epilobium
                 Eriophorum
                 Festuca
                 Galium
                 Gentiana
                 Hedysarum
                 Hepatica
                 Luzula
                 Minuartia
                 Oxytropis
                 Pedicularis
                 Petasites
                 Poa	
                 Poa sp.
                 Poaceae
                 Polemonium
                 Polygonum
                 Salix
                 Saxifraga
                 Senecio
                 Stellaria
                 Tofieldia
                 Viola", what="")

# This function is AMAZING! Never adding quotes and commas manually again!
morp.vector2 <- c(morp.vector, "Juncus NA", "Minuartia NA", "Sagina NA")

# Transform into morphospecies
itex.full5 <- itex.full4 %>% 
   mutate(SPECIES_NAME = ifelse(SPECIES_NAME %in% morp.vector2, paste0("XXX", SPECIES_NAME, ":", SITE), SPECIES_NAME))

# All looks good now
spppp <- unique(itex.full5$SPECIES_NAME)


## GEO DATA 

# Specify Region
eurasia <- c("ABISKO", "BILLEFJORDEN", "DOVRE", "ENDALEN", "FINSE", "FURI", "KILPISJARVI", "LATNJA", "LOGH", "LORI", 
             "RIRI", "SADVENT", "JOATKA", "NAKKALA", "NYALESUND", "MALAYA", "TAISETSU", "GAVIA", "STILLBERG", "VALBERCLA",
             "PYRAMIDEN")
greenice <- c("KANGER", "ZACKENBERG", "DISKO", "AKUREYRI", "AUDKULUHEIDI", "BLONDUOS", "FAROE", "HJARDARLAND", 
              "HOLTAVORDUHEIDI", "MODRUVELLIR", "OXNADALSHEIDI", "THINGVELLIR", "THUFUVER", "THYKKVIBAER", "NUUK")
na.east <- c("ALEXFIORD", "TORNGATS", "BYLOT", "DALSMYNNI", "AUYUITTUQ", "IGLOOLIK", "QUTTINIRPAAQ")
na.west <- c("ANWR", "ATQASUK", "BARROW", "BROOKS", "DARING", "KLUANE", "NIWOT", "QHI", "TOOLIK", "WOLFCREEK")

# Add in categories
itex.full6 <- itex.full5 %>% 
   mutate(AlpArc = ifelse(LAT >= 66.4 & ELEV >= 1000, "Arctic-Alpine",
                          ifelse(LAT >= 66.4, "Arctic",
                                 ifelse(ELEV >= 1000, "Alpine", "Subarctic")))) %>% 
   mutate(lat_grid = plyr::round_any(LAT, 0.5, f = floor)) %>% 
   mutate(lon_grid = ifelse(LONG >0, plyr::round_any(LONG, 0.5, f = floor), 
                            plyr::round_any(LONG, 0.5, f = ceiling))) %>%
   mutate(gridcell = paste0("_", lat_grid, "_", lon_grid)) %>%
   mutate(Region = ifelse(SITE %in% eurasia, "Eurasia", 
                          ifelse(SITE %in% greenice, "GreenIceLand", 
                                 ifelse(SITE %in% na.east, "North America-East",
                                        ifelse(SITE %in% na.west, "North America-West", NA)))))



## MORPHOSPECIES
length(unique(itex.full6$SiteSubsitePlotYear)) # 6859 plotXyear in total

# Check the unidentified species names
xxxtest <- itex.full6 %>% 
   filter(str_detect(SPECIES_NAME, 'XXX|xxx')) %>% 
   group_by(SiteSubsitePlotYear) %>% 
   mutate(MorphoCover = sum(RelCover)) %>%
   ungroup() %>% dplyr::select(SiteSubsitePlotYear, MorphoCover) %>% 
   distinct(., .keep_all = TRUE)

# 2170 unique plotXyear that contain morphospecies
unique(xxxtest$SiteSubsitePlotYear) 

# Check how many plots would be removed with different cut-offs
morpho10 <- xxxtest %>% filter(MorphoCover > 10) # 10% morphospecies cutoff would remove 912 plotXyear (13.2% of database)
morpho15 <- xxxtest %>% filter(MorphoCover > 15) # 15% morphospecies cutoff would remove 671 plotXyear (9.7% of database)
morpho20 <- xxxtest %>% filter(MorphoCover > 20) # 20% morphospecies cutoff would remove 524 plotXyear (7.6% of database)
morpho25 <- xxxtest %>% filter(MorphoCover > 25) # 20% morphospecies cutoff would remove 404 plotXyear (5.8% of database)

# Mean is around 15% morphospecies cover in plotXyears
(xxxmorph <- ggplot(xxxtest, aes(x=MorphoCover)) + 
      geom_histogram(binwidth = 0.5) + 
      geom_vline(aes(xintercept = mean(MorphoCover)), colour = "red", linetype = "dashed", size = 1) +
      xlab("Cover of morphospecies in plots") + bio.theme)

# Extract as vector from the main dataset
morpho25.vec <- unique(morpho25$SiteSubsitePlotYear)

# Remove those plots
itex.full7 <- itex.full6 %>% filter(SiteSubsitePlotYear %notin% morpho25.vec)


## FG-DOMINATION

# Add % cover per functional group & specify dominant FG
itex.full8 <- itex.full7 %>% 
   mutate(ValueType = case_when(ValueType == "pf_all_xy" ~ "pf_all_XY", 
                                ValueType == "pf_topbot_xy" ~ "pf_topbot_XY", 
                                TRUE ~ ValueType)) %>%
   mutate(Method = case_when(ValueType %in% c("BraunBlanquet", "percent_cover") ~ "Cover",
                             ValueType %in% c("pf_all_plot", "pf_top_plot", "pf_topbot_plot") ~ "Point-framing (sum)",
                             ValueType %in% c("pf_all_XY", "pf_top_XY", "pf_topbot_XY") ~ "Point-framing (XY)")) %>%
   mutate(FuncGroup = case_when(GFNARROWwalker %in% c("SEVER", "SDECI") ~ "Shrub",
                                GFNARROWwalker == "FORB" ~ "Forb",
                                GFNARROWwalker %in% c("RUSH", "SEDGE", "GRASS", "GRAMINOIDU", "GRAMU") ~ "Graminoid",
                                GFNARROWwalker %in% c("MACRO", "MPLEU", "MOSSU", "SPHAG", "BRYOPH") ~ "Moss",
                                GFNARROWwalker %in% c("LCRUS", "LFOLI", "LICHENU", "LFRUT") ~ "Lichen")) %>%
   group_by(SiteSubsitePlotYear, FuncGroup) %>% 
   mutate(FuncPlotCover = sum(RelCover)) %>% ungroup() %>%
   mutate(DominatingFG = case_when(FuncPlotCover > 50 ~ paste0(FuncGroup, "-Dominated"), TRUE ~ "None")) # we need this column for homogeneization

# Check plot-dominated functional groups - this leaves a row per siteXyear but not necessarily the dominating one
dom <- itex.full8 %>% distinct(SiteSubsitePlotYear, .keep_all = TRUE) %>% 
   dplyr::select(SITE, SUBSITE, SiteSubsitePlot, SiteSubsitePlotYear, FuncGroup, FuncPlotCover, DominatingFG)

# Long format: include all covers values per functional group 
dom.fg <- itex.full8 %>% distinct(SiteSubsitePlotYear, FuncGroup, .keep_all = TRUE) %>% 
   dplyr::select(SiteSubsitePlotYear, FuncGroup, FuncPlotCover) %>%
   pivot_wider(names_from = FuncGroup, values_from = FuncPlotCover, values_fill = list(FuncPlotCover = 0)) %>%
   mutate(DominatingFG = case_when(Shrub > 50 ~ "Shrub-Dominated",
                                   Graminoid > 50 ~ "Graminoid-Dominated",
                                   Forb > 50 ~ "Forb-Dominated",
                                   Moss > 50 ~ "Moss-Dominated",
                                   Lichen > 50 ~ "Lichen-Dominated",
                                   TRUE ~ "None")) %>%
   rename(ShrubCover = Shrub) %>% rename(GraminoidCover = Graminoid) %>% 
   rename(ForbCover = Forb) %>% rename(LichenCover = Lichen) %>% rename(MossCover = Moss) %>%
   rename(PlotDominatingFG = DominatingFG)

# Join with full dataset
itex.full9 <- left_join(itex.full8, dom.fg, by = "SiteSubsitePlotYear")


# Calculate mean cover per plot over time (more representative of FG cover over time)
avg.fg <- itex.full9 %>% distinct(SiteSubsitePlotYear, .keep_all = TRUE) %>% 
   group_by(SiteSubsitePlot) %>% mutate(ShrubMean = mean(ShrubCover)) %>% 
   mutate(ForbMean = mean(ForbCover)) %>% mutate(GraminoidMean = mean(GraminoidCover)) %>%
   mutate(MossMean = mean(MossCover)) %>% mutate(LichenMean = mean(LichenCover)) %>%
   distinct(SiteSubsitePlot, .keep_all = TRUE) %>% ungroup() %>%
   dplyr::select(SiteSubsitePlot, ShrubMean, ForbMean, GraminoidMean, LichenMean, MossMean)

# Join with main dataset
itex.full10 <- left_join(itex.full9, avg.fg, by = "SiteSubsitePlot")

# Remove all those rows with cover = 0 since otherwise it might impact the richness calculation
itex.full100 <- itex.full10 %>% filter(RelCover > 0)

# Calculate average richness over time and round to nearest integer
mean.rich <- itex.full100 %>% group_by(SiteSubsitePlotYear) %>% 
   mutate(AnnualRichness = length(unique(SPECIES_NAME))) %>% ungroup() %>%
   distinct(SiteSubsitePlotYear, .keep_all = TRUE) %>% 
   group_by(SiteSubsitePlot) %>% 
   mutate(MeanRichness = round(mean(AnnualRichness))) %>% 
   ungroup() %>% distinct(SiteSubsitePlotYear, .keep_all = TRUE) %>% 
   dplyr::select(SiteSubsitePlotYear, AnnualRichness, MeanRichness)

# Join with main dataset
ITEX_EZ_diss <- left_join(itex.full100, mean.rich, by = "SiteSubsitePlotYear")

# Save full dataset
save(ITEX_EZ_diss, file = "~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/ITEX_EZ_diss.RData") #52890



