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


ITEX_all_veg$FuncGroup <- as.factor(as.character(ITEX_all_veg$FuncGroup))


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
  distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE)

str(ITEX_shrubs_mean_trim)
ITEX_shrubs$PLOT <- as.factor(as.character(ITEX_shrubs$PLOT))
hist(ITEX_shrubs_mean_trim$mean_cover)

# Mean shrub cover change over time  
(shrub_scatter <- (ggplot(ITEX_shrubs_mean_trim)+
                     geom_point(aes(x = YEAR, y = mean_cover, colour = PLOT), size = 2) +
                     geom_smooth(aes(x = YEAR, y = mean_cover), method = "lm") + 
                     labs(y = "Mean shrub % cover\n", x = "\nYear") + 
                     theme_shrub()))


# Model 6----
# Shrub cover over time
lm_shrub <- lm(mean_cover~YEAR, data = ITEX_shrubs_mean_trim)
summary(lm_shrub)
# F-statistic:  5.54 on 1 and 517 DF,  p-value: 0.01896

# mixed effect model with plot and year as random effects
model_6 <- glmer(mean_cover~YEAR + (1|PLOT)+ (1|YEAR), family = beta(), data = ITEX_shrubs_mean_trim)
summary(model_6)

# total variance: 17.20 + 13.67   =30.87
# variance for plot =   17.20
# amount of variance explained by random effect:  17.20 /30.87 = 0.5571753= ~56%
# I.e. differences between plots explain ~56% of the variance 
# that’s “left over” after the variance explained by our fixed effect (year).
# estimate for precip (exp variable =   0.154***    ) i.e. year positively impacts shrub cover
# significant effect of year on shrub cover  = 0.154***    

# Checking model 6 assumptions 
plot(model_6)
qqnorm(resid(model_6))
qqline(resid(model_6))  # points fall nicely onto the line - good!

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
                        geom_line(aes(x = x, y = predicted, group=group)) +          # slope
                        # geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                        # fill = "lightgrey", alpha = 0.5) +  # error band
                        geom_point(data = ITEX_shrubs_mean_trim,                      # adding the raw data 
                                   aes(x = YEAR, y = mean_cover, colour = PLOT), size = 0.5) + 
                        labs(x = "\nYear", y = "Shrub cover (%)\n", 
                             title = "Shrub % cover increase in the ANWR\n") + 
                        theme_shrub()
))


ggsave(file = "output/figures/shrub_cover_ANWR.png")

# checking that overall shrub cover change has same trend as mean cover change
# Total shrub cover 
ITEX_shrubs_tot <- ITEX_shrubs %>%
  group_by(SiteSubsitePlotYear) %>%
  mutate(tot_cover = sum(RelCover)) %>%
  ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ITEX_shrubs_tot_trim <- ITEX_shrubs_tot  %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, tot_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, tot_cover, .keep_all = TRUE)

ITEX_shrubs_tot_trim$PLOT <- as.factor(as.character(ITEX_shrubs_tot_trim$PLOT))
hist(ITEX_shrubs_tot_trim$tot_cover)

# Tot shrub cover change over time  
(shrub_scatter_sum <- (ggplot(ITEX_shrubs_tot_trim)+
                         geom_point(aes(x = YEAR, y = tot_cover, colour = PLOT), size = 2) +
                         geom_smooth(aes(x = YEAR, y = tot_cover), method = "lm") + 
                         labs(y = "Total shrub % cover\n", x = "\nYear") + 
                         theme_shrub())) # similar trend to mean


lm_shrub_tot <- lm(tot_cover~YEAR, data = ITEX_shrubs_tot_trim)
summary(lm_shrub_tot) # not sig: F-statistic: 0.02088 on 1 and 143 DF,  p-value: 0.8853

### SHRUB GENUS -----
# shrub species
unique(ITEX_shrubs$GENUS)

# Mean shrub cover per plot per year
ITEX_shrub_sp <- ITEX_shrubs %>%
  group_by(SiteSubsitePlotYear, GENUS) %>%
  mutate(genus_cover = sum(RelCover)) %>%
  ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ITEX_shrubs_sp_trim <- ITEX_shrub_sp  %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, GENUS, genus_cover) %>% 
  distinct(SiteSubsitePlotYear, genus_cover, .keep_all = TRUE) 

ITEX_shrubs_sp_trim$GENUS <- as.factor(as.character(ITEX_shrubs_sp_trim$GENUS ))
hist(ITEX_shrubs_sp_trim$genus_cover)

(facet_scatter_shrub_genus <- (ggplot(ITEX_shrubs_sp_trim, aes(x = YEAR, y = genus_cover, colour = GENUS))+
                                 geom_point(size = 2) +
                                 geom_smooth(method = "lm") + 
                                 facet_wrap(~ GENUS, scales = "free_y") +
                                 labs(y = "Mean cover (%) \n", x = "\nYear") +
                                 theme_shrub()))
dev.off()
ggsave(file = "output/figures/facet_scatter_shrub_genus.png")

# Model ----

# mixed effect model with plot and year as random effects
lmer_shrub_sp <- lmer(genus_cover~YEAR*GENUS + (1|YEAR), data = ITEX_shrubs_sp_trim)
summary(lmer_shrub_sp)

stargazer(lmer_shrub_sp, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
pred_model_shrub_sp <- ggpredict(lmer_shrub_sp, terms = c("YEAR", "GENUS"), ci = 0.95)  # this gives overall predictions for the model
# write.csv(pred_model_9, file = "datasets/pred_model_9.csv")
pred_model_shrub_sp_1 <- ggpredict(lmer_shrub_sp, terms = "YEAR")
pred_model_shrub_sp_2 <- ggpredict(lmer_shrub_sp, terms = "GENUS")
str(ITEX_shrub_sp)

# Plot the predictions 
(plot_model_shrub_sp <- (ggplot(pred_model_shrub_sp) + 
                           geom_line(aes(x = x, y = predicted, colour = group) +          # slope
                                       # geom_ribbon(aes(ymin = predicted - std.error, ymax = predicted + std.error), 
                                       # fill = "lightgrey", alpha = 0.5) +  # error band
                                       geom_point(data = ITEX_shrub_sp, aes(x = YEAR, y = Mean_cover), size = 0.5) + 
                                       # facet_wrap(~GENUS) +
                                       labs(x = "Year", y = "Shrub species cover (%)", 
                                            title = "Shrub species cover (%) in the ANWR") + 
                                       theme_shrub())))
# wrong

# trying diff graph
ITEX_shrub_sp$Predicted <- predict(lmer_shrub_sp, ITEX_shrub_sp)

# plot predicted values
ggplot(ITEX_shrub_sp, aes(YEAR, Predicted)) +
  facet_wrap(~GENUS) +
  geom_point(aes(x = YEAR, y = Mean_cover, colour= GENUS), size = .5) +
  geom_smooth(aes(y = Predicted, colour= GENUS), linetype = "solid", 
              se = T, method = "lm") +
  guides(color=guide_legend(override.aes=list(fill=NA))) +  
  theme_shrub() + 
  xlab("Year")


# Plotting fixed effects
(fe.effects <- plot_model(lmer_shrub_sp , show.values = TRUE))

# Plotting random effects
(re.effects <- plot_model(lmer_shrub_sp , type = "re", show.values = TRUE))

# Random slopes 
predict_sp <- ggpredict(lmer_shrub_sp , terms = c("YEAR", "GENUS"), type = "re") 

(pred_plot2 <- ggplot(predict_sp, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    # scale_y_continuous(limits = c(0, )) +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Predicted mean % cover\n"))
# all increasing?


# SHRUB cover VS LAT ----
shrub_lat <- lm(mean_cover ~ LAT, data = ITEX_shrubs_mean_trim)
summary(shrub_lat)
# F-statistic: 55.18 on 1 and 143 DF,  p-value: 9.125e-12***
# mean shrub cover decreases with lat

stargazer(shrub_lat, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

(cover_lat_scatter <- (ggplot(ITEX_shrubs_mean_trim))+
    geom_point(aes(x = LAT, y = mean_cover, colour = PLOT), size = 2) +
    geom_smooth(aes(x = LAT, y = mean_cover), method = "lm") + 
    labs(y = "Mean shrub % cover\n", x = "\nLatitude") +
    theme_shrub())

ggsave(file = "output/figures/cover_lat_scatter.png")


# Extracting model predictions 
pred_shrub_lat <- ggpredict(shrub_lat, terms = c("LAT"))  # this gives overall predictions for the model
# write.csv(pred_model_10, file = "datasets/pred_model_10.csv")

# Plot the predictions 
(plot_model_shrub_lat <- (ggplot(pred_shrub_lat) + 
                            geom_line(aes(x = x, y = predicted)) +          # slope
                            geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                                        fill = "lightgrey", alpha = 0.5) +  # error band
                            geom_point(data = ITEX_shrubs_mean_trim,                      # adding the raw data 
                                       aes(x = LAT, y = mean_cover), size = 0.5) + 
                            labs(x = "Latitude", y = "Shrub cover (%)", 
                                 title = "") + 
                            # scale_x_continuous(scale_x_continuous(breaks = 1996:2007))+ 
                            theme_shrub()))

ggsave(file = "output/figures/plot_model_shrub_lat.png")

# SHRUB cover VS LONG ----
shrub_long<- lm(mean_cover ~ LONG, data = ITEX_shrubs_mean_trim)
summary(shrub_long)


stargazer(shrub_long, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
# F-statistic: 55.18 on 1 and 143 DF,  p-value: 9.123e-12***
# mean shrub cover decreases with longitude

(cover_long_scatter <- (ggplot(ITEX_shrubs_mean_trim))+
    geom_point(aes(x = LONG, y = mean_cover, colour = PLOT), size = 2) +
    geom_smooth(aes(x = LONG, y = mean_cover), method = "lm") + 
    labs(y = "Mean shrub % cover\n", x = "\nLongitude") +
    theme_shrub())

ggsave(file = "output/figures/cover_long_scatter.png")

# Extracting model predictions 
pred_shrub_lon <- ggpredict(shrub_long, terms = c("lon_grid"))  # this gives overall predictions for the model
# write.csv(pred_model_10, file = "datasets/pred_model_10.csv")

=# Plot the predictions 
  (plot_model_shrub_lon <- (ggplot(pred_shrub_lon) + 
                              geom_line(aes(x = x, y = predicted)) +          # slope
                              geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                                          fill = "lightgrey", alpha = 0.5) +  # error band
                              geom_point(data = ITEX_shrubs_mean_trim,                      # adding the raw data 
                                         aes(x = lon_grid, y = mean_cover), size = 0.5) + 
                              labs(x = "Longitude", y = "Shrub cover (%)", 
                                   title = "") + 
                              # scale_x_continuous(scale_x_continuous(breaks = 1996:2007))+ 
                              theme_shrub()))

ggsave(file = "output/figures/plot_model_shrub_lon.png")

# Model ----

# F.group fixed ----
# mixed model with functional group as fixed effect
lmer_all <- lmer(Mean_cover~YEAR + FuncGroup + (1|YEAR) + (1|PLOT), data = ITEX_all_veg)
summary(lmer_all)

# Output table model 7 
stargazer(lmer_all, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

predslm <-  predict(lmer_all , interval = "confidence")
head(predslm)

datlm <- cbind(ITEX_all_veg, predslm)
head(datlm)

### ****SHRUB GENUS****-----
# shrub species
unique(ITEX_shrubs$GENUS)

# Mean shrub cover per plot per year
ITEX_shrub_sp <- ITEX_shrubs %>%
  group_by(SiteSubsitePlotYear, GENUS) %>%
  mutate(genus_cover = sum(RelCover)) %>%
  ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ITEX_shrubs_sp_trim <- ITEX_shrub_sp  %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, GENUS, genus_cover) %>% 
  distinct(SiteSubsitePlotYear, genus_cover, .keep_all = TRUE) 

hist(ITEX_shrubs_sp_trim$genus_cover)

ITEX_shrub_sp$GENUS <- as.factor(as.character(ITEX_shrub_sp$GENUS ))

(facet_scatter_shrub_genus <- (ggplot(ITEX_shrubs_sp_trim , aes(x = YEAR, y = genus_cover, colour = GENUS))+
                                 geom_point(size = 2) +
                                 geom_smooth(method = "lm") + 
                                 facet_wrap(~ GENUS, scales = "free_y") +
                                 labs(y = "Relative cover \n", x = "\nYear") +
                                 theme_shrub()))


dev.off()
ggsave(file = "output/figures/facet_scatter_shrub_genus.png")

# Model ----

# mixed effect model with plot and year as random effects
lmer_shrub_sp <- lmer(genus_cover~YEAR + GENUS + (1|YEAR), data = ITEX_shrub_sp_trim)
summary(lmer_shrub_sp)

str(ITEX_shrub_sp)

stargazer(lmer_shrub_sp, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") # some sig

newdat.lme = data.frame(GENUS = ITEX_shrub_sp$GENUS, 
                        YEAR = ITEX_shrub_sp$YEAR) 

newdat.lme$predlme = predict(lmer_shrub_sp, newdata = newdat.lme, level = 0)

ggplot(ITEX_shrub_sp, aes(x = YEAR, y = Mean_cover, color = GENUS) ) +
  geom_rug(sides = "b", size = 1) +
  geom_line(data = newdat.lme, aes(x=YEAR, y = predlme, colour=GENUS), size = 1) 
##lines squiggly???

