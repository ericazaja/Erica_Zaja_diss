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
ITEX_shrubs_mean_trim$PLOT <- as.factor(as.character(ITEX_shrubs_mean_trim$PLOT))
hist(ITEX_shrubs_mean_trim$mean_cover)

# Mean shrub cover change over time  
(shrub_mean_change <- (ggplot(ITEX_shrubs_mean_trim)+
                     geom_point(aes(x = YEAR, y = mean_cover), colour = "skyblue", size = 1) +
                     geom_smooth(aes(x = YEAR, y = mean_cover), colour= "black", method = "lm") + 
                     scale_x_continuous(breaks=1997:2009)+
                     labs(y = "Mean shrub % cover\n", x = "\nYear") + 
                    annotate(geom = "text", x = 2007, y = 50, label="(a)", size = 10) +
                     theme_shrub()+
                     theme(axis.text.x = element_text(angle = 45))))

ggsave(shrub_mean_change, file = "output/figures/shrub_mean_change.png")          

# Model 6----
# Shrub cover over time
lm_shrub <- lm(mean_cover~YEAR, data = ITEX_shrubs_mean_trim)
summary(lm_shrub) # not significant 
# F-statistic: 1.175 on 1 and 143 DF,  p-value: 0.2802

# Transform percentage cover to proportion (dividing by 100)
ITEX_shrubs_mean_trim <- ITEX_shrubs_mean_trim %>% mutate(cover_prop = mean_cover/100)
  hist(ITEX_shrubs_mean_trim$mean_cover)
# mixed effect model with plot and year as random effects
model_6 <- lmer(cover_prop~YEAR + (1|PLOT)+ (1|YEAR), data = ITEX_shrubs_mean_trim)
summary(model_6)

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
  mutate(tot_cover = sum(FuncPlotCover)) %>%
  ungroup()


# Shrinking the dataframe to retain one row per plot etc.
ITEX_shrubs_tot_trim <- ITEX_shrubs_tot  %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, tot_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, tot_cover, .keep_all = TRUE)

ITEX_shrubs_tot_trim$PLOT <- as.factor(as.character(ITEX_shrubs_tot_trim$PLOT))
hist(ITEX_shrubs_tot_trim$tot_cover) # Wrong because goes more than 100

# Tot shrub cover change over time  
(shrub_scatter_sum <- (ggplot(ITEX_shrubs_tot_trim)+
                         geom_point(aes(x = YEAR, y = tot_cover), size = 2) +
                         geom_smooth(aes(x = YEAR, y = tot_cover), method = "lm") + 
                         labs(y = "Total shrub % cover\n", x = "\nYear") + 
                         theme_shrub())) # not similar trend to mean but not sig


lm_shrub_tot <- lm(tot_cover~YEAR, data = ITEX_shrubs_tot_trim)
summary(lm_shrub_tot) # not sig: F-statistic: 0.02088 on 1 and 143 DF,  p-value: 0.8853

# Trying brms (to use family = beta)
bcpriors <- get_prior(cover_prop~YEAR +  (1|PLOT) + (1|YEAR), data=ITEX_shrubs_mean_trim, family="beta")

stmt.fitc <- brm(cover_prop~YEAR +  (1|PLOT) + (1|YEAR), data=ITEX_shrubs_mean_trim, family="beta",
                 prior = bcpriors) # this doesnt work

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
  distinct(SiteSubsitePlotYear, genus_cover, .keep_all = TRUE) 

ITEX_shrubs_sp_trim$GENUS <- as.factor(as.character(ITEX_shrubs_sp_trim$GENUS ))
hist(ITEX_shrubs_sp_trim$genus_cover)
ITEX_shrubs_sp_trim$YEAR<- as.numeric(ITEX_shrubs_sp_trim$YEAR)
str(ITEX_shrubs_sp_trim)

(facet_scatter_shrub_genus <- (ggplot(ITEX_shrubs_sp_trim, aes(x = YEAR, y = genus_cover, colour = GENUS))+
                                 geom_point(size = 0.6) +
                                 geom_smooth(method = "lm") + 
                                 facet_wrap(~ GENUS, scales = "free_y") +
                                 scale_x_continuous(breaks=1997:2009)+
                                 labs(y = "Mean cover (%) \n", x = "\nYear") +
                                 theme_shrub()+
                                 theme(axis.text.x  = element_text(vjust=0.5, size=10, angle= 45, colour = "black"))))

dev.off()
ggsave(file = "output/figures/facet_scatter_shrub_genus.png")

# Model ----

# mixed effect model with year as random effects
lmer_shrub_sp <- lmer(genus_cover~YEAR*GENUS + (1|YEAR), data = ITEX_shrubs_sp_trim)
summary(lmer_shrub_sp)

print(lmer_shrub_sp, correlation=TRUE)
stargazer(lmer_shrub_sp, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
pred_model_shrub_sp <- ggpredict(lmer_shrub_sp, terms = c("YEAR", "GENUS"))  # this gives overall predictions for the model
# write.csv(pred_model_9, file = "datasets/pred_model_9.csv")
pred_model_shrub_sp_1 <- ggpredict(lmer_shrub_sp, terms = "YEAR")
pred_model_shrub_sp_2 <- ggpredict(lmer_shrub_sp, terms = "GENUS")
str(ITEX_shrub_sp)

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
lmer_shrub_sp_rand <- lmer(genus_cover~YEAR + (1+YEAR|GENUS) + (1|YEAR), data = ITEX_shrubs_sp_trim)
summary(lmer_shrub_sp_rand )

predictions_rs_ri <- ggpredict(lmer_shrub_sp_rand , terms = c("YEAR", "GENUS"), type = "re")
(genus_rand_slopes <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    scale_y_continuous(limits = c(0, 30)) +
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
salix_model <- lmer(genus_cover ~ YEAR + (1|YEAR), data = Salix)
summary(salix_model) # not sig
(salix_plot <- ggplot(Salix, aes(x = YEAR, y = genus_cover)) +
   geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n")) 

# Dryas sp.
Dryas <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Dryas") 
dryas_model <- lmer(genus_cover ~ YEAR + (1|YEAR), data = Dryas)
summary(dryas_model)# not sig
(dryas_plot <- ggplot(Dryas, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# Vaccinium sp.
Vaccinium <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Vaccinium") 
vacc_model <- lmer(genus_cover ~ YEAR + (1|YEAR), data = Vaccinium)
summary(vacc_model)# not sig
(vacc_plot <- ggplot(Vaccinium, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# Arctostaphylos sp.
Arctostaphylos <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Arctostaphylos") 
arcto_model <- lmer(genus_cover ~ YEAR + (1|YEAR), data = Arctostaphylos)
summary(vacc_model)# not sig
(arcto_plot <- ggplot(Arctostaphylos, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# Betula sp.
Betula <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Betula") 
betula_model <- lmer(genus_cover ~ YEAR + (1|YEAR), data = Betula)
summary(betula_model)# not sig
(betula_plot <- ggplot(Betula, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# Cassiope sp.
Cassiope<-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Cassiope") 
cassiope_model <- lmer(genus_cover ~ YEAR + (1|YEAR), data = Cassiope)
summary(cassiope_model)# not sig
(cassiope_plot <- ggplot(Cassiope, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# Ledum sp.
Ledum <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Ledum") 
ledum_model <- lmer(genus_cover ~ YEAR + (1|YEAR), data = Ledum)
summary(ledum_model)# not sig
(ledum_plot <- ggplot(Ledum, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# Vary genus name to visualise distributions
hist(Dryas$genus_cover)



# 3. SHRUB COVER IN SPACE  ----

# Shrub cover vs latitude 
shrub_lat <- lm(mean_cover ~ LAT, data = ITEX_shrubs_mean_trim)
summary(shrub_lat)

# F-statistic: 55.18 on 1 and 143 DF,  p-value: 9.125e-12***
# mean shrub cover decreases with lat

stargazer(shrub_lat, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

(cover_lat_scatter <- (ggplot(ITEX_shrubs_mean_trim))+
    geom_point(aes(x = LAT, y = mean_cover), colour= "skyblue", size = 2) +
    geom_smooth(aes(x = LAT, y = mean_cover),colour = "black", method = "lm") + 
    labs(y = "Mean shrub % cover\n", x = "\nLatitude") +
    annotate(geom = "text", x = 69.7, y = 60, label="(a)", size = 10) +
    annotate(geom = "text", x = 69.25, y = 20, label="slope = -7.214*** ", size = 6) +
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

# Shrub cover vs longitude
shrub_long<- lm(mean_cover ~ LONG, data = ITEX_shrubs_mean_trim)
summary(shrub_long)
# F-statistic: 55.18 on 1 and 143 DF,  p-value: 9.123e-12***


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
    annotate(geom = "text", x = -144, y = 60, label="(b)", size = 10) +
    annotate(geom = "text", x = -146, y = 20, label="slope = -1.563*** ", size = 6) +
    theme_shrub())


ggsave(file = "output/figures/cover_long_scatter.png")

# Extracting model predictions 
pred_shrub_lon <- ggpredict(shrub_long, terms = c("lon_grid"))  # this gives overall predictions for the model
# write.csv(pred_model_10, file = "datasets/pred_model_10.csv")

# Plot the predictions 
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



(panel_cover_latlong <- grid.arrange(arrangeGrob(cover_lat_scatter,cover_long_scatter,
                                           ncol = 2)) )# Sets number of panel columns

ggsave(panel_cover_latlong, file = "output/figures/panel_cover_latlong.png", height = 10, width = 20)
           

