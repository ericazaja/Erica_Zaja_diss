##%######################################################%##
#                                                          #
####                 RQ4: PHENOLOGY                     ####
#               Erica Zaja - 05/02/2022                   ##
#                                                         #
##%######################################################%##

# RQ: How has shrub phenology (greening) changed over time near the PCH range?

# LOADING LIBRARIES -----
library(tidyverse)

# LOADING DATA  -----
phenology_data <- read_csv("datasets/phenology_data/CCIN13215_20210302_tundra_phenology_database.csv")

# DATA EXPLORATION and WRANGLING  -----
range(phenology_data$year)# 1992-2019

unique(phenology_data$study_area) # Unique site names

# Retaining only locations on Alaskan north slope or close to PCH range
phenology_new <- phenology_data %>%
  filter(study_area %in% c("Atqasuk", "Toolik Lake","Qikiqtaruk", "Utqiagvik"))
# Keeping Toolik lake and Qikiqtaruk (close enough to PCH summer range). 
# and  Atqasuk and Utqiaġvik that are on the North slope of Alaska
# NB the largest total number of phenology observations came from Utqiaġvik, Alaska
# with 60,434 observations of phenological events of 48 species over 26 years in control
# and experimentally warmed plots

unique(phenology_new$study_area) # Unique site names

unique(phenology_new$functional_group) # Unique functional group names
# [1] "evergreen shrub" "deciduous shrub" "graminoid"       "forb"  

# keeping shrubs only 
phenology_new <- phenology_new %>%
  filter(functional_group %in% c("evergreen shrub", "deciduous shrub"))

unique(phenology_new$genus) # unique shrub genus names
#  [1] "Cassiope"       "Diapensia"      "Ledum"          "Vaccinium"      "Salix"          "Betula"        
# [7] "Dryas"          "Arctostaphylos" "Loiseleuria"    "Andromeda"   

unique(phenology_new$phenophase) # Unique phenophase names
# "green"     "flower"    "flowerend" "seedmat"   "senesce"   "Flower"    "Flowerend"
# "Green"     "SeedMat"   "Senesce"  

# Standardising names
phenology_new$phenophase[phenology_new$phenophase == "Green"] <- "green"
phenology_new$phenophase[phenology_new$phenophase == "Flower"] <- "flower"
phenology_new$phenophase[phenology_new$phenophase == "Flowerend"] <- "flowerend"
phenology_new$phenophase[phenology_new$phenophase == "Senesce"] <- "senesce"
phenology_new$phenophase[phenology_new$phenophase == "SeedMat"] <- "seedmat"
unique(phenology_new$phenophase) # new unique phenophase names
# [1] "green"     "flower"    "flowerend" "seedmat"   "senesce"  


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

# Plot DOY on x and phenophase on y
(phenophases <- (ggplot(phenology_new, aes(x = DOY, y = phenophase))+
                    geom_boxplot(size = 0.5) +
                    labs(y = "Phenophase\n", x = "\nDay of Year") +                    
                   theme_shrub()))
# reorder levels

# ggsave(file = "output/figures/phenophases.png")

# filter for greening only
phenology_green <- phenology_new %>%
  filter(phenophase == "green")

unique(phenology_green$phenophase) # only greening

(greening <- (ggplot(phenology_green, aes(x = phenophase, y = DOY))+
                 geom_boxplot(size = 0.5) +
                 labs(x = "\nOnset of shrub greening", y = "Day of Year\n") +                  theme_shrub()))
 
# ggsave(file = "output/figures/greening.png")
unique(phenology_green$year)

phenology_green$plot <- as.factor(as.character(phenology_green$plot))

# EARLY VS LATE GREENING -----

# Classifying early vs late greening plots
range(phenology_green$DOY) # range of DOY of onset of greening

# 135 (earliest greening DOY) 211 (latest greening DOY)
# 211-135 = 76 days difference
# 76/2= 38
# 135+38 = 173 midpoint
# greening < 173 DOY --> early greening year
# greening > 173 DOY --> late greening year

# BUT checking the number of plots per year 
phenology_green_98 <- phenology_green %>% filter(year == "1998") # 451 obs
phenology_green_99 <- phenology_green %>% filter(year == "1999") # 431
phenology_green_00<- phenology_green %>% filter(year == "2000") # 450
# NOT Same number of observations eachn year
# There are different numeber of total plots every year
# SO need to calculate proportion of plots greening early each year

# checking range of DOY each year
range(phenology_green_98$DOY)
range(phenology_green_99$DOY)
range(phenology_green_00$DOY)
# different ranges every year so need to find mean DOY 

# Create a new version of phenology_green with unique plot identifiers
phenology_green_id <- phenology_green %>% 
  mutate(SiteSubsitePlot = paste(study_area, ":", subsite, ":", plot)) %>% 
  mutate(SiteSubsitePlotYear = paste(study_area, ":", subsite, ":", plot, ":", year))

# How may unique plot and year combos
unique_plot_year <- unique(phenology_green_id$SiteSubsitePlotYear) # 2980
  
# Group the dataframe by year to see the number of plots per year
phenology_plots <- phenology_green_id %>%
  group_by(year) %>%
  summarise(plot.n = length(unique(SiteSubsitePlot))) %>% 
  ungroup()

# Calculating the mean DOY
phenology_mean_doy <- phenology_green_id %>% 
  group_by(SiteSubsitePlotYear) %>% 
  mutate(mean.doy = mean(DOY)) %>% 
  ungroup()

hist(phenology_mean_doy$mean.doy) # looks normal 

# Shrinking the dataframe to retain one row per plot etc.
phenology_green_trim <- phenology_mean_doy %>% 
  dplyr::select(study_area, subsite, plot, year, SiteSubsitePlotYear, SiteSubsitePlot,
                lat, long, elevation, ecosystem, exstart, soil_moisture, treatment, mean.doy) %>% 
  distinct(SiteSubsitePlotYear, mean.doy, .keep_all = TRUE) # 2980 rows, perfect!


# defining a threshold based on mean of all ranges 
threshold <- phenology_green_trim %>% group_by(SiteSubsitePlotYear) %>% 
  summarise(mean = mean(mean.doy))

mean(threshold$mean) # 168.2861 threshold of early VS late greening
median(threshold$mean)

# Classify as early or late plots
phenology_green_class <- phenology_green_trim %>% 
  mutate(greening_type = ifelse(mean.doy >= 168, "late", "early"))

# late vs early phenology year as factor
phenology_green_class$greening_type <- as.factor(phenology_green_class$greening_type)


# COUNT -----

# Count the number of plots of early and late type per year
count_years <- phenology_green_class %>% 
  group_by(year, greening_type) %>% 
  summarise(total = length(unique(SiteSubsitePlot))) %>% 
  ungroup()


# CHECKS ----

# Convert to wide format to do some checks
count_years_wide <- count_years %>% 
  pivot_wider(names_from = greening_type, values_from = total) %>% 
  mutate(early = ifelse(is.na(early), 0, early),
         late = ifelse(is.na(late), 0, late)) %>% # Replace NAs with 0
  mutate(total_plots = early + late)

# Now join to your count of plots per year to see if identical
count_years_check <- left_join(count_years_wide, phenology_plots, by = c("year" = "year"))


# PROPORTIONS ----

# Calculate the proportion of plots as early
prop_greening_plots <- count_years_wide %>% 
  mutate(prop_early = early / total_plots,
         prop_late = late / total_plots) %>% 
  mutate(prop_total = prop_early + prop_late) # Just checking = 1

hist(prop_greening_plots$prop_early)# not normal
hist(prop_greening_plots$prop_late)# not normal 


# DATA VISUALISATION ----
# 1. EARLY GREENING  -----
(early_greening_plots <- ggplot(prop_greening_plots, aes(x = year, y = prop_early)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm")+
    labs(x = "Year\n", y = "Early greening plots (prop)\n",
         title = "Proportion of early greening plots increasing\n") +
    theme_shrub())

ggsave(file = "output/figures/early_greening_plots.png")
# need to add subsite?

# Model ----
lm_early <- lm(prop_early~ year, data = prop_greening_plots) 
summary(lm_early) # not sig
# F-statistic: 1.064 on 1 and 24 DF,  p-value: 0.3126

# Generalised linear model family binomial 
glm_early <- glm(prop_early ~ year, family = binomial, data = prop_greening_plots)
summary(glm_early)


# 2. LATE GREENING -----
(late_greening_plots <- ggplot(prop_greening_plots, aes(x = year, y = prop_late)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm")+
    labs(x = "Year\n", y = "Late greening plots (prop)\n",
         title = "Proportion of late greening plots decreasing\n") +
    theme_shrub())

ggsave(file = "output/figures/late_greening_plots.png")

# Model ----
# simple lm
lm_late <- lm(prop_late~ year, data = prop_greening_plots) 
summary(lm_late) # not sig
# F-statistic: 1.064 on 1 and 24 DF,  p-value: 0.3126

# Generalised linear model family binomial 
glm_late <- glm(prop_late ~ year, family = binomial, data = prop_greening_plots)
summary(glm_late)

phenology_green_trim$study_area<- as.factor(as.character(phenology_green_trim$study_area))
phenology_green_trim$SiteSubsitePlotYear<- as.factor(as.character(phenology_green_trim$SiteSubsitePlotYear))
str(phenology_green_trim)

# Linear mixed model ----
# lmer with study_area as random effect 
lmer_green <- lmer(mean.doy ~ year + (1|study_area), data = phenology_green_trim ) 
summary(lmer_green)
stargazer(lmer_green, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extract

# Extracting model predictions 
pred_lmer_green <- ggpredict(lmer_green, terms = c("year", "study_area"), type = "re")  # this gives overall predictions for the model

write.csv(pred_lmer_green, file = "datasets/pred_model_10.csv")

# Plot the predictions 
(greening_model <- (ggplot(pred_lmer_green) + 
                         geom_line(aes(x = x, y = predicted)) +          # slope
                         geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                                     fill = "lightgrey", alpha = 0.5) +  # error band
                         geom_point(data = phenology_green_trim,                      # adding the raw data 
                                    aes(x = year, y = mean.doy, colour = study_area), size = 0.5) + 
                         labs(x = "\nYear", y = "\nMean greening DOY", 
                              title = "Mean greening DOY not changing\n") + 
                         theme_shrub()))

# ggsave(file = "output/figures/greening_model.png")

# I might want random slopes/intercepts?
(slopes_pred_lmer_green <- ggplot(pred_lmer_green, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    theme(legend.position = "bottom")+
    labs( x= "Year", y = "mean DOY"))

ggsave(file = "outputs/figures/slopes_pred_lmer_green.png")

(years_count <- ggplot(prop_greening_plots) +
    geom_bar(aes(x = year, y = prop, colour = greening_type, fill= greening_type),
             stat = "identity", binwidth = 3) +
    labs(x = "greening type (count)", y = "proportion") +
    theme_shrub())

# TO DO 
# lmer(count_no_early_years ~ years + (1 | SUBSITE))
## Year (x) VS DOY of greening (y) —> negative trned 
# lmer(DOY ~ YEAR  + (1|subsite)) 