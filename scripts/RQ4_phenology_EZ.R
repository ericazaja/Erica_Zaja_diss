##%######################################################%##
#                                                          #
####                 RQ4: PHENOLOGY                     ####
#               Erica Zaja - 05/02/2022                   ##
#                                                         #
##%######################################################%##

# LOADING LIBRARIES -----
library(readr)

# LOADING DATA  -----
phenology_data <- read_csv("datasets/phenology_data/CCIN13215_20210302_tundra_phenology_database.csv")

# DATA WRANGLING  -----

# Keep Toolik lake and Qikiqtaruk (close enough to PCH summer range) 
# and perhaps Atqasuk, Utqiaġvik that are on the North slope of Alaska
# NB the largest total number of phenology observations came from Utqiaġvik, Alaska
# with 60,434 observations of phenological events of 48 species over 26 years in control
# and experimentally warmed plots

# I need DOI, phenophase of shrubs (Salix?) 

# Data exploration
range(phenology_data$year)
# 1992-2019
unique(phenology_data$study_area) # Unique site names

# Retaining only locations on Alaskan north slope or close to PCH range
phenology_new <- phenology_data %>%
  filter(study_area %in% c("Atqasuk", "Toolik Lake","Qikiqtaruk", "Utqiagvik"))

unique(phenology_new$study_area) # Unique site names
unique(phenology_new$functional_group) # Unique functional group names
# [1] "evergreen shrub" "deciduous shrub" "graminoid"       "forb"           

phenology_new <- phenology_new %>%
  filter(functional_group %in% c("evergreen shrub", "deciduous shrub"))

unique(phenology_new$phenophase) # Unique phenophase names
# "green"     "flower"    "flowerend" "seedmat"   "senesce"   "Flower"    "Flowerend"
# "Green"     "SeedMat"   "Senesce"  
# Standardising names
phenology_new$phenophase[phenology_new$phenophase == "Green"] <- "green"
phenology_new$phenophase[phenology_new$phenophase == "Flower"] <- "flower"
phenology_new$phenophase[phenology_new$phenophase == "Flowerend"] <- "flowerend"
phenology_new$phenophase[phenology_new$phenophase == "Senesce"] <- "senesce"
phenology_new$phenophase[phenology_new$phenophase == "SeedMat"] <- "seedmat"

unique(phenology_new$phenophase) # Unique phenophase names
# [1] "green"     "flower"    "flowerend" "seedmat"   "senesce"  

# Theme 
theme_shrub <- theme(legend.position = "right",
                     axis.title.x = element_text(face="bold", size=20),
                     axis.text.x  = element_text(vjust=0.5, size=18, colour = "black"), 
                     axis.title.y = element_text(face="bold", size=20),
                     axis.text.y  = element_text(vjust=0.5, size=18, colour = "black"),
                     panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                     panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                     plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                     plot.margin = unit(c(1,1,1,1), units = , "cm"))

# DATA VISUALISATION -----
# Plot DOY on x and phenophase on y
(phenophases <- (ggplot(phenology_new, aes(x = DOY, y = phenophase))+
                     geom_point(size = 2) +
                     geom_smooth(method = "lm") + 
                     labs(y = "Phenophase", x = "\nDay of Year") + 
                    theme_shrub))

## I need to compare onset of greening (DOY) across the years
# filter for greening only
phenology_green <- phenology_new %>%
  filter(phenophase == "green")

unique(phenology_green$phenophase) # only green

(greening <- (ggplot(phenology_green, aes(x = DOY, y = phenophase))+
                   geom_point(size = 2) +
                   geom_smooth(method = "lm") + 
                   labs(y = "Onset of greening", x = "\nDay of Year") + 
                   theme_shrub))


unique(phenology_green$year)
phenology_green_98 <- phenology_green %>% filter(year == "1998") # 451 obs
phenology_green_99 <- phenology_green %>% filter(year == "1999") # 431
phenology_green_00<- phenology_green %>% filter(year == "2000") # 450
# NOT Same number of observations eachn year

# Need to calculate proportion of plots greening early 

# Classifying early vs late greening years -----

range(phenology_green$DOY) # range of DOY of onset of greening
# 135 (earliest greening DOY) 211 (latest greening DOY)
# 76 days difference
# > 211-135 = 76
# 76/2= 38
# 135+38 = 173 midpoint

# greening < 173 DOY --> early greening year
# greening > 173 DOY --> late greening year

phenology_green <- phenology_green %>%
  mutate(year_type = case_when(DOY >= 173 ~ 'late' , # late year
                                DOY < 173 ~ 'early')) # early year

# write.csv(phenology_green, file = "datasets/phenology_data/phenology_green.csv")

# late vs early phenology year as factor
phenology_green$year_type <- as.factor(as.character(phenology_green$year_type))

count_years <- phenology_green %>% group_by(year) %>% count(year_type)

# EARLY YEARS -----
count_years_early <- count_years %>% group_by(year) %>% filter(year_type=="early")

(erly_years_count <- ggplot(count_years_early, aes(x = year, y = n)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm")+
    theme_minimal())

# INCREASE in number (count) of early greening years 
# need to add subsite?
lm_early <- lm(n ~ year, data =count_years_early) 
summary(lm_early) # sig
# F-statistic: 6.113 on 1 and 23 DF,  p-value: 0.02125

# LATE YEARS ----
count_years_late <- count_years %>% group_by(year) %>% filter(year_type=="late")

(late_years_count <- ggplot(count_years_late, aes(x = year, y = n)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm")+
    theme_minimal())

# INCREASE in number (count) of late greening years 

lm_late <- lm(n ~ year, data =count_years_late) 
summary(lm_late) # not sig
# F-statistic: 3.151 on 1 and 22 DF,  p-value: 0.08971


(boxplot_green <- ggplot(phenology_green, aes(x = year, y = mean_onset_greening, fill = late_early)) +
    geom_boxplot() +
    theme_minimal()) # more early greening in later years!

str(phenology_green)


# TO DO -----
# NB check I have same number of points per year? —> if not proportion of plots greening early. 
# Count of number of early years
# Barplot of count of number of early years (x = year [1998-2020], y = count of number of early years)
# lmer(count_no_early_years ~ years + (1 | SUBSITE))
## Year (x) VS DOY of greening (y) —> negative trned 
# lmer(DOY ~ YEAR  + (1|subsite)) 

##################################
# Other ignore 
# calculating mean onset of greening day per year
phenology_green <- phenology_green %>%
  group_by(year) %>%
  mutate(mean_onset_greening = mean(DOY)) %>%
  ungroup()


