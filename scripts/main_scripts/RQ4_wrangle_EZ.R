##%######################################################%##
#                                                          #
####                 RQ4: PHENOLOGY                     ####
#               Erica Zaja - 05/02/2022                   ##
#                                                         #
##%######################################################%##

# RQ: How has shrub phenology (greening) changed over time near the PCH range?
# Credit for help: Joseph Everest 

# LOADING LIBRARIES -----
library(tidyverse)

# LOADING DATA  -----
phenology_data <- read_csv("datasets/phenology_data/CCIN13215_20210302_tundra_phenology_database.csv")

# DATA EXPLORATION and WRANGLING  -----
range(phenology_data$year)# 1992-2019

unique(phenology_data$year) # Unique site names

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

# filter for greening only
phenology_green <- phenology_new %>%
  filter(phenophase == "green")

hist(phenology_green$DOY)

unique(phenology_green$phenophase) # only greening

(greening <- (ggplot(phenology_green, aes(x = phenophase, y = DOY))+
                geom_boxplot(size = 0.5) +
                labs(x = "\nOnset of shrub greening", y = "Day of Year\n") + theme_shrub()))

# ggsave(file = "output/figures/greening.png")
unique(phenology_green$year)

# making plot as factor
phenology_green$plot <- as.factor(as.character(phenology_green$plot))

# EARLY VS LATE GREENING -----
# loading new dataset
phenology_green <- read_csv("datasets/phenology_data/phenology_green.csv")

# Classifying early vs late greening plots
range(phenology_green$DOY) # range of DOY of onset of greening
quantile(phenology_green$DOY) 
# 0%  25%  50%  75% 100% 
# 135  162  166  170  211 

# Checking the number of plots per year 
phenology_green_98 <- phenology_green %>% filter(year == "1998") # 451 obs
phenology_green_99 <- phenology_green %>% filter(year == "1999") # 431
phenology_green_00<- phenology_green %>% filter(year == "2000") # 450
# NOT Same number of observations each year
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

mean(phenology_green_id$DOY)
mean(phenology_mean_doy$mean.doy) # 166.5352

# HISTOGRAM of greening DOY ----
(doy_hist <- phenology_green_id %>%
   ggplot(aes(x = DOY)) +
   geom_histogram( color="black", fill="#009E73", alpha=0.6, position = 'identity', bins = 60) +
   geom_vline(aes(xintercept = mean(DOY)),            
              colour = "black", linetype = "dashed", size = 1) +
   ylab("Frequency\n") +
   xlab(bquote("\n Day of year of shrub green-up"))+ 
   theme_shrub()+
   theme(axis.title.x =element_text(size=25),
         axis.title.y =element_text(size=25),
         axis.text.x = element_text(size=25, hjust = 1),
         axis.text.y = element_text(size=25, hjust = 1),
         legend.text = element_text(size=20),
         legend.title = element_text(size=25),
         legend.position = "bottom"))

ggsave(file = "output/figures/doy_hist.png")

# Shrinking the dataframe to retain one row per plot etc.
phenology_green_trim <- phenology_mean_doy %>% 
  dplyr::select(study_area, subsite, plot, year, SiteSubsitePlotYear, SiteSubsitePlot,
                lat, long, elevation, ecosystem, exstart, soil_moisture, treatment, mean.doy) %>% 
  distinct(SiteSubsitePlotYear, mean.doy, .keep_all = TRUE) %>% # 2980 rows, perfect!
  mutate(year_index = case_when (year == 1994 ~ '1', year == 1995 ~ '2', 
                                 year == 1996 ~ '3', year == 1997 ~ '4',
                                 year == 1998 ~ '5', year== 1999 ~ '6', 
                                 year == 2000 ~ '7', year== 2001 ~ '8',
                                 year== 2002 ~ '9', year == 2003 ~ '10',
                                 year== 2004 ~ '11', year == 2005 ~ '12',
                                 year == 2006 ~ '13', year == 2007 ~ '14',
                                 year == 2008 ~ '15', year == 2009 ~ '16',
                                 year == 2010 ~ '17',year == 2011 ~ '18',
                                 year == 2012 ~ '19',year == 2013~ '20',
                                 year == 2014 ~ '21',year == 2015 ~ '22',
                                 year == 2016 ~ '23',year == 2017 ~ '24',
                                 year == 2018~ '25',year == 2019 ~ '26')
  ) 

write.csv(phenology_green_trim, file = "datasets/phenology_data/phenology_green_trim.csv")

# defining a threshold based on mean of all ranges 
threshold <- phenology_green_trim %>% group_by(SiteSubsitePlotYear) %>% 
  summarise(mean = mean(mean.doy))

mean(threshold$mean) # 168.2861 threshold of early VS late greening
median(threshold$mean)

# Classify as early or late plots
phenology_green_class <- phenology_green_trim %>% 
  mutate(greening_type = ifelse(mean.doy >= 166, "late", "early")) # using 50% quantile as threshold

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

unique(prop_greening_plots$year)

prop_greening_plots <- prop_greening_plots %>% 
  mutate(prop_early_int = round(prop_early)) %>%  
  mutate(prop_late_int = round(prop_late)) %>%   
  mutate(year_index = case_when (year == 1994 ~ '1', year == 1995 ~ '2', 
                                 year == 1996 ~ '3', year == 1997 ~ '4',
                                 year == 1998 ~ '5', year== 1999 ~ '6', 
                                 year == 2000 ~ '7', year== 2001 ~ '8',
                                 year== 2002 ~ '9', year == 2003 ~ '10',
                                 year== 2004 ~ '11', year == 2005 ~ '12',
                                 year == 2006 ~ '13', year == 2007 ~ '14',
                                 year == 2008 ~ '15', year == 2009 ~ '16',
                                 year == 2010 ~ '17',year == 2011 ~ '18',
                                 year == 2012 ~ '19',year == 2013~ '20',
                                 year == 2014 ~ '21',year == 2015 ~ '22',
                                 year == 2016 ~ '23',year == 2017 ~ '24',
                                 year == 2018~ '25',year == 2019 ~ '26')
         ) 

hist(prop_greening_plots$prop_early_int)# not normal
hist(prop_greening_plots$prop_late_int)# not normal 

write.csv(prop_greening_plots, "datasets/phenology_data/prop_greening_plots.csv")

#################################################################### END -----
