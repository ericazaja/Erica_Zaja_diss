##%######################################################%##
#                                                          #
####                 RQ4: PHENOLOGY                     ####
#               Erica Zaja - 05/02/2022                   ##
#                                                         #
##%######################################################%##

# RQ: How has shrub phenology (greening) changed over time near the PCH range?

# LOADING LIBRARIES -----
library(readr)

# LOADING DATA  -----
phenology_data <- read_csv("datasets/phenology_data/CCIN13215_20210302_tundra_phenology_database.csv")

# DATA EXPLORATION and WRANGLING  -----
range(phenology_data$year)
# 1992-2019
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

# ggsave(file = "output/figures/phenophases.png")

# I want to compare onset of greening (DOY) over the years
# filter for greening only
phenology_green <- phenology_new %>%
  filter(phenophase == "green")

unique(phenology_green$phenophase) # only greening

(greening <- (ggplot(phenology_green, aes(x = phenophase, y = DOY))+
                   geom_boxplot(size = 0.5) +
                   labs(x = "\nOnset of shrub greening", y = "Day of Year\n") + 
                   theme_shrub()))

# ggsave(file = "output/figures/greening.png")


unique(phenology_green$year)
phenology_green$plot <- as.factor(as.character(phenology_green$plot))

# EARLY VS LATE GREENING -----
# Classifying early vs late greening years -----
# Need to calculate proportion of plots greening early 
range(phenology_green$DOY) # range of DOY of onset of greening
# 135 (earliest greening DOY) 211 (latest greening DOY)
# # 211-135 = 76 days difference
# 76/2= 38
# 135+38 = 173 midpoint

# greening < 173 DOY --> early greening year
# greening > 173 DOY --> late greening year

# BUT checking the number of plots per year 
# Group the dataframe by year to see the number of plots per year
phenology_plots <- phenology_green %>% group_by(year) %>%
summarise(plot.n = length(unique(plot))) 
# There are different numeber of total plots every year

str(phenology_green)
phenology_green_98 <- phenology_green %>% filter(year == "1998") # 451 obs
phenology_green_99 <- phenology_green %>% filter(year == "1999") # 431
phenology_green_00<- phenology_green %>% filter(year == "2000") # 450
# NOT Same number of observations eachn year

range(phenology_green_98$DOY)
range(phenology_green_99$DOY)
range(phenology_green_00$DOY)
# different ranges every year

# defining a threshold based on mean of all ranges 
threshold <- phenology_green %>% group_by(year)%>% summarise(min_DOY=min(DOY), max_DOY=(max(DOY)), 
                                                             diff = max_DOY - min_DOY, 
                                                             divide = diff/2, mid_point = min_DOY+divide)
 
  
mean(threshold$mid_point) # 172.0385 threshold of early VS late greening
                                                    
# classifyin each plot in early VS late
phenology_green <- phenology_green %>%
  group_by(year, plot) %>%
  mutate(greening_type = case_when(DOY >= 172 ~ 'late' , # late greening
                                DOY < 172 ~ 'early')) # early greening


# write.csv(phenology_green, file = "datasets/phenology_data/phenology_green.csv")
# phenology_green <- read.csv("datasets/phenology_data/phenology_green.csv")

# late vs early phenology year as factor
phenology_green$greening_type <- as.factor(as.character(phenology_green$greening_type))

# EARLY vs LATE YEARS -----
count_years <- phenology_green %>% group_by(year) %>% count(greening_type) #right!
count_years_new <- left_join(count_years, phenology_plots) # join with phenology_plots
prop_greening_plots <- count_years_new %>% mutate(prop = n/plot.n)

str(prop_greening_plots)
prop_greening_plots$greening_type <- as.factor(as.character(prop_greening_plots$greening_type))

# DATA VISUALISATION ----
# 1. EARLY GREENING  -----
prop_years_early <- prop_greening_plots %>% group_by(year) %>% filter(greening_type=="early")

(early_greening_plots <- ggplot(prop_years_early, aes(x = year, y = prop)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm")+
    labs(x = "Year\n", y = "Early greening plots (prop)\n",
         title = "Proportion of early greening plots increasing\n") +
    theme_shrub())

#ggsave(file = "output/figures/early_greening_plots.png")

# need to add subsite?
lm_early <- lm(prop ~ year, data = prop_years_early) 
summary(lm_early) # not sig
# F-statistic: 3.375 on 1 and 23 DF,  p-value: 0.07916

# 2. LATE GREENING -----
prop_years_late <- prop_greening_plots %>% group_by(year) %>% filter(greening_type=="late")

(late_greening_plots <- ggplot(prop_years_late, aes(x = year, y = prop)) +
    geom_point(size = 0.1) +
    geom_smooth(method = "lm")+
    labs(x = "Year\n", y = "Late greening plots (prop)\n",
               title = "Proportion of late greening plots decreasing\n") +
    theme_shrub())

# ggsave(file = "output/figures/late_greening_plots.png")

lm_late <- lm(prop ~ year, data = prop_years_late ) 
summary(lm_late) # not sig
# F-statistic: 0.09805 on 1 and 22 DF,  p-value: 0.7571


(boxplot_green <- ggplot(phenology_green, aes(x = year, y = mean_onset_greening, fill = late_early)) +
    geom_boxplot() +
    theme_minimal()) # more early greening in later years!

str(phenology_green)

(years_count <- ggplot(prop_greening_plots) +
    geom_bar(aes(x = year, y = prop, colour = greening_type, fill= greening_type),
             stat = "identity", binwidth = 3) +
    labs(x = "greening type (count)", y = "proportion") +
    theme_shrub())

# TO DO -----
# NB check I have same number of points per year? —> if not proportion of plots greening early. 
# Count of number of early years
# Barplot of count of number of early years (x = year [1998-2020], y = count of number of early years)
# lmer(count_no_early_years ~ years + (1 | SUBSITE))
## Year (x) VS DOY of greening (y) —> negative trned 
# lmer(DOY ~ YEAR  + (1|subsite)) 


