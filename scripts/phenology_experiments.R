##%######################################################%##
#                                                          #
####         PHENOLOGY  EXPERIMENTS -----               ####
#               Erica Zaja - 22/10/2021                   ##
#                                                         #
##%######################################################%##

# Loading libraries -----
library(readr)

# Loading data -----
phenology_data <- read_csv("datasets/phenology_data/CCIN13215_20210302_tundra_phenology_database.csv")

# Data wrangling -----
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
shrub.theme <- theme(legend.position = "right",
                     axis.title.x = element_text(face="bold", size=20),
                     axis.text.x  = element_text(vjust=0.5, size=18, colour = "black"), 
                     axis.title.y = element_text(face="bold", size=20),
                     axis.text.y  = element_text(vjust=0.5, size=18, colour = "black"),
                     panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                     panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                     plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                     plot.margin = unit(c(1,1,1,1), units = , "cm"))

# Plot DOY on x and phenophase on y
(greening <- (ggplot(phenology_new, aes(x = DOY, y = phenophase))+
                     geom_point(size = 2) +
                     geom_smooth(method = "lm") + 
                     labs(y = "Phenophase", x = "\nDay of Year") + 
                     shrub.theme))

## I need to compare onset of greening (DOY) across the years




