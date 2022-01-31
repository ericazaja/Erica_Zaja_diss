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
min(phenology_data$year)
max(phenology_data$year)

unique(phenology_data$study_area) # Unique site names

# Retaining only locations on Alaskan north slope or close to PCH range
phenology_new <- phenology_data %>%
  filter(study_area %in% c("Atqasuk", "Toolik Lake","Qikiqtaruk", "Utqiagvik"))

unique(phenology_new$study_area) # Unique site names
unique(phenology_new$functional_group) # Unique site names
unique(phenology_new$phenophase) # Unique site names


