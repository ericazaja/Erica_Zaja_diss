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