##%######################################################%##
#                                                          #
####         ITEX VEG COVER EXPERIMENTS -----             ##
#               Erica Zaja - 22/10/2021                   ##
#                                                         #
##%######################################################%##

# Loading data ----
load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/ITEX_ALL.RData")
load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/full_itex_marianas_version_sept21.RData")

# Data wrangling ----
## Make sure I retain mosses and lichens, forbs, shrubs, graminoids in Arctic national wildlife refuge (ANWR)
## NB the mosses and lichens might be not well recorded, check for ANWR


## eg code to clean out species from https://ourcodingclub.github.io/tutorials/model-design/ 
# toolik_plants <- toolik_plants %>%
#  filter(!Species %in% c("Woody cover", "Tube",
#                         "Hole", "Vole trail",
#                         "removed", "vole turds",
#                         "Mushrooms", "Water",
#                        "Caribou poop", "Rocks",
#                         "mushroom", "caribou poop",
#                        "animal litter", "vole poop",
#                         "Vole poop", "Unk?"))

