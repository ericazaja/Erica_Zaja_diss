##%######################################################%##
#                                                          #
####         ITEX VEG COVER EXPERIMENTS -----             ###
#               Erica Zaja - 22/10/2021                   ##
#                                                         #
##%######################################################%##

# Loading data ----
load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/ITEX_ALL.RData")
load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/full_itex_marianas_version_sept21.RData")

# Data wrangling ----
## Make sure I retain mosses and lichens, forbs, shrubs, graminoids in Arctic national wildlife refuge (ANWR)
## NB the mosses and lichens might be not well recorded, check for ANWR