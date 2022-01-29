##%######################################################%##
#                                                          #
####         ITEX VEG COVER EXPERIMENTS -----             ##
#               Erica Zaja - 22/10/2021                   ##
#                                                         #
##%######################################################%##

# Loading libraries ----
library(tidyverse)

# Loading data ----
load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/ITEX_ALL.RData")
load("~/Desktop/dissertation/R_dissertation/datasets/ITEX_data/full_itex_marianas_version_sept21.RData")

# Data wrangling ----
## Make sure I retain mosses and lichens, forbs, shrubs, graminoids in Arctic national wildlife refuge (ANWR)
## NB the mosses and lichens might be not well recorded, check for ANWR

## Data exploration
max(itex.full11$YEAR) # latest year
min(itex.full11$YEAR) # earliest year
unique(itex.full11$SITE) # Unique site names


# Retaining only Arctic National Wildlife Refuge (ANWR) site 
ANWR_veg <- itex.full11 %>%
  filter(SITE=="ANWR")

# Range of years 
range(ANWR_veg$YEAR) 
# 1997-2007

### Shrub cover over time  ----
(ggplot(ANWR_veg, aes(x = YEAR, y = ShrubCover))+
  geom_point(size = 2) +
  geom_smooth(method = "lm"))
## Shrub cover increasing 

lm_shrub <- lm(ShrubCover~YEAR, data = ANWR_veg)
summary(lm_shrub)
# F-statistic: 9.151 on 1 and 1118 DF,  p-value: 0.002543

### Graminoid cover over time  ----
(ggplot(ANWR_veg, aes(x = YEAR, y = GraminoidCover))+
   geom_point(size = 2) +
   geom_smooth(method = "lm"))
## Graminoid cover decreasing

lm_graminoid <- lm(GraminoidCover~YEAR, data = ANWR_veg)
summary(lm_graminoid)
# F-statistic:  4.06 on 1 and 1118 DF,  p-value: 0.04415

### Forb cover over time  ----
(ggplot(ANWR_veg, aes(x = YEAR, y = ForbCover))+
   geom_point(size = 2) +
   geom_smooth(method = "lm"))
## Forb cover decreasing 

lm_forb <- lm(ForbCover~YEAR, data = ANWR_veg)
summary(lm_forb)
# F-statistic: 4.148 on 1 and 1118 DF,  p-value: 0.04191

## put them all in same graph !
# facet? or panel


