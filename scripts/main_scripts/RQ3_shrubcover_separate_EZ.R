##%######################################################%##
#                                                          #
####         RQ3: SHRUB COVER in ANWR -----               ##
#               Erica Zaja - 05/02/2022                   ##
#                                                         ##
##%######################################################%##

# RQ3: How has vegetation cover changed in the Arctic National Wildlife Refuge between 1996-2007? 
library(tidyverse)

# LOADING DATA ----
# Loading and cleaning the data 
ANWR_veg <- read_csv("datasets/ITEX_data/ANWR_veg.csv")
# Filtering shrub only data
ITEX_shrubs <-  ANWR_veg %>% filter (FuncGroup == "Shrub") 

# Mean shrub genus cover per plot per year
ITEX_shrub_sp <- ITEX_shrubs %>%
  group_by(SiteSubsitePlotYear, GENUS) %>%
  mutate(genus_cover = sum(RelCover)) %>%
  ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ITEX_shrubs_sp_trim <- ITEX_shrub_sp  %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, GENUS, genus_cover) %>% 
  distinct(SiteSubsitePlotYear, genus_cover, .keep_all = TRUE)  %>% 
  mutate(genus_cover_prop = genus_cover/100)

# SEPARATE models per genus ----
# NB. none significant 

# a. Salix sp. ----
Salix <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Salix") 
salix_model <- glmer.nb(genus_cover ~ I(YEAR-1995)+ (1|YEAR), data = Salix)
summary(salix_model) # not sig
stargazer(salix_model, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

hist(Salix$genus_cover)

# scatter
(salix_plot <- ggplot(Salix, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n")) 

# b. Dryas sp.-----
Dryas <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Dryas") 
dryas_model <- glmer.nb(genus_cover ~ I(YEAR-1995) + (1|YEAR), data = Dryas)
summary(dryas_model)# not sig

stargazer(dryas_model, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

(dryas_plot <- ggplot(Dryas, aes(x = YEAR, y = genus_cover)) +
    geom_point(colour = "green4", size = 1)+
    stat_smooth(method = "lm", colour = 'black', fill = 'yellow4')  +
    scale_x_continuous(breaks=c(1996, 1999, 2002,2005, 2007))+
    labs(x = "\nYear", y = "Dryas % cover\n")+ 
    theme_shrub()+
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 45)))


# c. Vaccinium sp.-----
Vaccinium <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Vaccinium") 
hist(Vaccinium$genus_cover)
vacc_model <- glmer.nb(genus_cover ~ YEAR + (1|YEAR), data = Vaccinium)
summary(vacc_model)# not sig

(vacc_plot <- ggplot(Vaccinium, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# d. Arctostaphylos sp.----
Arctostaphylos <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Arctostaphylos") 
arcto_model <- glmer.nb(genus_cover ~ YEAR + (1|YEAR), data = Arctostaphylos)
summary(arcto_model)# not sig

(arcto_plot <- ggplot(Arctostaphylos, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# e. Betula sp.----
Betula <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Betula") 
betula_model <- glmer.nb(genus_cover ~ YEAR + (1|YEAR), data = Betula)
summary(betula_model)# not sig

(betula_plot <- ggplot(Betula, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# f. Cassiope sp.----
Cassiope<-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Cassiope") 
cassiope_model <- glmer.nb(genus_cover ~ YEAR + (1|YEAR), data = Cassiope)
summary(cassiope_model)# not sig

(cassiope_plot <- ggplot(Cassiope, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# g. Ledum sp.----
Ledum <-  ITEX_shrubs_sp_trim %>% filter (GENUS == "Ledum") 
ledum_model <- glmer.nb(genus_cover ~ YEAR + (1|YEAR), data = Ledum)
summary(ledum_model)# not sig

(ledum_plot <- ggplot(Ledum, aes(x = YEAR, y = genus_cover)) +
    geom_point()+
    stat_smooth(method = "lm")  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Genus % cover\n"))

# Vary genus name to visualise distributions
hist(Salix$genus_cover)

