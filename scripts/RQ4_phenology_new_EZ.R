##%######################################################%##
#                                                          #
####                 RQ4: PHENOLOGY                     ####
#               Erica Zaja - 05/02/2022                   ##
#                                                         #
##%######################################################%##

# RQ: How has shrub phenology (greening) changed over time near the PCH range?

# LOADING LIBRARIES -----
library(tidyverse)
library(betareg)
library(emmeans)


# LOADING DATA  -----
phenology_data <- read_csv("datasets/phenology_data/CCIN13215_20210302_tundra_phenology_database.csv")

# DATA EXPLORATION and WRANGLING  -----
range(phenology_data$year)# 1992-2019

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
# reorder levels

# ggsave(file = "output/figures/phenophases.png")

# filter for greening only
phenology_green <- phenology_new %>%
  filter(phenophase == "green")

unique(phenology_green$phenophase) # only greening

(greening <- (ggplot(phenology_green, aes(x = phenophase, y = DOY))+
                 geom_boxplot(size = 0.5) +
                 labs(x = "\nOnset of shrub greening", y = "Day of Year\n") + theme_shrub()))
 
# ggsave(file = "output/figures/greening.png")
unique(phenology_green$year)

phenology_green$plot <- as.factor(as.character(phenology_green$plot))

# EARLY VS LATE GREENING -----
phenology_green <- read_csv("datasets/phenology_data/phenology_green.csv")

# Classifying early vs late greening plots
range(phenology_green$DOY) # range of DOY of onset of greening
quantile(phenology_green$DOY) 
# 0%  25%  50%  75% 100% 
# 135  162  166  170  211 

# BUT checking the number of plots per year 
phenology_green_98 <- phenology_green %>% filter(year == "1998") # 451 obs
phenology_green_99 <- phenology_green %>% filter(year == "1999") # 431
phenology_green_00<- phenology_green %>% filter(year == "2000") # 450
# NOT Same number of observations eachn year
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

hist(phenology_mean_doy$mean.doy) # looks normal 

# Shrinking the dataframe to retain one row per plot etc.
phenology_green_trim <- phenology_mean_doy %>% 
  dplyr::select(study_area, subsite, plot, year, SiteSubsitePlotYear, SiteSubsitePlot,
                lat, long, elevation, ecosystem, exstart, soil_moisture, treatment, mean.doy) %>% 
  distinct(SiteSubsitePlotYear, mean.doy, .keep_all = TRUE) # 2980 rows, perfect!


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

hist(prop_greening_plots$prop_early)# not normal
hist(prop_greening_plots$prop_late)# not normal 


# DATA VISUALISATION ----
# 1. EARLY GREENING  -----
(early_greening_plots <- ggplot(prop_greening_plots, aes(x = year, y = prop_early)) +
    geom_point(size = 3, colour = "green4") +
    geom_smooth(method = "lm", colour = "black",  fill ="yellow4")+
   scale_x_continuous(breaks= c(1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2019))+
   annotate(geom = "text", x = 2020, y = 1, label="(a)", size = 10) +
    labs(x = "\nYear", y = "Proportion of early greening plots\n") +
         #title = "Proportion of early greening plots increasing\n") +
    theme_shrub() +
   theme(axis.text.x = element_text(size= 10, angle = 45)))


ggsave(file = "output/figures/early_greening_plots.png")

unique(prop_greening_plots$year)

# Model ----
# Generalised linear mixed model model family binomial 
glm_early <- glmer(prop_early ~  I(year-1995) + (1|year), family = binomial, data = prop_greening_plots)
summary(glm_early)

#Null model
glm_early_null <- glm(prop_early ~  1, family = binomial, data = prop_greening_plots)

AIC(glm_early, glm_early_null)

plot(glm_early)
dispersion_glmer(glm_early)# 0.7259031
qqnorm(resid(glm_early))
qqline(resid(glm_early))

stargazer(glm_early, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")



# 2. LATE GREENING -----
(late_greening_plots <- ggplot(prop_greening_plots, aes(x = year, y = prop_late)) +
    geom_point(size = 3, colour = "green4") +
    geom_smooth(method = "lm", colour = "black", fill ="yellow4")+
   scale_x_continuous(breaks= c(1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2019))+
   annotate(geom = "text", x = 2020, y = 1, label="(b)", size = 10) +
   labs(x = "\nYear", y = "Proportion of late greening plots\n") +
   #title = "Proportion of early greening plots increasing\n") +
    theme_shrub() +
   theme(axis.text.x = element_text(size= 10, angle = 45)))

ggsave(file = "output/figures/late_greening_plots.png")

# Model ----
# Generalised linear mixed model family binomial 
glm_late <- glmer(prop_late ~ I(year-1995) + (1|year), family = binomial, data = prop_greening_plots)
summary(glm_late)
plot(glm_late)
dispersion_glmer(glm_late)# 0.7259028
qqnorm(resid(glm_late))
qqline(resid(glm_late))

# null
glm_late_null <- glm(prop_late ~  1, family = binomial, data = prop_greening_plots)

AIC(glm_late, glm_late_null)

stargazer(glm_late, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Panel ----

panel_pheno <- grid.arrange(arrangeGrob(early_greening_plots, late_greening_plots,
                                           ncol = 2))# Sets number of panel columns

ggsave(panel_pheno, file="output/figures/panel_pheno.png", height = 10, width = 20)

# 3. MEAN DOY ----
# Linear mixed model 
phenology_green_trim$study_area<- as.factor(as.character(phenology_green_trim$study_area))
phenology_green_trim$SiteSubsitePlotYear<- as.factor(as.character(phenology_green_trim$SiteSubsitePlotYear))
str(phenology_green_trim)

# lmer with study_area as random effect 
hist(phenology_green_trim$mean.doy) # normal distribution
lmer_green <- lmer(mean.doy ~ I(year-1995) + (1 |study_area) + (1|year), data = phenology_green_trim ) 
summary(lmer_green)

# null
lmer_green_null <- lm(mean.doy ~ 1, data = phenology_green_trim ) 
AIC(lmer_green, lmer_green_null)

stargazer(lmer_green, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

plot(lmer_green)
qqnorm(resid(glm_late))
qqline(resid(glm_late))

# Extracting model predictions 
predictions_pheno <- ggpredict(lmer_green , terms = c("year", "study_area"), type = "re")
(pheno_rand_slopes <- ggplot(predictions_pheno, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Mean greening DOY\n")+
    theme_shrub()) # mean greening DOY getting earlier 

ggsave(filename = "output/figures/pheno_rand_slopes.png")

# Plot the predictions 
pred_lmer_green <- ggpredict(lmer_green , terms = c("year", "study_area"), type = "re")

(greening_model <- (ggplot(pred_lmer_green, aes(x = x, y = predicted), group=group) + 
                         #geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                           #           fill = "lightgrey", alpha = 0.5) +  # error band
                      #scale_x_continuous(breaks = 1994:2020)+
                         geom_point(data = phenology_green_trim,                      # adding the raw data 
                                    aes(x = year, y = mean.doy, colour= study_area),  size = 0.5) + 
                      stat_smooth(method = lm, color= "green4")+
                         labs(x = "\nYear", y = "Mean greening DOY\n")+ 
                             # title = "Mean greening DOY not changing\n") + 
                         theme_shrub() +  theme(axis.text.x = element_text(size= 10, angle = 45))))

                      

 ggsave(file = "output/figures/greening_model.png")
 
 (all_sites_greening<- (ggplot(phenology_green_trim, aes(x = year, y = mean.doy)) +
                       geom_point(size = 1, aes(colour = study_area))+
                       scale_colour_manual(values = c("brown", "green4", "blue4", "yellow4"), name = "Study area"))+
                       geom_smooth(method = lm, aes(colour= study_area, fill =study_area), show.legend = FALSE)+ 
    # scale_fill_manual(values = c("brown", "green4", "blue4", "yellow4"))+
     scale_x_continuous(breaks= c(1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2019))+
     labs(x = "\nYear", y = "Mean greening DOY\n")+ 
                       theme_shrub() +  theme(axis.text.x = element_text(size= 10, angle = 45)
                                              ))
 
 ggsave(file = "output/figures/all_sites_greening.png")
 
# I might want random slopes/intercepts?
(slopes_pred_lmer_green <- ggplot(pred_lmer_green, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    theme(legend.position = "bottom")+
    labs( x= "Year", y = "mean DOY"))

ggsave(file = "outputs/figures/slopes_pred_lmer_green.png")


# Separate models per study area ----
## ONLY QIKI significant  
# Qikiqtaruk -----
Qikiqtaruk <-  phenology_green_trim %>% filter (study_area == "Qikiqtaruk") 
hist(Qikiqtaruk$mean.doy) 
lmer_Qiki <- lmer(mean.doy ~ I(year-1995) + (1|year), data =Qikiqtaruk ) 
summary(lmer_Qiki)
plot(lmer_Qiki)

lm_Qiki_null <- lm(mean.doy ~ 1, data =Qikiqtaruk ) 
AIC(lmer_Qiki, lm_Qiki_null)

stargazer(lmer_Qiki, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") # Mean DOY does decrease in Qiki
(Qiki_DOY <- ggplot(Qikiqtaruk, aes(x = year, y =mean.doy)) +
    geom_point(size = 2, colour = "green4") +
    geom_smooth(method = "lm", colour = "black", fill = "yellow4")+
    scale_x_continuous(breaks= c(1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2019))+
    #annotate(geom = "text", x = 2015, y = 190, label="(a)", size = 10) +
    annotate(geom = "text", x = 2005, y = 145, label="slope = -0.982** ", size = 6) +
    labs(x = "\nYear", y = "Mean greening DOY\n") +
    #title = "Proportion of early greening plots increasing\n") +
    theme_shrub() +
    theme(axis.text.x = element_text(size = 10, angle=45)))

ggsave(Qiki_DOY, filename = "output/figures/Qiki_DOY.png")

# Atqasuk -----
Atqasuk <-  phenology_green_trim %>% filter (study_area == "Atqasuk") 
lmer_Atqasuk <- lmer(mean.doy ~ year + (1|year), data =Atqasuk ) 
summary(lmer_Atqasuk)
plot(lmer_Atqasuk)

lm_Atqasuk_null <- lm(mean.doy ~ 1, data = Atqasuk) 
AIC(lmer_Atqasuk, lm_Atqasuk_null)

stargazer(lmer_Atqasuk, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") # Mean DOY does decrease in Qiki
(Atqasuk_DOY <- ggplot(Atqasuk, aes(x = year, y =mean.doy)) +
    geom_point(size = 3, colour = "skyblue") +
    geom_smooth(method = "lm", colour = "black")+
    #annotate(geom = "text", x = 2015, y = 190, label="(a)", size = 10) +
    #annotate(geom = "text", x = 2005, y = 145, label="slope = -0.982** ", size = 6) +
    labs(x = "\nYear", y = "Mean greening DOY\n") +
    #title = "Proportion of early greening plots increasing\n") +
    theme_shrub())

# Toolik -----
Toolik <-  phenology_green_trim %>% filter (study_area == "Toolik Lake") 
lmer_Toolik <- lmer(mean.doy ~ year + (1|year), data =Toolik) 
summary(lmer_Toolik)
plot(lmer_Toolik)

lm_Toolik_null <- lm(mean.doy ~ 1, data = Toolik) 
AIC(lmer_Toolik, lm_Toolik_null)

stargazer(lmer_Toolik, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") # Mean DOY does decrease in Qiki
(Toolik_DOY <- ggplot(Toolik, aes(x = year, y =mean.doy)) +
    geom_point(size = 3, colour = "skyblue") +
    geom_smooth(method = "lm", colour = "black")+
    #annotate(geom = "text", x = 2015, y = 190, label="(a)", size = 10) +
    #annotate(geom = "text", x = 2005, y = 145, label="slope = -0.982** ", size = 6) +
    labs(x = "\nYear", y = "Mean greening DOY\n") +
    #title = "Proportion of early greening plots increasing\n") +
    theme_shrub())

# Utqiagvik -----

Utqiagvik<-  phenology_green_trim %>% filter (study_area == "Utqiagvik") 
lmer_Utqiagvik <- lmer(mean.doy ~ year + (1|year), data =Utqiagvik) 
summary(lmer_Toolik)
plot(lmer_Utqiagvik)

lm_Utqiagvik_null <- lm(mean.doy ~ 1, data = Utqiagvik) 
AIC(lmer_Utqiagvik, lm_Utqiagvik_null)

stargazer(lmer_Utqiagvik, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") # Mean DOY does decrease in Qiki
(Utqiagvik_DOY <- ggplot(Utqiagvik, aes(x = year, y =mean.doy)) +
    geom_point(size = 3, colour = "skyblue") +
    geom_smooth(method = "lm", colour = "black")+
    #annotate(geom = "text", x = 2015, y = 190, label="(a)", size = 10) +
    #annotate(geom = "text", x = 2005, y = 145, label="slope = -0.982** ", size = 6) +
    labs(x = "\nYear", y = "Mean greening DOY\n") +
    #title = "Proportion of early greening plots increasing\n") +
    theme_shrub())


# END -----


 