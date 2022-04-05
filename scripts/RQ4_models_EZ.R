##%######################################################%##
#                                                          #
####                 RQ4: PHENOLOGY                     ####
#               Erica Zaja - 05/02/2022                   ##
#                                                         #
##%######################################################%##

# RQ: How has shrub phenology (greening) changed over time near the PCH range?

# LOADING LIBRARIES -----
library(tidyverse)

# LOADING DATA  -----
phenology_data <- read_csv("datasets/phenology_data/CCIN13215_20210302_tundra_phenology_database.csv")
prop_greening_plots <- read_csv("datasets/phenology_data/prop_greening_plots.csv")

# DATA VISUALISATION and modelling ----
# 1. EARLY GREENING  -----
(early_greening_plots <- ggplot(prop_greening_plots, aes(x = year, y = prop_early)) +
    geom_point(size = 3, colour = "#009E73") +
    geom_smooth(method = "lm", colour = "#009E73",  fill ="#009E73", alpha= 0.2, size = 2)+
   scale_x_continuous(breaks= c(1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018))+
   #annotate(geom = "text", x = 2020, y = 1, label="(a)", size = 15) +
    labs(x = "\nYear", y = "Proportion of early greening plots\n") +
         #title = "Proportion of early greening plots increasing\n") +
    theme_shrub() +
   theme(axis.text.x = element_text(size= 20, angle = 45),
         axis.title.x = element_text(size=25),
         axis.title.y = element_text(size=25),
         axis.text.y = element_text(size=25, hjust = 1)))


ggsave(file = "output/figures/early_greening_plots.png")

# adding logo
early_logo <- readPNG("early.png")
raster_early_logo <- as.raster(early_logo)
(early_greening_plots <- early_greening_plots + annotation_raster(raster_early_logo, 2011, 2019, 0.70, 1))
ggsave(file = "output/figures/early_greening_plots.png")



unique(prop_greening_plots$year)

# Model ----
# Generalised linear mixed model model family binomial 
glm_early <- glmer(prop_early ~  I(year-1995) + (1|year), family = binomial, data = prop_greening_plots)
summary(glm_early)
tab_model(glm_early, file = "output/tables/glm_early.html")
webshot("output/tables/glm_early.html", "output/tables/glm_early.png")


glm_early_2 <- glm(prop_early ~  I(year-1995) , family = binomial, data = prop_greening_plots)
summary(glm_early_2)

#Null model
glm_early_null <- glm(prop_early ~  1, family = binomial, data = prop_greening_plots)

AICc(glm_early, glm_early_null, glm_early_2)

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
    geom_point(size = 3, colour = "#009E73") +
   geom_smooth(method = "lm", colour = "#009E73",  fill ="#009E73", alpha= 0.2, size = 2)+
   scale_x_continuous(breaks= c(1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018))+
   # annotate(geom = "text", x = 2020, y = 1, label="(b)", size = 15) +
   labs(x = "\nYear", y = "Proportion of late greening plots\n") +
   #title = "Proportion of early greening plots increasing\n") +
    theme_shrub() +
   theme(axis.text.x = element_text(size= 20, angle = 45),
         axis.title.x = element_text(size=25),
         axis.title.y = element_text(size=25),
         axis.text.y = element_text(size=25, hjust = 1)))

ggsave(file = "output/figures/late_greening_plots.png")

# adding logo
late_logo <- readPNG("late.png")
raster_late_logo <- as.raster(late_logo)
(late_greening_plots <- late_greening_plots + annotation_raster(raster_late_logo, 2012, 2019, 0, 0.25))
ggsave(file = "output/figures/late_greening_plots.png")

# Model ----
# Generalised linear mixed model family binomial 
glm_late <- glmer(prop_late ~ I(year-1995) + (1|year), family = binomial, data = prop_greening_plots)
summary(glm_late)
plot(glm_late)
dispersion_glmer(glm_late)# 0.7259028
qqnorm(resid(glm_late))
qqline(resid(glm_late))

glm_late_2 <- glm(prop_late ~ I(year-1995), family = binomial, data = prop_greening_plots)
summary(glm_late_2)
AIC(glm_late_null)
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
r2_nakagawa(lmer_green)

tab_model(lmer_green, file = "output/tables/lmer_green.html")
webshot("output/tables/lmer_green.html", "output/tables/lmer_green.png")


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
                       scale_colour_manual(values = c("#CC79A7", "#46AAE2", "#D55E00", "#009E73"), name = "Study area"))+
                       geom_smooth(method = lm, aes(colour= study_area, fill =study_area), alpha = 0.3, show.legend = FALSE)+ 
    scale_fill_manual(values = c("#CC79A7", "#46AAE2", "#D55E00", "#009E73"))+
     scale_x_continuous(breaks= c(1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2019))+
     labs(x = "\nYear", y = "Mean greening DOY\n")+ 
    theme_shrub() +  
    theme(axis.text.x = element_text(size= 20, angle = 45), legend.text = element_text(size= 18),
    legend.title = element_text(size=25)) +
    guides(color = guide_legend(override.aes = list(size = 3))))

                                  
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
tab_model(lmer_Qiki, file = "output/tables/lmer_Qiki.html")
webshot("output/tables/lmer_Qiki.html", "output/tables/lmer_Qiki.png")

r2_nakagawa(lmer_Qiki)

lm_Qiki_null <- lm(mean.doy ~ 1, data =Qikiqtaruk ) 
AIC(lmer_Qiki, lm_Qiki_null)

stargazer(lmer_Qiki, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") # Mean DOY does decrease in Qiki

(Qiki_DOY <- ggplot(Qikiqtaruk, aes(x = year, y =mean.doy)) +
    geom_point(size = 2, colour = "#009E73") +
    geom_smooth(method = "lm", colour = "black", fill = "#009E73", size =2)+
    scale_x_continuous(breaks= c(1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2019))+
    #annotate(geom = "text", x = 2015, y = 190, label="(a)", size = 10) +
    annotate(geom = "text", x = 2005, y = 145, label="slope = -0.982** ", size = 10) +
    labs(x = "\nYear", y = "Mean greening DOY\n") +
    #title = "Proportion of early greening plots increasing\n") +
    theme_shrub() +
    theme(axis.text.x = element_text(size = 15, angle=45)))

ggsave(Qiki_DOY, filename = "output/figures/Qiki_DOY.png")

# adding logo

(Qiki_DOY <- Qiki_DOY + annotation_raster(raster_early_logo, 2012, 2016, 180, 200))
ggsave(file = "output/figures/Qiki_DOY.png")


# Atqasuk -----
Atqasuk <-  phenology_green_trim %>% filter (study_area == "Atqasuk") 
lmer_Atqasuk <- lmer(mean.doy ~ year + (1|year), data =Atqasuk ) 
summary(lmer_Atqasuk)
plot(lmer_Atqasuk)

lm_Atqasuk_null <- lm(mean.doy ~ 1, data = Atqasuk) 
AIC(lmer_Atqasuk, lm_Atqasuk_null)

r2_nakagawa(lmer_Atqasuk)

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

r2_nakagawa(lmer_Toolik)

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

r2_nakagawa(lmer_Utqiagvik)

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


 