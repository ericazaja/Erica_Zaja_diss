##%######################################################%##
#                                                          #
####                 RQ4: PHENOLOGY                     ####
#               Erica Zaja - 05/02/2022                   ##
#                                                         #
##%######################################################%##

# RQ: How has shrub phenology (greening) changed over time near the PCH range?

# LOADING LIBRARIES -----
library(tidyverse)
library(webshot)

# LOADING DATA  -----
prop_greening_plots <- read_csv("datasets/phenology_data/prop_greening_plots.csv")
phenology_green_trim <- read_csv("datasets/phenology_data/phenology_green_trim.csv")

# DATA VISUALISATION and modelling ----

# 1. EARLY GREENING  -----

# Scatter
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

#ggsave(file = "output/figures/early_greening_plots.png")

# adding logo
early_logo <- readPNG("early.png")
raster_early_logo <- as.raster(early_logo)
(early_greening_plots <- early_greening_plots + annotation_raster(raster_early_logo, 2011, 2019, 0.70, 1))
ggsave(file = "output/figures/early_greening_plots.png")

# MODEL 13 ----
# Generalised linear mixed model model family binomial 
glm_early <- glmer(prop_early ~  I(year-1995) + (1|year), family = binomial, data = prop_greening_plots)
summary(glm_early)
r.squaredGLMM(glm_early)

# null model
glm_early_null <- glm(prop_early ~  1, family = binomial, data = prop_greening_plots)
AIC(glm_early, glm_early_null)

# assumptions
plot(glm_early)
dispersion_glmer(glm_early)# 0.7259031
qqnorm(resid(glm_early))
qqline(resid(glm_early))

# model output tables
tab_model(glm_early, file = "output/tables/glm_early.html")
webshot("output/tables/glm_early.html", "output/tables/glm_early.png")
stargazer(glm_early, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


# 2. LATE GREENING -----
# Scatter
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

#ggsave(file = "output/figures/late_greening_plots.png")

# adding logo
late_logo <- readPNG("late.png")
raster_late_logo <- as.raster(late_logo)
(late_greening_plots <- late_greening_plots + annotation_raster(raster_late_logo, 2012, 2019, 0, 0.25))
ggsave(file = "output/figures/late_greening_plots.png")

# Model 13 ----
# Generalised linear mixed model family binomial 
glm_late <- glmer(prop_late ~ I(year-1995) + (1|year), family = binomial, data = prop_greening_plots)
summary(glm_late)
r.squaredGLMM(glm_late)

# null model
glm_late_null <- glm(prop_late ~  1, family = binomial, data = prop_greening_plots)
AIC(glm_late, glm_late_null)

# checking assumptions
plot(glm_late)
dispersion_glmer(glm_late)# 0.7259028
qqnorm(resid(glm_late))
qqline(resid(glm_late))

# output table
stargazer(glm_late, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Panel ----

panel_pheno <- grid.arrange(arrangeGrob(early_greening_plots, late_greening_plots,
                                           ncol = 2))# Sets number of panel columns

ggsave(panel_pheno, file="output/figures/panel_pheno.png", height = 10, width = 20)

# 3. MEAN DOY ----

# making study area categorical
phenology_green_trim$study_area<- as.factor(as.character(phenology_green_trim$study_area))
str(phenology_green_trim)

# lmer with study_area as random effect 
lmer_green <- lmer(mean.doy ~ I(year-1995) + (1 |study_area) + (1|year), data = phenology_green_trim ) 
summary(lmer_green)
r2_nakagawa(lmer_green)

tab_model(lmer_green, file = "output/tables/lmer_green.html")
webshot("output/tables/lmer_green.html", "output/tables/lmer_green.png")

# null model
lmer_green_null <- lm(mean.doy ~ 1, data = phenology_green_trim ) 
AIC(lmer_green, lmer_green_null)

# checking assumptions
plot(lmer_green)
qqnorm(resid(glm_late))
qqline(resid(glm_late))

# Scatter by study area
(all_sites_greening <- (ggplot(phenology_green_trim, aes(x = year, y = mean.doy)) +
                       geom_point(size = 1, aes(colour = study_area))+
                       scale_colour_manual(values = c("#332288", "#117733", "#DDCC77", "#CC6677"), name = "Study area"))+
                       geom_smooth(method = lm, aes(colour= study_area, fill =study_area), alpha = 0.3, show.legend = FALSE)+ 
    scale_fill_manual(values = c("#332288", "#117733", "#DDCC77", "#CC6677"))+
     scale_x_continuous(breaks= c(1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2019))+
     labs(x = "\nYear", y = "Mean greening DOY\n")+ 
    theme_shrub() +  
    theme(axis.text.x = element_text(size= 20, angle = 45), legend.text = element_text(size= 18),
    legend.title = element_text(size=25)) +
    guides(color = guide_legend(override.aes = list(size = 3))))

                                  
# ggsave(file = "output/figures/all_sites_greening.png")

# Extract predictions
predictions_pheno <- as.data.frame(predict(lmer_green, newdata = phenology_green_trim, CI = TRUE)) # this gives overall predictions for the model
preds_pheno <- cbind(phenology_green_trim, predictions_pheno)

preds_pheno$study_area <- as.factor(as.character(preds_pheno$study_area))

# Plot the predictions 

# Separate models per study area ----
## ONLY QIKI significant  

# a. Qikiqtaruk -----
Qikiqtaruk <-  phenology_green_trim %>% filter (study_area == "Qikiqtaruk") 
hist(Qikiqtaruk$mean.doy) 

# Model
lmer_Qiki <- lmer(mean.doy ~ I(year-1995) + (1|year), data =Qikiqtaruk ) 
summary(lmer_Qiki)
plot(lmer_Qiki)
r2_nakagawa(lmer_Qiki)

# null
lm_Qiki_null <- lm(mean.doy ~ 1, data =Qikiqtaruk ) 
AIC(lmer_Qiki, lm_Qiki_null)

# output
tab_model(lmer_Qiki, file = "output/tables/lmer_Qiki.html")
webshot("output/tables/lmer_Qiki.html", "output/tables/lmer_Qiki.png")
stargazer(lmer_Qiki, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") # Mean DOY does decrease in Qiki

# scatter
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

# b. Atqasuk -----
Atqasuk <-  phenology_green_trim %>% filter (study_area == "Atqasuk") 
lmer_Atqasuk <- lmer(mean.doy ~ year + (1|year), data =Atqasuk ) 
summary(lmer_Atqasuk)
plot(lmer_Atqasuk)
r2_nakagawa(lmer_Atqasuk)

# null
lm_Atqasuk_null <- lm(mean.doy ~ 1, data = Atqasuk) 
AIC(lmer_Atqasuk, lm_Atqasuk_null)

# output
stargazer(lmer_Atqasuk, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") 

# scatter
(Atqasuk_DOY <- ggplot(Atqasuk, aes(x = year, y =mean.doy)) +
    geom_point(size = 3, colour = "skyblue") +
    geom_smooth(method = "lm", colour = "black")+
    #annotate(geom = "text", x = 2015, y = 190, label="(a)", size = 10) +
    #annotate(geom = "text", x = 2005, y = 145, label="slope = -0.982** ", size = 6) +
    labs(x = "\nYear", y = "Mean greening DOY\n") +
    #title = "Proportion of early greening plots increasing\n") +
    theme_shrub())

# c. Toolik -----
Toolik <-  phenology_green_trim %>% filter (study_area == "Toolik Lake") 
lmer_Toolik <- lmer(mean.doy ~ year + (1|year), data =Toolik) 
summary(lmer_Toolik)
plot(lmer_Toolik)
r2_nakagawa(lmer_Toolik)

# null
lm_Toolik_null <- lm(mean.doy ~ 1, data = Toolik) 
AIC(lmer_Toolik, lm_Toolik_null)

# output
stargazer(lmer_Toolik, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") # Mean DOY does decrease in Qiki
# scatter
(Toolik_DOY <- ggplot(Toolik, aes(x = year, y =mean.doy)) +
    geom_point(size = 3, colour = "skyblue") +
    geom_smooth(method = "lm", colour = "black")+
    #annotate(geom = "text", x = 2015, y = 190, label="(a)", size = 10) +
    #annotate(geom = "text", x = 2005, y = 145, label="slope = -0.982** ", size = 6) +
    labs(x = "\nYear", y = "Mean greening DOY\n") +
    #title = "Proportion of early greening plots increasing\n") +
    theme_shrub())

# d. Utqiagvik -----
Utqiagvik<-  phenology_green_trim %>% filter (study_area == "Utqiagvik") 
lmer_Utqiagvik <- lmer(mean.doy ~ year + (1|year), data =Utqiagvik) 
summary(lmer_Toolik)
plot(lmer_Utqiagvik)
r2_nakagawa(lmer_Utqiagvik)

# null
lm_Utqiagvik_null <- lm(mean.doy ~ 1, data = Utqiagvik) 
AIC(lmer_Utqiagvik, lm_Utqiagvik_null)

# output
stargazer(lmer_Utqiagvik, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") 

# scatter
(Utqiagvik_DOY <- ggplot(Utqiagvik, aes(x = year, y =mean.doy)) +
    geom_point(size = 3, colour = "skyblue") +
    geom_smooth(method = "lm", colour = "black")+
    #annotate(geom = "text", x = 2015, y = 190, label="(a)", size = 10) +
    #annotate(geom = "text", x = 2005, y = 145, label="slope = -0.982** ", size = 6) +
    labs(x = "\nYear", y = "Mean greening DOY\n") +
    #title = "Proportion of early greening plots increasing\n") +
    theme_shrub())


####################################################################### END -----


 