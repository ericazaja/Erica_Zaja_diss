##%######################################################%##
#                                                          #
####                 RQ4: PHENOLOGY                     ####
#               Erica Zaja - 05/02/2022                   ##
#                                                         #
##%######################################################%##

# RQ: How has shrub phenology (greening) changed over time near the PCH range?

# LOADING LIBRARIES -----
library(tidyverse)

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

# I might want random slopes/intercepts?
(slopes_pred_lmer_green <- ggplot(pred_lmer_green, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    theme(legend.position = "bottom")+
    labs( x= "Year", y = "mean DOY"))

ggsave(file = "outputs/figures/slopes_pred_lmer_green.png")
