##%######################################################%##
#                                                          #
####         RQ3: VEGETATION COVER in ANWR -----          ##
#               Erica Zaja - 05/02/2022                   ##
#                                                         ##
##%######################################################%##

# RQ3: How has vegetation cover changed in the Arctic National Wildlife Refuge between 1996-2007? 
library(betareg)
library(emmeans)

# Beta regression for functional group analysis ----
lmer_all_beta <- betareg(mean_cover_prop~I(YEAR-1995) + 1|FuncGroup, data = ANWR_veg_fg_trim)
summary(lmer_all_beta)
coeftest(lmer_all_beta, vcov = sandwich)
ANWR_veg_fg_trim$Prediction <- predict(lmer_all_beta, ANWR_veg_fg_trim)
ggplot(ANWR_veg_fg_trim, aes(x=YEAR, y=Prediction)) + geom_point() + geom_smooth(method="lm")

plot(lmer_all_beta)
qqnorm(resid(lmer_all_beta))
qqline(resid(lmer_all_beta))  

# Extracting and plotting predictions of random slope model
predictions_rs_all <- ggpredict(lmer_all_2a  , terms = c("YEAR", "FuncGroup"))
(groups_rand_slopes <- ggplot(predictions_rs_all, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    scale_colour_manual(values = c("green4", "green3", "red", "red4", "brown"), name = "Functional group")+
    #scale_x_continuous(breaks=1997:2009)+
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Mean cover (%)\n")+
    theme_shrub()+ 
    theme(axis.text.x = element_text(angle = 45))) # really ugly 

# Extracting model predictions for shrub genus analysis
pred_model_shrub_sp <- ggpredict(lmer_shrub_sp, terms = c("YEAR", "GENUS"))  # this gives overall predictions for the model
# write.csv(pred_model_9, file = "datasets/pred_model_9.csv")
pred_model_shrub_sp_1 <- ggpredict(lmer_shrub_sp, terms = "YEAR")
pred_model_shrub_sp_2 <- ggpredict(lmer_shrub_sp, terms = "GENUS")
str(ITEX_shrub_sp)

# Plot the predictions 
(plot_model_shrub_sp <- (ggplot(pred_model_shrub_sp) + 
                           geom_line(aes(x = x, y = predicted, colour = group) +          # slope
                                       #geom_ribbon(aes(ymin = predicted - std.error, ymax = predicted + std.error), 
                                       #fill = "lightgrey", alpha = 0.5) +  # error band
                                       geom_point(data = ITEX_shrubs_sp_trim, aes(x = YEAR, y = genus_cover), size = 0.5) + 
                                       facet_wrap(~GENUS) +
                                       labs(x = "Year", y = "Shrub species cover (%)", 
                                            title = "Shrub species cover (%) in the ANWR") + 
                                       theme_shrub())))

# trying diff graph
ITEX_shrubs_sp_trim$Predicted <- predict(lmer_shrub_sp, ITEX_shrubs_sp_trim)

# plot predicted values
ggplot(ITEX_shrubs_sp_trim, aes(YEAR, Predicted)) +
  facet_wrap(~GENUS) +
  geom_point(aes(x = YEAR, y = genus_cover, colour= GENUS), size = .5) +
  geom_smooth(aes(y = Predicted, colour = GENUS), linetype = "solid", 
              se = T, method = "lm") +
  guides(color=guide_legend(override.aes=list(fill=NA))) +  
  theme_shrub() + 
  xlab("Year") # ugly 

# Random slopes for shrub genus ----
predict_sp <- ggpredict(lmer_shrub_sp , terms = c("YEAR", "GENUS"), type = "re") 

(pred_plot2 <- ggplot(predict_sp, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    # scale_y_continuous(limits = c(0, )) +
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Predicted mean % cover\n"))

predictions_rs_ri <- ggpredict(lmer_shrub_sp_rand , terms = c("YEAR", "GENUS"), type = "re")
(genus_rand_slopes <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    scale_colour_manual(values = c("green4", "green3", "red", "red4", "brown", "blue4", 'blue3'), name = "Shrub genus")+
    scale_x_continuous(breaks=1997:2009)+
    theme(legend.position = "bottom") +
    labs(x = "\nYear", y = "Mean shrub genus cover (%)\n")+
    theme_shrub()+ 
    theme(axis.text.x = element_text(angle = 45)))

stargazer(lmer_shrub_sp_rand, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

ggsave(file = "output/figures/genus_rand_slopes.png")

# Trying models with poisson distribution for shrub genus
lmer_shrub_sp_2a <- glmer(genus_cover_prop~I(YEAR-1995) + GENUS + (1|YEAR), family = "poisson", data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_3a <- glmer(genus_cover_prop~I(YEAR-1995) + (1|GENUS) + (1|YEAR), family = "poisson", data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_0a <- glmer(genus_cover_prop~I(YEAR-1995) + (1|YEAR), family = "poisson", data = ITEX_shrubs_sp_trim)
dispersion_glmer(lmer_shrub_sp_2a) #0.2316012
dispersion_glmer(lmer_shrub_sp_3a)#0.2369076
dispersion_glmer(lmer_shrub_sp_0a)#0.2512536


