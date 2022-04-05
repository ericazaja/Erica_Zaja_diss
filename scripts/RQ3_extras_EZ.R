##%######################################################%##
#                                                          #
####         RQ3: VEGETATION COVER in ANWR -----          ##
#               Erica Zaja - 05/02/2022                   ##
#                                                         ##
##%######################################################%##

# RQ3: How has vegetation cover changed in the Arctic National Wildlife Refuge between 1996-2007? 

# Beta regression ----
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
