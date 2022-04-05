# Beta regression ----
lmer_all_beta <- betareg(mean_cover_prop~I(YEAR-1995) + 1|FuncGroup, data = ANWR_veg_fg_trim)
summary(lmer_all_beta)
coeftest(lmer_all_beta, vcov = sandwich)
ANWR_veg_fg_trim$Prediction <- predict(lmer_all_beta, ANWR_veg_fg_trim)
ggplot(ANWR_veg_fg_trim, aes(x=YEAR, y=Prediction)) + geom_point() + geom_smooth(method="lm")

plot(lmer_all_beta)
qqnorm(resid(lmer_all_beta))
qqline(resid(lmer_all_beta))  
