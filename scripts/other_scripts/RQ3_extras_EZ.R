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



# REMOVE from here -----
# Trying and comparing different model syntaxes
hist(ANWR_veg_fg_trim$sum_cover_int)

# glmer.nb, functional group fixed effect, year and plot random effects
lmer_all_2a <- glmer(sum_cover_int~year_index + FuncGroup + (1|YEAR), data = ANWR_veg_fg_trim)

dispersion_glmer(lmer_all_2a)
summary(lmer_all_2a)
AIC(lmer_all_2a, lmer_all_null)
r.squaredGLMM(lmer_all_2a)
### THIS IS THE MODEL SELECTED FOR THE RESULTS

str(ANWR_veg_fg_trim$mean_cover_int)
unique(ANWR_veg_fg_trim$mean_cover_int)

# glmer poisson, no functional group
lmer_all_0 <- glmer(mean_cover_int~year_index + (1|YEAR), family = "poisson", data = ANWR_veg_fg_trim)
summary(lmer_all_0)
r.squaredGLMM(lmer_all_0)
dispersion_glmer(lmer_all_0)
plot(lmer_all_0)

# glmer poisson, functional group fixed effect 
lmer_all_2 <- glmer(mean_cover_prop~year_index + FuncGroup + (1|YEAR) + (1|PLOT),family = "poisson", data = ANWR_veg_fg_trim)
summary(lmer_all_2) 
r.squaredGLMM(lmer_all_2)
dispersion_glmer(lmer_all_2)
plot(lmer_all_2)

#glm.nb, functional group fixed effect, model without year and plot random effects 
lmer_all_2a_try <- glm.nb(mean_cover_prop~I(YEAR-1995) + FuncGroup, data = ANWR_veg_fg_trim)
summary(lmer_all_2a_try)

# glmer.nb, functional group random effect, no year and plot random effects
lmer_all_3_try <- glmer.nb(mean_cover_prop~I(YEAR-1995) + (1|FuncGroup), data = ANWR_veg_fg_trim)
summary(lmer_all_3_try)

# glmer.nb, functional group random effect, year and plot random effects
lmer_all_3a <- glmer.nb(mean_cover_prop~I(YEAR-1995) + (1|FuncGroup) + (1|YEAR) + (1|PLOT), data = ANWR_veg_fg_trim)
dispersion_glmer(lmer_all_3a) #0.1845308
summary(lmer_all_3a)
r.squaredGLMM(lmer_all_3a)

# glmer poisson, functional group random effect, year and plot random effects
lmer_all_3b <- glmer(mean_cover_prop~I(YEAR-1995) + (1|FuncGroup) + (1|YEAR) + (1|PLOT), family = "poisson", data = ANWR_veg_fg_trim)
dispersion_glmer(lmer_all_3b) # 0.1595953

# glmer.nb, random slopes 
lmer_all_4 <- glmer.nb(mean_cover_prop~I(YEAR-1995) + (1+YEAR|FuncGroup) + (1|YEAR) + (1|PLOT), data = ANWR_veg_fg_trim)
summary(lmer_all_4)
r.squaredGLMM(lmer_all_4)

# glmer.nb with functional group interacting with year
lmer_all <- glmer.nb(mean_cover_prop~I(YEAR-1995)*FuncGroup + (1|YEAR) + (1|PLOT), data = ANWR_veg_fg_trim)
summary(lmer_all)
r.squaredGLMM(lmer_all)
AIC(lmer_all, lmer_all_null, lmer_all_4)
dispersion_glmer(lmer_all_2)# 0.9356298

# glmer poisson, with functional group interacting with year
lmer_all_2<- glmer(mean_cover~I(YEAR-1995)*FuncGroup + (1|YEAR) + (1|PLOT), family = poisson, data = ANWR_veg_fg_trim)
summary(lmer_all_2)
plot(lmer_all_2)
dispersion_glmer(lmer_all_4)# 1.477103

# saving model outputs
tab_model(lmer_all_2a, file = "output/tables/lmer_2a.html")
webshot("output/tables/lmer_2a.html", "output/tables/lmer_2a.png")

tab_model(lmer_all_2a_try, file = "output/tables/lmer_2a_try.html")
webshot("output/tables/lmer_2a_try.html", "output/tables/lmer_2a_try.png")

# null model
lmer_all_null <- glm.nb(mean_cover_prop~1, data = ANWR_veg_fg_trim)
glimpse(ANWR_veg_fg_trim)

# Extract predictions
predictions_vegcover<- ggpredict(lmer_all_2a, terms = c("year_index", "FuncGroup"), interval = "confidence")  # this gives overall predictions for the model

# Model selection ----
# comparing AIC values 
AIC(lmer_all_null, lmer_all, lmer_all_0, lmer_all_2, lmer_all_2a, lmer_all_3, lmer_all_3a, lmer_all_3b, lmer_all_4)

# Model selected: glmer.nb with fixed effect f group and year and plot random, but AIC equivalent to null model


# Extracting model predictions of selected model 
pred_lmer_all_1 <- ggpredict(lmer_all_2a , terms = c("YEAR"))
pred_lmer_all_2 <- ggpredict(lmer_all_2a , terms = c("FuncGroup"))  # this gives overall predictions for the model
# write.csv(pred_model_10, file = "datasets/pred_model_10.csv")

# trying different graph
ANWR_veg_fg_trim$Predicted <- predict(lmer_all_2a, ANWR_veg_fg_trim)

# plot predicted values
ggplot(ANWR_veg_fg_trim, aes(YEAR, Predicted)) +
  facet_wrap(~FuncGroup) +
  geom_point(aes(x = YEAR, y = mean_cover, colour= FuncGroup), size = .5) +
  geom_smooth(aes(y = Predicted, colour= FuncGroup), linetype = "solid", 
              se = T, method = "lm") +
  guides(color=guide_legend(override.aes=list(fill=NA))) +  
  theme_shrub() + 
  xlab("Year") # ugly

# extracting model predictions
pred.mm <- ggpredict(lmer_all_2a, terms = c("YEAR"))

# Plotting fixed effects
(fe.effects <- plot_model(lmer_all_2a, show.values = TRUE))

# Plotting random effects
(re.effects <- plot_model(lmer_all_2a, type = "re", show.values = TRUE))

# MODELLING -----

# 1. SHRUB COVER CHANGE ------

# Mean shrub cover per plot per year (right method - checked with Mariana)
ITEX_shrubs_mean <- ITEX_shrubs %>%
  group_by(SiteSubsitePlotYear) %>%
  mutate(sum_cover = sum(RelCover)) %>%
  ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ITEX_shrubs_mean_trim <- ITEX_shrubs_mean %>% 
  dplyr::select(PLOT, YEAR, LAT, LONG, SUBSITE, SiteSubsitePlotYear, SiteSubsitePlot, sum_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, sum_cover, .keep_all = TRUE)%>% 
  mutate(sum_cover_prop = sum_cover/100) %>%    # making into proportion data
  mutate(sum_cover_int = floor(sum_cover)) %>%   
  mutate(year_index = case_when (YEAR == 1996 ~ '1', YEAR == 1997 ~ '2', 
                                 YEAR == 1998 ~ '3', YEAR == 1999 ~ '4',
                                 YEAR == 2000 ~ '5', YEAR== 2001 ~ '6', 
                                 YEAR == 2002 ~ '7', YEAR == 2003 ~ '8',
                                 YEAR== 2004 ~ '9', YEAR == 2005 ~ '10',
                                 YEAR== 2006 ~ '11', YEAR == 2007 ~ '12')) 

ITEX_shrubs_mean_trim$year_index <- as.numeric(ITEX_shrubs_mean_trim$year_index)
ITEX_shrubs_mean_trim$YEAR <- as.factor(ITEX_shrubs_mean_trim$YEAR)

str(ITEX_shrubs_mean_trim)

# making plot categorical
ITEX_shrubs_mean_trim$PLOT <- as.factor(as.character(ITEX_shrubs_mean_trim$PLOT))
hist(ITEX_shrubs_mean_trim$sum_cover_int) # right skewed
glimpse(ITEX_shrubs_mean_trim)

# Mean shrub cover change over time scatter plot
(shrub_mean_change <- (ggplot(ITEX_shrubs_mean_trim)+
                         geom_point(aes(x = YEAR, y = mean_cover_prop), colour = "green4", size = 1) +
                         geom_smooth(aes(x = YEAR, y = mean_cover_prop), colour= "black", method = "glm") + 
                         scale_x_continuous(breaks=c(1996, 1999, 2002,2005, 2007))+
                         labs(y = "Mean shrub % cover\n", x = "\nYear") + 
                         # annotate(geom = "text", x = 2007, y = 50, label="(a)", size = 10) +
                         theme_shrub()+
                         theme(axis.text.x = element_text(angle = 45))))

# ggsave(shrub_mean_change, file = "output/figures/shrub_mean_change.png")          

# Model 12 ----
# Shrub cover over time
unique(ITEX_shrubs_mean_trim$YEAR)

# Transform percentage cover to proportion (dividing by 100)
ITEX_shrubs_mean_trim <- ITEX_shrubs_mean_trim %>% mutate(cover_prop = mean_cover/100)
hist(ITEX_shrubs_mean_trim$mean_cover)


# glmer.nb, with plot and year as random effects
model_6b <- glmer.nb(mean_cover_int~ year_index  + (1|PLOT) + (1|YEAR), data = ITEX_shrubs_mean_trim)
summary(model_6b)
dispersion_glmer(model_6b)# 0.9659739

# glmer poisson,  with plot and year as random effects
model_6c <- glmer(mean_cover_prop ~ I(YEAR-1995) + (1|PLOT) + (1|YEAR), family = poisson, data = ITEX_shrubs_mean_trim)
summary(model_6c)
dispersion_glmer(model_6c)#0.1816644

# null model 
model_6_null <- glm.nb(mean_cover_prop ~1,  data = ITEX_shrubs_mean_trim)
# comparing AIC
AIC(model_6_null, model_6,model_6a, model_6b, model_6c)

# Checking model assumptions 
plot(model_6b)
qqnorm(resid(model_6b))
qqline(resid(model_6b))  # points fall nicely onto the line - good!

# Output table model 6 
stargazer(model_6, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
pred_model_6 <- ggpredict(model_6b, terms = c("year_index"))
# this gives overall predictions for the model
pred_model_6a <- ggpredict(model_6b, terms = c("YEAR"))
g <-ggeffect(model_6b)
random_effect_terms <- insight::find_random(model_6b, split_nested = TRUE, flatten = TRUE)
ggeffect(model_6b, "year_index",ci.lvl = 0.95)

# write.csv(pred_model_6, file = "datasets/pred_model_6.csv")

# Plot the predictions 
(shrub_preds <- ggplot(pred_model_6, aes(x = x, y = predicted)) +
    stat_smooth(method = "lm")  +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
    geom_point(data = ITEX_shrubs_mean_trim,                      # adding the raw data (scaled values)
               aes(x = year_index, y = mean_cover_int))+
    labs(x = "\nYear (indexed)", y = "Mean shrub cover (%)\n")+
    theme_shrub()) 


ggsave(file = "output/figures/shrub_cover_ANWR.png")

# MODEL(s) 13 ----

# Comparing different models:
lmer_shrub_sp_2a <- glmer.nb(genus_cover_prop~I(YEAR-1995) + (1|GENUS) + (1|PLOT), data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_2 <- glmer.nb(genus_cover_int~year_index + GENUS + (1|YEAR) + (1|PLOT), data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_3 <- glmer.nb(genus_cover_prop~I(YEAR-1995) + (1|GENUS) + (1|YEAR) + (1|PLOT), data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_0 <- glmer.nb(genus_cover_prop~I(YEAR-1995) + (1|YEAR) + (1|PLOT), data = ITEX_shrubs_sp_trim)
lmer_shrub_sp_1 <- glmer.nb(genus_cover_prop~I(YEAR-1995)+ (1|GENUS) , data = ITEX_shrubs_sp_trim)
lmer_shrub_sp <- glmer.nb(genus_cover_int~year_index*GENUS + (1|YEAR) + (1|PLOT), data = ITEX_shrubs_sp_trim) # doesnt converge
dispersion_glmer(lmer_shrub_sp_2) #0.2723795
dispersion_glmer(lmer_shrub_sp_0)# 0.2750381
r.squaredGLMM(lmer_shrub_sp_2)
r.squaredGLMM(lmer_shrub_sp_0)
summary(lmer_shrub_sp_2) 

# Using model with genus fixed effect
summary(lmer_shrub_sp_2)
dispersion_glmer(lmer_shrub_sp_2) # 1.015989
r.squaredGLMM(lmer_shrub_sp_2) 
AIC(lmer_shrub_sp_null, lmer_shrub_sp_2)

# Output tables
tab_model(lmer_shrub_sp_2, file = "output/tables/lmer_shrub_sp_2.html")
webshot("output/tables/lmer_shrub_sp_2.html", "output/tables/lmer_shrub_sp_2.png")

tab_model(lmer_shrub_sp_0, file = "output/tables/lmer_shrub_sp_0.html")
webshot("output/tables/lmer_shrub_sp_0.html", "output/tables/lmer_shrub_sp_0.png")

# Output table 
stargazer(lmer_shrub_sp_2,
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "", 
          type = "html", out = "output/tables/lmer_shrub_sp.html")


# Trying model with random intercepts/slopes
lmer_shrub_sp_rand <- glmer.nb(genus_cover_int~year_index + (1+YEAR|GENUS) + (1|YEAR), data = ITEX_shrubs_sp_trim) 
# NB. doesn't converge with glmer.nb and with glmer we have overdispersion

# null model
lmer_shrub_sp_null <- glm.nb(genus_cover_int~1, data = ITEX_shrubs_sp_trim)

# comparing AIC values
AIC(lmer_shrub_sp_null, lmer_shrub_sp_2)

# Plotting fixed effects
(fe.effects <- plot_model(lmer_shrub_sp_2, show.values = TRUE))

# Plotting random effects
(re.effects <- plot_model(lmer_shrub_sp , type = "re", show.values = TRUE))


#model predicions
p <- ggpredict(lmer_shrub_sp_2, c("year_index", "GENUS"))
p$group <- as.factor(as.character(p$group))
str(p)

# Plot the predictions 
(genera_predictions <- ggplot(p, aes(x = x, y = predicted)) +
    stat_smooth(method = "lm", colour = "black", size = 1.5) +
    # facet_wrap(~ GENUS, scales = "free_y") +
    geom_ribbon(data = p, aes(ymin = conf.low, ymax = conf.high), alpha = 0)) +
  geom_point(data = ITEX_shrubs_sp_trim,                
             aes(x = year_index, y = genus_cover_int, colour = GENUS), size = 2)+
  scale_colour_manual(values = c("#DC9902", "#000000", "#46AAE2", "#003654", "#D55E00", "#009E73","#CC79A7", "#000000"))+
  #geom_smooth(data = ITEX_shrubs_sp_trim, method = lm, aes(colour = GENUS, fill = GENUS), show.legend = FALSE)+
  #scale_fill_manual(values = c("#DC9902", "#000000", "#46AAE2", "#003654", "#D55E00", "#009E73","#CC79A7", "#000000"))+
  scale_x_continuous(breaks=1:13)+
  labs(x = "\nYear (indexed)", y = "Mean cover (%)\n")+
  theme_shrub()+
  theme(axis.text.x  = element_text(vjust=0.5, size=20, angle= 0, 
                                    colour = "black"), 
        legend.position = "right",
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        legend.text = element_text(size=20),
        legend.title = element_text(size=25),
        strip.text.x = element_text(size = 25, face = "italic" )) +
  guides(color = guide_legend(override.aes = list(size = 3)))

ggsave(file = "output/figures/genera_predictions.png")



