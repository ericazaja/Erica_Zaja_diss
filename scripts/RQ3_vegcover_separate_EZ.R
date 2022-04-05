##%######################################################%##
#                                                          #
####         RQ3: VEGETATION COVER in ANWR -----          ##
#               Erica Zaja - 05/02/2022                   ##
#                                                         ##
##%######################################################%##

# Loading full dataset
ANWR_veg <- read_csv("datasets/ITEX_data/ANWR_veg.csv")

# Creating separate datasets per functional group ----

# a. Filtering shrub only data
ITEX_shrubs <-  ANWR_veg %>% filter (FuncGroup == "Shrub") 
unique(ITEX_shrubs$GENUS) # Unique genus names 
# [1] "Dryas"          "Salix"          "Vaccinium"      "Arctostaphylos" "Betula"         "Cassiope"       "Ledum"         
str(ITEX_shrubs)

# b. Filtering graminoid only data 
ITEX_gram <-  ANWR_veg %>% filter (FuncGroup == "Graminoid") 

# c. Filtering forb only data
ITEX_forbs <-  ANWR_veg %>% filter (FuncGroup == "Forb")

# d. Filtering moss only data
ITEX_moss <-  ANWR_veg %>% filter (FuncGroup == "Moss")

# e. Filtering lichens only data
ITEX_lich <-  ANWR_veg %>% filter (FuncGroup == "Lichen")

# 1. GRAMINOID COVER ----
# Mean graminoid cover per plot per year
ITEX_gram_mean <- ITEX_gram %>%
  group_by(SiteSubsitePlotYear) %>%
  mutate(mean_cover = mean(RelCover)) %>%
  ungroup()

ITEX_gram$PLOT <- as.factor(as.character(ITEX_gram$PLOT))

# Shrinking the dataframe to retain one row per plot etc.
ITEX_gram_mean_trim <- ITEX_gram_mean %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE) %>% 
  mutate(mean_cover_prop = mean_cover/100)

(graminoid_scatter <- (ggplot(ITEX_gram_mean_trim)+
                         geom_point(aes(x = YEAR, y = mean_cover), size = 2) +
                         geom_smooth(aes(x = YEAR, y = mean_cover), method = "lm") + 
                         labs(y = "Mean graminoid cover\n", x = "\nYear") +
                         scale_x_continuous(breaks=1997:2009)+
                         theme_shrub()+
                         theme(axis.text.x = element_text(angle = 45))))

# Model 7 ----
hist(ITEX_gram_mean_trim$mean_cover_prop)
# Graminoid cover over time 
# mixed effect model with plot and year as random effects
model_7 <- glmer.nb(mean_cover_prop~ I(YEAR-1995) + (1|PLOT) + (1|YEAR), data = ITEX_gram_mean_trim)
summary(model_7)
dispersion_glmer(model_7)
tab_model(model_7a)
model_7a <- glm.nb(mean_cover_prop~ I(YEAR-1995), data = ITEX_gram_mean_trim)

# Checking model 7 assumptions 
plot(model_7)
qqnorm(resid(model_7))
qqline(resid(model_7))  # points fall nicely onto the line - good!

# Output table model 7 
stargazer(model_7, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
pred_model_7 <- ggpredict(model_7, terms = c("YEAR"))  # this gives overall predictions for the model
# write.csv(pred_model_7, file = "datasets/pred_model_7.csv")

# Plot the predictions 
(gram_cover_ANWR <- (ggplot(pred_model_7) + 
                       geom_line(aes(x = x, y = predicted)) +          # slope
                       geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                                   fill = "lightgrey", alpha = 0.5) +  # error band
                       geom_point(data = ITEX_gram,                      # adding the raw data 
                                  aes(x = YEAR, y = Mean_cover, colour = PLOT), size = 0.5) + 
                       labs(x = "\nYear", y = "Graminoid cover (%)\n", 
                            title = "Graminoid cover (%) constant in the ANWR\n") + 
                       theme_shrub()
))

# ggsave( file = "output/figures/gram_cover_ANWR.png")

# checking that overall graminoid cover change has same trend as mean cover change
# Total graminoid cover 
ITEX_gram_tot <- ITEX_gram %>%
  group_by(SiteSubsitePlotYear) %>%
  mutate(tot_cover = sum(FuncPlotCover)) %>%
  ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ITEX_gram_tot_trim <- ITEX_gram_tot  %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, tot_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, tot_cover, .keep_all = TRUE)

ITEX_gram_tot_trim$PLOT <- as.factor(as.character(ITEX_gram_tot_trim$PLOT))
hist(ITEX_gram_tot_trim$tot_cover)

# Tot shrub cover change over time  
(gram_scatter_sum <- (ggplot(ITEX_gram_tot_trim)+
                        geom_point(aes(x = YEAR, y = tot_cover, colour = PLOT), size = 2) +
                        geom_smooth(aes(x = YEAR, y = tot_cover), method = "lm") + 
                        scale_x_continuous(breaks=1997:2009)+
                        labs(y = "Total graminoid % cover\n", x = "\nYear") + 
                        theme_shrub()+
                        theme(axis.text.x = element_text(angle = 45))))
# similar trend to mean


# 2. FORB COVER ----

# Mean forb cover per plot per year
ITEX_forb_mean <- ITEX_forbs %>%
  group_by(SiteSubsitePlotYear) %>%
  mutate(mean_cover = mean(RelCover)) %>%
  ungroup()

ITEX_forb_mean$PLOT <- as.factor(as.character(ITEX_forb_mean$PLOT))

# Shrinking the dataframe to retain one row per plot etc.
ITEX_forb_mean_trim <- ITEX_forb_mean %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE) %>% 
  mutate(mean_cover_prop = mean_cover/100)

(forb_scatter <- (ggplot(ITEX_forb_mean_trim)+
                    geom_point(aes(x = YEAR, y = mean_cover, colour = PLOT), size = 2) +
                    geom_smooth(aes(x = YEAR, y = mean_cover), method = "lm") + 
                    labs(y = "Mean forb cover\n", x = "\nYear") +
                    scale_x_continuous(breaks=1997:2009)+
                    theme_shrub() +
                    theme(axis.text.x = element_text(angle = 45))))
# Forb cover decreasing

# Model 8 ----
# Forb cover over time 
# mixed effect model with plot and year as random effects
model_8 <- glmer.nb(mean_cover_prop~I(YEAR-1995) + (1|PLOT) + (1|YEAR), data = ITEX_forb_mean_trim)
summary(model_8)

model_8a <- glm.nb(mean_cover_prop~I(YEAR-1995), data = ITEX_forb_mean_trim)
tab_model(model_8a)
# total variance: 5.687 + 13.076   =18.763
# variance for plot =   5.687 
# amount of variance explained by random effect:  5.687  /18.763 = 0.3030965= ~30%
# I.e. differences between plots explain ~30% of the variance 
# that’s “left over” after the variance explained by our fixed effect (year).
# estimate for year (exp variable = -0.201*** ) i.e. year negatively impats forb cover
# significant effect of year on forb cover  = -0.201***        

# Checking model 8 assumptions 
plot(model_8)
qqnorm(resid(model_8))
qqline(resid(model_8))  # points fall nicely onto the line - good!

# Output table model 8 
stargazer(model_8, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
# significant effect of year

# Extracting model predictions 
pred_model_8 <- ggpredict(model_8, terms = c("YEAR"))  # this gives overall predictions for the model
# write.csv(pred_model_8, file = "datasets/pred_model_8.csv")

# Plot the predictions 
(forb_cover_ANWR <- (ggplot(pred_model_8) + 
                       geom_line(aes(x = x, y = predicted)) +          # slope
                       geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                                   fill = "lightgrey", alpha = 0.5) +  # error band
                       geom_point(data = ITEX_forb_mean_trim,                      # adding the raw data 
                                  aes(x = YEAR, y = mean_cover, colour = PLOT), size = 0.5) + 
                       labs(x = "\nYear", y = "Forb cover (%)\n", 
                            title = "Forb cover (%) decrease in the ANWR\n") + 
                       theme_shrub()
))

ggsave( file = "output/figures/forb_cover_ANWR.png")

# 3. MOSS COVER  ----
# Mean moss cover per plot per year
ITEX_moss_mean <- ITEX_moss %>%
  group_by(SiteSubsitePlotYear) %>%
  mutate(mean_cover = mean(RelCover)) %>%
  ungroup()

ITEX_moss_mean$PLOT <- as.factor(as.character(ITEX_moss_mean$PLOT))

# Shrinking the dataframe to retain one row per plot etc.
ITEX_moss_mean_trim <- ITEX_moss_mean %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE)%>%
  mutate(mean_cover_prop = mean_cover/100)

(moss_scatter <- (ggplot(ITEX_moss_mean_trim)+
                    geom_point(aes(x = YEAR, y = mean_cover, colour = PLOT),size = 2) +
                    geom_smooth(aes(x = YEAR, y = mean_cover), method = "lm") + 
                    labs(y = "Mean moss cover\n", x = "\nYear") +
                    theme_shrub()))
# Moss cover increasing 

# Model 9 ----
# Moss cover over time
# mixed effect model with plot and year as random effects
model_9 <- glmer.nb(mean_cover~I(YEAR-1995) + (1|PLOT) + (1|YEAR), data = ITEX_moss_mean_trim)
summary(model_9)
# total variance: 15.09    + 61.07   =76.16
# variance for plot =   15.09
# amount of variance explained by random effect:  15.09  /76.16=0.1981355= ~20%
# I.e. differences between plots explain ~20% of the variance 
# that’s “left over” after the variance explained by our fixed effect (year).
# estimate for year (exp variable = 6.353e-01***  ) year positively impacts moss cover 
# significant effect of year on moss cover  = 0.635***        

# Checking model 9 assumptions 
plot(model_9)
qqnorm(resid(model_9))
qqline(resid(model_9))  # points fall nicely onto the line - good!

# Output table model 9
stargazer(model_9, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
# year significant

# Extracting model predictions 
pred_model_9 <- ggpredict(model_9, terms = c("YEAR"))  # this gives overall predictions for the model
#write.csv(pred_model_9, file = "datasets/pred_model_9.csv")

# Plot the predictions 
(moss_cover_ANWR <- (ggplot(pred_model_9) + 
                       geom_line(aes(x = x, y = predicted)) +          # slope
                       geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                                   fill = "lightgrey", alpha = 0.5) +  # error band
                       geom_point(data = ITEX_moss_mean_trim,                      # adding the raw data 
                                  aes(x = YEAR, y = mean_cover, colour = PLOT), size = 0.5) + 
                       labs(x = "\nYear", y = "Moss cover (%)\n", 
                            title = "Moss cover (%) increase in the ANWR\n") + 
                       theme_shrub()
))

# ggsave( file = "output/figures/moss_cover_ANWR.png")


# 4. LICHEN COVER  ----
# Mean moss cover per plot per year
ITEX_lich_mean<- ITEX_lich %>%
  group_by(SiteSubsitePlotYear) %>%
  mutate(mean_cover = mean(RelCover)) %>%
  ungroup()

ITEX_lich_mean$PLOT <- as.factor(as.character(ITEX_lich_mean$PLOT))

# Shrinking the dataframe to retain one row per plot etc.
ITEX_lich_mean_trim <- ITEX_lich_mean %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE)%>%
  mutate(mean_cover_prop = mean_cover/100)


(lichen_scatter <- (ggplot(ITEX_lich_mean_trim))+
    geom_point(aes(x = YEAR, y = mean_cover, colour = PLOT), size = 2) +
    geom_smooth(aes(x = YEAR, y = mean_cover), method = "lm") + 
    labs(y = "Mean lichen cover\n", x = "\nYear") +
    theme_shrub())
## Lichen cover increasing

# Model 10 ----
# Lichen cover over time 
# mixed effect model with plot and year as random effects
model_10 <- glmer.nb(mean_cover~I(YEAR-1995)+ (1|PLOT) + (1|YEAR), data = ITEX_lich_mean_trim)
summary(model_10)
# total variance: 99.37     + 260.29  = 359.66
# variance for plot =   99.37
# amount of variance explained by random effect:  99.37  /359.66=0.2762887= ~28%
# I.e. differences between plots explain ~28% of the variance 
# that’s “left over” after the variance explained by our fixed effect (year).
# estimate for year (exp variable =  0.445** ) year positively impacts moss cover 
# significant effect of year on lichen cover  = 0.445**         

# Checking model 9 assumptions 
plot(model_10)
qqnorm(resid(model_10))
qqline(resid(model_10))  # points fall nicely onto the line - good!

# Output table model 9 
stargazer(model_10, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
# year significant

# Extracting model predictions 
pred_model_10 <- ggpredict(model_10, terms = c("YEAR"))  # this gives overall predictions for the model
# write.csv(pred_model_10, file = "datasets/pred_model_10.csv")

# Plot the predictions 
(lichen_cover_ANWR <- (ggplot(pred_model_10) + 
                         geom_line(aes(x = x, y = predicted)) +          # slope
                         geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                                     fill = "lightgrey", alpha = 0.5) +  # error band
                         geom_point(data = ITEX_lich_mean_trim,                      # adding the raw data 
                                    aes(x = YEAR, y = mean_cover, colour = PLOT), size = 0.5) + 
                         labs(x = "\nYear", y = "\nLichen cover (%)", 
                              title = "Lichen cover (%) increase in the ANWR\n") + 
                         # scale_x_continuous(scale_x_continuous(breaks = 1996:2007))+ 
                         theme_shrub()))

# ggsave(file = "output/figures/lichen_cover_ANWR.png")

## Panel 
library(gridExtra)  # For making panels
library(ggpubr)  # For data visualisation formatting
(veg_panel <- grid.arrange(arrangeGrob(shrub_cover_ANWR, forb_cover_ANWR, 
                                       moss_cover_ANWR, lichen_cover_ANWR, 
                                       gram_cover_ANWR,nrow = 2)))

dev.off()
