##%######################################################%##
#                                                          #
####         RQ3: VEGETATION COVER in ANWR -----          ##
#               Erica Zaja - 05/02/2022                   ##
#                                                         ##
##%######################################################%##

# LIBRARIES -----
library(lme4)
library(ggeffects)
library(effects)


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

ITEX_lich_forb <-  ANWR_veg %>% filter (FuncGroup %in% c("Lichen", "Forb")) 


# Mean cover per plot per year
ITEX_lich_forb_mean <- ITEX_lich_forb %>%
  group_by(SiteSubsitePlotYear) %>%
  mutate(mean_cover = mean(RelCover)) %>%
  ungroup()

# making plot categorical
ITEX_lich_forb_mean$PLOT <- as.factor(as.character(ITEX_lich_forb_mean$PLOT))

# Shrinking the dataframe to retain one row per plot etc.
ITEX_lich_forb_mean_trim <- ITEX_lich_forb_mean %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE) %>% 
  mutate(mean_cover_prop = mean_cover/100) %>% 
  mutate(mean_cover_int = floor(mean_cover)) %>%   
  mutate(year_index = case_when (YEAR == 1996 ~ '1', YEAR == 1997 ~ '2', 
                                 YEAR == 1998 ~ '3', YEAR == 1999 ~ '4',
                                 YEAR == 2000 ~ '5', YEAR== 2001 ~ '6', 
                                 YEAR == 2002 ~ '7', YEAR == 2003 ~ '8',
                                 YEAR== 2004 ~ '9', YEAR == 2005 ~ '10',
                                 YEAR== 2006 ~ '11', YEAR == 2007 ~ '12')) 


ITEX_lich_forb_mean_trim %>% group_by(PLOT)%>%  count(mean_cover_int == 0)

# 1. GRAMINOID COVER ----
# Mean graminoid cover per plot per year
ITEX_gram_mean <- ITEX_gram %>%
  group_by(SiteSubsitePlotYear) %>%
  mutate(mean_cover = mean(RelCover)) %>%
  ungroup()

# making plot categorical
ITEX_gram$PLOT <- as.factor(as.character(ITEX_gram$PLOT))

# Shrinking the dataframe to retain one row per plot etc.
ITEX_gram_mean_trim <- ITEX_gram_mean %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE) %>% 
  mutate(mean_cover_prop = mean_cover/100) %>% 
mutate(mean_cover_int = ceiling(mean_cover)) %>%   
  mutate(year_index = case_when (YEAR == 1996 ~ '1', YEAR == 1997 ~ '2', 
                                 YEAR == 1998 ~ '3', YEAR == 1999 ~ '4',
                                 YEAR == 2000 ~ '5', YEAR== 2001 ~ '6', 
                                 YEAR == 2002 ~ '7', YEAR == 2003 ~ '8',
                                 YEAR== 2004 ~ '9', YEAR == 2005 ~ '10',
                                 YEAR== 2006 ~ '11', YEAR == 2007 ~ '12')) 
  
# scatter
(graminoid_scatter <- (ggplot(ITEX_gram_mean_trim)+
                         geom_point(aes(x = YEAR, y = mean_cover), size = 2) +
                         geom_smooth(aes(x = YEAR, y = mean_cover), method = "lm") + 
                         labs(y = "Mean graminoid cover\n", x = "\nYear") +
                         scale_x_continuous(breaks=1997:2009)+
                         theme_shrub()+
                         theme(axis.text.x = element_text(angle = 45))))

# Model 7 ----
hist(ITEX_gram_mean_trim$mean_cover_prop) # right skewed
# Graminoid cover over time 
# glmer.nb, with plot and year as random effects
ITEX_gram_mean_trim$year_index <- as.numeric(ITEX_gram_mean_trim$year_index)
model_7 <- glmer.nb(mean_cover_int ~ year_index + (1|PLOT) + (1|YEAR), data = ITEX_gram_mean_trim)

summary(model_7)
dispersion_glmer(model_7)

# extracting predictions with ggeffect()
g7 <- ggeffect(model_7, back.transform = TRUE)
get_complete_df(g7)

pred_7 <- ggpredict(model_7, terms= c("year_index"))

# Plotting predictions
(gram_preds <- ggplot(pred_7, aes(x = x, y = predicted)) +
    stat_smooth(method = "lm")  +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
       geom_point(data = ITEX_gram_mean_trim,                      # adding the raw data (scaled values)
                  aes(x = year_index, y = mean_cover_int))+
     labs(x = "\nYear (indexed)", y = "Mean graminoid cover (%)\n")+
    theme_shrub()) 

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
pred_model_7 <- ggpredict(model_7, terms = c("year_index"))  # this gives overall predictions for the model
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

# making plot categorical
ITEX_forb_mean$PLOT <- as.factor(as.character(ITEX_forb_mean$PLOT))

# Shrinking the dataframe to retain one row per plot etc.
ITEX_forb_mean_trim <- ITEX_forb_mean %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE) %>% 
  mutate(mean_cover_prop = mean_cover/100)%>% 
  mutate(mean_cover_int = ceiling(mean_cover)) %>%   
  mutate(year_index = case_when (YEAR == 1996 ~ '1', YEAR == 1997 ~ '2', 
                                 YEAR == 1998 ~ '3', YEAR == 1999 ~ '4',
                                 YEAR == 2000 ~ '5', YEAR== 2001 ~ '6', 
                                 YEAR == 2002 ~ '7', YEAR == 2003 ~ '8',
                                 YEAR== 2004 ~ '9', YEAR == 2005 ~ '10',
                                 YEAR== 2006 ~ '11', YEAR == 2007 ~ '12')) 

# scatter 
(forb_scatter <- (ggplot(ITEX_forb_mean_trim)+
                    geom_point(aes(x = YEAR, y = mean_cover, colour = PLOT), size = 2) +
                    geom_smooth(aes(x = YEAR, y = mean_cover), method = "lm") + 
                    labs(y = "Mean forb cover\n", x = "\nYear") +
                    scale_x_continuous(breaks=1997:2009)+
                    theme_shrub() +
                    theme(axis.text.x = element_text(angle = 45))))

# Model 8 ----
# Forb cover over time 
ITEX_forb_mean_trim$year_index <- as.numeric(ITEX_forb_mean_trim$year_index)
unique(ITEX_forb_mean_trim$mean_cover_int)

# glmer.nb, with plot and year as random effects
model_8 <- glmer.nb(mean_cover_int~year_index + (1|PLOT) + (1|YEAR), data = ITEX_forb_mean_trim)
summary(model_8) # doesnt converge

model_8a <- glmer(mean_cover_int~year_index + (1|PLOT) + (1|YEAR), family = "poisson", data = ITEX_forb_mean_trim)


# trying model without random effects
model_8a <- glm.nb(mean_cover_int~year_index, data = ITEX_forb_mean_trim)
summary(model_8a) # this runs

# Checking model 8 assumptions 
plot(model_8a)
qqnorm(resid(model_8))
qqline(resid(model_8))  # points fall nicely onto the line - good!

# Output table model 8 
stargazer(model_8, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
pred_model_8 <- ggpredict(model_8a, terms = c("year_index"))  # this gives overall predictions for the model
# write.csv(pred_model_8, file = "datasets/pred_model_8.csv")

# Plot the predictions 
(forb_preds <- ggplot(pred_model_8, aes(x = x, y = predicted)) +
    stat_smooth(method = "lm")  +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
    geom_point(data = ITEX_forb_mean_trim,                      # adding the raw data (scaled values)
               aes(x = year_index, y = mean_cover_int))+
    labs(x = "\nYear (indexed)", y = "Mean forb cover (%)\n")+
    theme_shrub()) 

# ggsave( file = "output/figures/forb_cover_ANWR.png")

# 3. MOSS COVER  ----
# Mean moss cover per plot per year
ITEX_moss_mean <- ITEX_moss %>%
  group_by(SiteSubsitePlotYear) %>%
  mutate(mean_cover = mean(RelCover)) %>%
  ungroup()

# making plot categorical
ITEX_moss_mean$PLOT <- as.factor(as.character(ITEX_moss_mean$PLOT))

# Shrinking the dataframe to retain one row per plot etc.
ITEX_moss_mean_trim <- ITEX_moss_mean %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE)%>%
  mutate(mean_cover_prop = mean_cover/100)%>%
  mutate(mean_cover_int = ceiling(mean_cover)) %>%   
  mutate(year_index = case_when (YEAR == 1996 ~ '1', YEAR == 1997 ~ '2', 
                                 YEAR == 1998 ~ '3', YEAR == 1999 ~ '4',
                                 YEAR == 2000 ~ '5', YEAR== 2001 ~ '6', 
                                 YEAR == 2002 ~ '7', YEAR == 2003 ~ '8',
                                 YEAR== 2004 ~ '9', YEAR == 2005 ~ '10',
                                 YEAR== 2006 ~ '11', YEAR == 2007 ~ '12')) 

ITEX_moss_mean_trim$year_index <- as.numeric(ITEX_moss_mean_trim$year_index )

# scatter
(moss_scatter <- (ggplot(ITEX_moss_mean_trim)+
                    geom_point(aes(x = YEAR, y = mean_cover, colour = PLOT),size = 2) +
                    geom_smooth(aes(x = YEAR, y = mean_cover), method = "lm") + 
                    labs(y = "Mean moss cover\n", x = "\nYear") +
                    theme_shrub()))

# Model 9 ----
# Moss cover over time
model_9 <- glmer.nb(mean_cover_int~year_index + (1|PLOT) + (1|YEAR), data = ITEX_moss_mean_trim)
summary(model_9)

# Checking model 9 assumptions 
plot(model_9)
qqnorm(resid(model_9))
qqline(resid(model_9))  # points fall nicely onto the line - good!

# Output table model 9
stargazer(model_9, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
pred_model_9 <- ggpredict(model_9, terms = c("year_index"))  # this gives overall predictions for the model
#write.csv(pred_model_9, file = "datasets/pred_model_9.csv")

# Plot the predictions 
(moss_preds <- ggplot(pred_model_9, aes(x = x, y = predicted)) +
    stat_smooth(method = "lm")  +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
    geom_point(data = ITEX_moss_mean_trim,                      # adding the raw data (scaled values)
               aes(x = year_index, y = mean_cover_int))+
    labs(x = "\nYear (indexed)", y = "Mean moss cover (%)\n")+
    theme_shrub()) 

# ggsave( file = "output/figures/moss_cover_ANWR.png")


# 4. LICHEN COVER  ----
# Mean moss cover per plot per year
ITEX_lich_mean<- ITEX_lich %>%
  group_by(SiteSubsitePlotYear) %>%
  mutate(mean_cover = mean(RelCover)) %>%
  ungroup()

# making plot categorical
ITEX_lich_mean$PLOT <- as.factor(as.character(ITEX_lich_mean$PLOT))

# Shrinking the dataframe to retain one row per plot etc.
ITEX_lich_mean_trim <- ITEX_lich_mean %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, mean_cover, lat_grid, lon_grid, gridcell) %>% 
  distinct(SiteSubsitePlotYear, mean_cover, .keep_all = TRUE)%>%
  mutate(mean_cover_prop = mean_cover/100)%>%
  mutate(mean_cover_int = ceiling(mean_cover)) %>%   
  mutate(year_index = case_when (YEAR == 1996 ~ '1', YEAR == 1997 ~ '2', 
                                 YEAR == 1998 ~ '3', YEAR == 1999 ~ '4',
                                 YEAR == 2000 ~ '5', YEAR== 2001 ~ '6', 
                                 YEAR == 2002 ~ '7', YEAR == 2003 ~ '8',
                                 YEAR== 2004 ~ '9', YEAR == 2005 ~ '10',
                                 YEAR== 2006 ~ '11', YEAR == 2007 ~ '12')) 

ITEX_lich_mean_trim$year_index <- as.numeric(ITEX_lich_mean_trim$year_index )
unique(ITEX_lich_mean_trim$year_index)
hist(ITEX_lich_mean_trim$mean_cover_int)

# scatter
(lichen_scatter <- (ggplot(ITEX_lich_mean_trim))+
    geom_point(aes(x = YEAR, y = mean_cover, colour = PLOT), size = 2) +
    geom_smooth(aes(x = YEAR, y = mean_cover), method = "lm") + 
    labs(y = "Mean lichen cover\n", x = "\nYear") +
    theme_shrub())

unique(ITEX_lich_mean_trim$mean_cover_int)
unique(ITEX_forb_mean_trim$mean_cover_int)
unique(ITEX_moss_mean_trim$mean_cover_int)
unique(ITEX_shrubs_mean_trim$mean_cover_int)
unique(ITEX_gram_mean_trim$mean_cover_int)


# Model 10 ----
# Lichen cover over time 
model_10 <- glmer.nb(mean_cover_int~year_index + (1|PLOT) + (1|YEAR), data = ITEX_lich_mean_trim)
summary(model_10)

model_10b <- glm.nb(mean_cover_int~year_index, data = ITEX_lich_mean_trim)


# Checking model 9 assumptions 
plot(model_10)
qqnorm(resid(model_10))
qqline(resid(model_10))  # points fall nicely onto the line - good!

# Output table model 9 
stargazer(model_10, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# Extracting model predictions 
pred_model_10 <- ggpredict(model_10b, terms = c("year_index"))  # this gives overall predictions for the model
# write.csv(pred_model_10, file = "datasets/pred_model_10.csv")

# Plot the predictions 
(lichen_preds <- ggplot(pred_model_10, aes(x = x, y = predicted)) +
    stat_smooth(method = "lm")  +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
    geom_point(data = ITEX_lich_mean_trim,                      # adding the raw data (scaled values)
               aes(x = year_index, y = mean_cover_int))+
    labs(x = "\nYear (indexed)", y = "Mean lichen cover (%)\n")+
    theme_shrub()) 

# ggsave(file = "output/figures/lichen_cover_ANWR.png")

## Panel 
library(gridExtra)  # For making panels
library(ggpubr)  # For data visualisation formatting
(veg_panel <- grid.arrange(arrangeGrob(shrub_cover_ANWR, forb_cover_ANWR, 
                                       moss_cover_ANWR, lichen_cover_ANWR, 
                                       gram_cover_ANWR,nrow = 2)))

dev.off()

########################################################## END ----
