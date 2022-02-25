##%######################################################%##
#                                                          #
####         RQ3: VEGETATION COVER in ANWR -----          ##
#               Erica Zaja - 05/02/2022                   ##
#                                                         ##
##%######################################################%##


# ****MERGING DATASETS**** -----
# NB here you might have 10 plots for each func group - you only want 10 in tot for each year
ITEX_all_veg <- rbind(ITEX_forbs,ITEX_gram, ITEX_lich, ITEX_shrubs, ITEX_moss)
length(unique(ITEX_all_veg$PLOT)) 

ITEX_all_veg %>% group_by(YEAR) %>%
  summarise(plot.n = length(PLOT)) # same as for the dataset at the beginnign

unique(ITEX_all_veg$FuncGroup)  # checking I have all functional groups
hist(ITEX_all_veg$Mean_cover)
str(ITEX_all_veg)

(hist_all_veg <- ITEX_all_veg %>%
    ggplot(aes(x = Mean_cover, fill = FuncGroup)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
    geom_vline(aes(xintercept = mean(Mean_cover)),            
               colour = "red", linetype = "dashed", size = 1) +
    labs(x = "\nPercentage cover", y = "Frequency\n") +
    scale_fill_manual(values=c( "green4", "blue", "yellow", "purple", "red")) +
    theme_shrub())

# ggsave(file = "output/figures/hist_all_veg.png")

ITEX_all_veg$FuncGroup <- as.factor(as.character(ITEX_all_veg$FuncGroup))

# Model ----

# F.group fixed ----
# mixed model with functional group as fixed effect
lmer_all <- lmer(Mean_cover~YEAR + FuncGroup + (1|YEAR) + (1|PLOT), data = ITEX_all_veg)
summary(lmer_all)

# Output table model 7 
stargazer(lmer_all, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

predslm <-  predict(lmer_all , interval = "confidence")
head(predslm)

datlm <- cbind(ITEX_all_veg, predslm)
head(datlm)

### ****SHRUB GENUS****-----
# shrub species
unique(ITEX_shrubs$GENUS)

# Mean shrub cover per plot per year
ITEX_shrub_sp <- ITEX_shrubs %>%
  group_by(SiteSubsitePlotYear, GENUS) %>%
  mutate(genus_cover = sum(RelCover)) %>%
  ungroup()

# Shrinking the dataframe to retain one row per plot etc.
ITEX_shrubs_sp_trim <- ITEX_shrub_sp  %>% 
  dplyr::select(PLOT, YEAR, SiteSubsitePlotYear, SiteSubsitePlot, GENUS, genus_cover) %>% 
  distinct(SiteSubsitePlotYear, genus_cover, .keep_all = TRUE) 
hist(ITEX_shrubs_sp_trim$genus_cover)


# or this way? 
shrub_sp_summary <- ITEX_shrubs %>%
  group_by(YEAR, PLOT, GENUS) %>%
  summarise(n = n(),  # Calculating sample size n
            avg_shrub_sp_cover = mean(FuncPlotCover),  
            # Calculating mean hatching time
            SD = sd(FuncPlotCover))%>%  # Calculating standard deviation
  mutate(SE = SD / sqrt(n))  # Calculating standard error

ITEX_shrub_sp$GENUS <- as.factor(as.character(ITEX_shrub_sp$GENUS ))

(facet_scatter_shrub_genus <- (ggplot(ITEX_shrubs_sp_trim , aes(x = YEAR, y = genus_cover, colour = GENUS))+
                                 geom_point(size = 2) +
                                 geom_smooth(method = "lm") + 
                                 facet_wrap(~ GENUS, scales = "free_y") +
                                 labs(y = "Relative cover \n", x = "\nYear") +
                                 theme_shrub()))


dev.off()
ggsave(file = "output/figures/facet_scatter_shrub_genus.png")

# Model ----

# mixed effect model with plot and year as random effects
lmer_shrub_sp <- lmer(genus_cover~YEAR + GENUS + (1|YEAR), data = ITEX_shrub_sp_trim)
summary(lmer_shrub_sp)

str(ITEX_shrub_sp)

stargazer(lmer_shrub_sp, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "") # some sig

newdat.lme = data.frame(GENUS = ITEX_shrub_sp$GENUS, 
                        YEAR = ITEX_shrub_sp$YEAR) 

newdat.lme$predlme = predict(lmer_shrub_sp, newdata = newdat.lme, level = 0)

ggplot(ITEX_shrub_sp, aes(x = YEAR, y = Mean_cover, color = GENUS) ) +
  geom_rug(sides = "b", size = 1) +
  geom_line(data = newdat.lme, aes(x=YEAR, y = predlme, colour=GENUS), size = 1) 
##lines squiggly???

