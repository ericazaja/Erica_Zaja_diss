# This R script computes Spearman correlations between spatial patterns of tundra aboveground biomass
# and mean monthly air temperature across the Alaskan North Slope. 

rm(list=ls())
require(plyr)
require(dplyr)

files <- rev(list.files('/scratch/lb968/agb_mc_iterations/clim_smry/', full.names = T))
length(files)
agb.clim.df <- do.call("rbind", lapply(files, read.csv, header=T))
agb.clim.df <- na.omit(agb.clim.df)
head(agb.clim.df)


# CORRELATE PLANT BIOMASS AND TAVG ACROSS MONTHS AND REPS -------------------------------------------------------------------

calc.tagb.cors <- function(df){
  cor.test(df$tavg, df$tagb.kgm2.avg, method='spearman')
}

calc.sagb.cors <- function(df){
  cor.test(df$tavg, df$sagb.kgm2.avg, method='spearman')
}

calc.sdom.cors <- function(df){
  cor.test(df$tavg, df$shrub.pcnt.avg, method='spearman')
}


cor.smry <- function(x){
  data.frame(r=x$estimate, p=x$p.value)
}

# compute correlations
tagb.cors <- dlply(agb.clim.df, .(month, rep), .fun = calc.tagb.cors)
tagb.cor.smry <- ldply(tagb.cors, function(x) cor.smry(x))
tagb.cor.smry$agb <- 'tagb'

sagb.cors <- dlply(agb.clim.df, .(month, rep), .fun = calc.sagb.cors)
sagb.cor.smry <- ldply(sagb.cors, function(x) cor.smry(x))
sagb.cor.smry$agb <- 'sagb'

sdom.cors <- dlply(agb.clim.df, .(month, rep), .fun = calc.sdom.cors)
sdom.cor.smry <- ldply(sdom.cors, function(x) cor.smry(x))
sdom.cor.smry$agb <- 'shrub.pcnt'

agb.cor.smry <- rbind(tagb.cor.smry, sagb.cor.smry, sdom.cor.smry)
head(agb.cor.smry)
# summarize correlations
agb.tavg.cor.smry <- agb.cor.smry %>% group_by(agb, month) %>% summarise(r.med=median(r),r.q975=quantile(r, 0.975), r.q025=quantile(r, 0.025))
agb.tavg.cor.smry
write.table(agb.tavg.cor.smry, '/projects/above_gedi/lberner/nslope_biomass/output/smry_agb_tavg_monthly_cor.csv', sep = ',', row.names = F, col.names = T)


# SUMMARIZE CORRELATIONS ACROSS MONTE CARLO SIMULATIONS ---------------------------------------------------------------
agb.clim.smry <- agb.clim.df %>% group_by(tavg, month) %>% summarise(area.km2=median(area.km2), 
                                                                     tagb.kgm2.avg.med=median(tagb.kgm2.avg),tagb.kgm2.avg.q975=quantile(tagb.kgm2.avg, 0.975), tagb.kgm2.avg.q025=quantile(tagb.kgm2.avg, 0.025),
                                                                     tagb.kgm2.avg.q25=quantile(tagb.kgm2.avg, 0.25), tagb.kgm2.avg.q75=quantile(tagb.kgm2.avg, 0.750),
                                                               sagb.kgm2.avg.med=median(sagb.kgm2.avg),sagb.kgm2.avg.q975=quantile(sagb.kgm2.avg, 0.975), sagb.kgm2.avg.q025=quantile(sagb.kgm2.avg, 0.025),
                                                               sagb.kgm2.avg.q25=quantile(sagb.kgm2.avg, 0.25), sagb.kgm2.avg.q75=quantile(sagb.kgm2.avg, 0.75),
                                                               shrub.pcnt.avg.med=median(shrub.pcnt.avg),shrub.pcnt.avg.q975=quantile(shrub.pcnt.avg, 0.975), shrub.pcnt.avg.q025=quantile(shrub.pcnt.avg, 0.025),
                                                               shrub.pcnt.avg.q25=quantile(shrub.pcnt.avg, 0.25), shrub.pcnt.avg.q75=quantile(shrub.pcnt.avg, 0.75),
                                                               tagb.Tg.tot.med=median(tagb.Tg.tot),tagb.Tg.tot.q975=quantile(tagb.Tg.tot, 0.975), tagb.Tg.tot.q025=quantile(tagb.Tg.tot, 0.025),
                                                               sagb.Tg.tot.med=median(sagb.Tg.tot),sagb.Tg.tot.q975=quantile(sagb.Tg.tot, 0.975), sagb.Tg.tot.q025=quantile(sagb.Tg.tot, 0.025))

agb.clim.smry

# write out summary table
write.table(agb.clim.smry, '/projects/above_gedi/lberner/nslope_biomass/output/smry_agb_tavg_monthly.csv', sep = ',', row.names = F, col.names = T)

# END SCRIPT ---------------------------------------------------------------------------------------------------------