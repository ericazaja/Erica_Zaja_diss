# This R script computes summaries of mean and total tundra aboveground biomass by landcover type across the Monte Carlo simulations.

rm(list=ls())
require(dplyr)


files <- list.files('/scratch/lb968/agb_mc_iterations/landcov_smry/', full.names = T)

landcov.df <- do.call("rbind", lapply(files, read.csv, header=T))
head(landcov.df)
write.table(landcov.df, '/projects/above_gedi/lberner/nslope_biomass/output/smry_agb_by_landcov_mc_reps.csv', sep = ',', row.names = F, col.names = T)


# SUMMARIZE BIOMASS BY LAND COVER TYPE ---------------------------------------------------------------
landcov.smry <- landcov.df %>% group_by(landcov) %>% summarise(tagb.kgm2.avg.med=median(tagb.kgm2.avg),tagb.kgm2.avg.q975=quantile(tagb.kgm2.avg, 0.975), tagb.kgm2.avg.q025=quantile(tagb.kgm2.avg, 0.025),
                                               sagb.kgm2.avg.med=median(sagb.kgm2.avg),sagb.kgm2.avg.q975=quantile(sagb.kgm2.avg, 0.975), sagb.kgm2.avg.q025=quantile(sagb.kgm2.avg, 0.025),
                                               shrub.pcnt.avg.med=median(shrub.pcnt.avg),shrub.pcnt.avg.q975=quantile(shrub.pcnt.avg, 0.975), shrub.pcnt.avg.q025=quantile(shrub.pcnt.avg, 0.025),
                                               tagb.Tg.tot.med=median(tagb.Tg.tot),tagb.Tg.tot.q975=quantile(tagb.Tg.tot, 0.975), tagb.Tg.tot.q025=quantile(tagb.Tg.tot, 0.025),
                                               sagb.Tg.tot.med=median(sagb.Tg.tot),sagb.Tg.tot.q975=quantile(sagb.Tg.tot, 0.975), sagb.Tg.tot.q025=quantile(sagb.Tg.tot, 0.025),
                                               sagb2tagb.pcnt.med=median(sagb2tagb.tot.pcnt),sagb2tagb.pcnt.q975=quantile(sagb2tagb.tot.pcnt, 0.975), sagb2tagb.pcnt.q025=quantile(sagb2tagb.tot.pcnt, 0.025))
                                                               
landcov.smry

# write out summary table
outname <- paste('/projects/above_gedi/lberner/nslope_biomass/output/smry_agb_by_landcov.csv', sep=',')
write.table(landcov.smry, outname, sep = ',', row.names = F, col.names = T)


# PRETTY TABLE FOR THE PAPER ------------------------------------------------------------------------
head(landcov.smry)

landcov.smry[,2:9] <- round(landcov.smry[,2:9],2)
landcov.smry[,10:16] <- round(landcov.smry[,10:16],1)

landcov.smry.pretty <- data.frame(model=landcov.smry$landcov, 
                               tagb.kgm2=with(landcov.smry, paste(tagb.kgm2.avg.med, " [",tagb.kgm2.avg.q025,",",tagb.kgm2.avg.q975,"]", sep='' )),
                               sagb.kgm2=with(landcov.smry, paste(sagb.kgm2.avg.med, " [",sagb.kgm2.avg.q025,",",sagb.kgm2.avg.q975,"]", sep='' )),
                               tagb.Tg=with(landcov.smry, paste(tagb.Tg.tot.med, " [",tagb.Tg.tot.q025,",",tagb.Tg.tot.q975,"]", sep='' )),
                               sagb.Tg=with(landcov.smry, paste(sagb.Tg.tot.med, " [",sagb.Tg.tot.q025,",",sagb.Tg.tot.q975,"]", sep='' )),
                               shrub.pcnt=with(landcov.smry, paste(shrub.pcnt.avg.med, " [",shrub.pcnt.avg.q025,",",shrub.pcnt.avg.q975,"]", sep='' )))

landcov.smry.pretty

write.table(landcov.smry.pretty, 'output/nslope/smry_agb_by_landcov_pretty.csv', sep = ',',row.names = F, col.names = T)
