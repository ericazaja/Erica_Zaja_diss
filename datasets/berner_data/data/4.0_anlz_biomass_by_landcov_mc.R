# This R script computes mean and total tundra aboveground biomass by land cover type across the Alaskan North Slope.
# It is designed to be repeatedly run as part of the Monte Carlo simulations. 

rm(list=ls())
require(raster)
require(dplyr)
require(R.utils)

# get slurm job array number
args <- commandArgs(TRUE)
i = as.numeric(args[1])

# set tmp directory 
tmp.dir.name <- paste('/scratch/lb968/Rtmp/agb_r',i,sep='')
mkdirs(tmp.dir.name)
rasterOptions(tmpdir = tmp.dir.name)

# load biomass and land cover
tagb <- raster(list.files('/scratch/lb968/agb_mc_iterations/tagb/', full.names = T)[i])
sagb <- raster(list.files('/scratch/lb968/agb_mc_iterations/sagb/', full.names = T)[i])
sdom <- raster(list.files('/scratch/lb968/agb_mc_iterations/shrub_dominance/', full.names = T)[i])
landcov <- raster('/projects/above_gedi/lberner/nslope_biomass/gis_data/landcov/nslope_ecosystems_jorgenson_vegtype_30m_aaea.tif')
landcov.key <- c('water','barren','sedge','tussock tundra','shrub tussock sedge mix','dwarf shrub','low shrub','forest')

# pull values from rasters into data frame
df <- data.frame(landcov=values(landcov), tagb.gm2=values(tagb), sagb.gm2=values(sagb), shrub.pcnt=values(sdom))
df <- na.omit(df)
df <- subset(df, landcov <=8)
df$landcov <- factor(df$landcov, labels=landcov.key)
head(df)

# summarize biomass by land cover
landcov.smry <- df %>% group_by(landcov) %>% summarise(rep=i, tagb.kgm2.avg=mean(tagb.gm2)/1000, sagb.kgm2.avg=mean(sagb.gm2)/1000, shrub.pcnt.avg=round(mean(shrub.pcnt)),
                                       tagb.Tg.tot=sum(tagb.gm2*900/10^12), sagb.Tg.tot=sum(sagb.gm2*900/10^12)) %>% mutate(sagb2tagb.avg.pcnt=sagb.kgm2.avg/tagb.kgm2.avg*100, sagb2tagb.tot.pcnt=sagb.Tg.tot/tagb.Tg.tot*100)


regional.smry <- df %>% group_by() %>% summarise(landcov='all', rep=i, tagb.kgm2.avg=mean(tagb.gm2)/1000, sagb.kgm2.avg=mean(sagb.gm2)/1000, shrub.pcnt.avg=round(mean(shrub.pcnt)),
                                tagb.Tg.tot=sum(tagb.gm2*900/10^12), sagb.Tg.tot=sum(sagb.gm2*900/10^12)) %>% mutate(sagb2tagb.avg.pcnt=sagb.kgm2.avg/tagb.kgm2.avg*100, sagb2tagb.tot.pcnt=sagb.Tg.tot/tagb.Tg.tot*100)

landcov.smry <- rbind(landcov.smry, regional.smry)

# write out summary table
outname <- paste('/scratch/lb968/agb_mc_iterations/landcov_smry/nslope_agb_by_landcov_rep_', i, '.csv', sep='')
write.table(landcov.smry, outname, sep = ',', row.names = F, col.names = T)

# delete tmp folder
unlink(tmp.dir.name, recursive = T)