# This R script computes mean and total tundra aboveground biomass along spatial gradients in mean monthtly air temperature
# across the Alaskan North Slope. The script is run once for each of the 1,000 Monte Carlo simulations.

rm(list=ls())
require(raster)
require(data.table)
require(R.utils)


# get slurm job array number
args <- commandArgs(TRUE)
i = as.numeric(args[1])

# set tmp directory 
tmp.dir.name <- paste('/scratch/lb968/Rtmp/agb_clim_r',i,sep='')
mkdirs(tmp.dir.name)
rasterOptions(tmpdir = tmp.dir.name)

# specify clim files and load biomass
tavg.files <- list.files('/projects/above_gedi/lberner/nslope_biomass/gis_data/climate/wordclim_v2/', pattern = '*tavg*', full.names = T)
tagb <- raster(list.files('/scratch/lb968/agb_mc_iterations/tagb/', full.names = T)[i])
sagb <- raster(list.files('/scratch/lb968/agb_mc_iterations/sagb/', full.names = T)[i])
sdom <- raster(list.files('/scratch/lb968/agb_mc_iterations/shrub_dominance/', full.names = T)[i])


month.num <- c('01','02','03','04','05','06','07','08','09','10','11','12')


# pull out biomass values
dt <- data.table(tagb.gm2=values(tagb), sagb.gm2=values(sagb), shrub.pcnt=values(sdom))
print('grabbed AGB vals')


# Extract monthly climate and summarize biomass along gradient ---------------------------------

for (j in 1:12){
  tavg <- raster(tavg.files[j])
  #tavg <- crop(tavg, aoi)
  dt$tavg <- values(tavg)
  
  smry <- dt[is.na(tavg)==F & is.na(tagb.gm2)==F & is.na(sagb.gm2)==F, .(area.km2=.N*900/10^6, tagb.kgm2.avg=mean(tagb.gm2)/1000, sagb.kgm2.avg=mean(sagb.gm2)/1000, shrub.pcnt.avg=round(mean(shrub.pcnt)),
                               tagb.Tg.tot=sum(tagb.gm2*900/10^12, na.rm=T), sagb.Tg.tot=sum(sagb.gm2*900/10^12, na.rm=T)),
             keyby=.(tavg=round(tavg/10,1))]
  smry$month <- j
  smry$rep <- i
  # write out summary table
  outname <- paste('/scratch/lb968/agb_mc_iterations/clim_smry/nslope_agb_by_tavg_', month.num[j], '_rep_', i, '.csv', sep='')
  write.table(smry, outname, sep = ',', row.names = F, col.names = T)
  print(paste('finished', j, sep = ' '))
}


# Extract SWI and summarize biomass along gradient ----------------------------------------
swi <- raster('/projects/above_gedi/lberner/nslope_biomass/gis_data/climate/wordclim_v2/nslope_worldclim2_tavg_swi_times10.tif')
#swi <- crop(swi, aoi)
dt$swi <- values(swi)
    
smry <- dt[is.na(swi)==F & is.na(tagb.gm2)==F & is.na(sagb.gm2)==F, .(area.km2=.N*900/10^6, tagb.kgm2.avg=mean(tagb.gm2)/1000, sagb.kgm2.avg=mean(sagb.gm2)/1000, shrub.pcnt.avg=round(mean(shrub.pcnt)),
                                                                         tagb.Tg.tot=sum(tagb.gm2*900/10^12, na.rm=T), sagb.Tg.tot=sum(sagb.gm2*900/10^12, na.rm=T)),
             keyby=.(tavg=round(swi/10))]
smry$month <- 'swi'
smry$rep <- i
  
# write out summary table
outname <- paste('/scratch/lb968/agb_mc_iterations/clim_smry/nslope_agb_by_swi_rep_', i, '.csv', sep='')
write.table(smry, outname, sep = ',', row.names = F, col.names = T)
print('finished swi')

# delete tmp folder
unlink(tmp.dir.name, recursive = T)

# END SCRIPT ----------------------------------------------------------------------