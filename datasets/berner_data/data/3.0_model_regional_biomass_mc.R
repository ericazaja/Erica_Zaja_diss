# This R script fits exporential regression models that related tundra plant and shrub aboveground biomass 
# to Landsat peak summer NDVI. Plant and shrub aboveground biomass are then modeled across the 
# Alaskan North Slope using these regression models and regional Landsta NDVI composite mosaics. 
# This modeling is done ina Monte Carlo framework with 1,000 simulations that all for characterizing/propegating uncertainty.

rm(list=ls())
require(raster)
require(dplyr)
require(R.utils)
source('/home/lb968/code/function/0_fun_exponential_regression.R')
options(error=function(){print('Model not fit; fit again')})

# get slurm job array number
args <- commandArgs(TRUE)
i = as.numeric(args[1])

# set tmp directory 
tmp.dir.name <- paste('/scratch/lb968/Rtmp/agb_r',i,sep='')
mkdirs(tmp.dir.name)
rasterOptions(tmpdir = tmp.dir.name)

# load field data for modeling fitting and regional ndvi raster
field.agb <- read.csv('/projects/above_gedi/lberner/nslope_biomass/field_data/tundra_biomass_harvests.csv')
ndvi <- raster(list.files('/scratch/lb968/agb_mc_iterations/ndvi/', full.names = T)[i])


# FIT REGRESSION RELATING AGB TO NDVI ----------------------------------------------------------
a <- rnorm(nrow(field.agb))
b <- rnorm(nrow(field.agb))

df <- data.frame(tagb.kgm2.avg=with(field.agb, total.agb.kgm2.avg+a*total.agb.kgm2.se),
                 sagb.kgm2.avg=with(field.agb, shrub.agb.kgm2.avg+a*shrub.agb.kgm2.se),
                 ndvi.50m.avg=with(field.agb, ndvi.50m.avg+b*(ndvi.50m.sd/sqrt(ndvi.50m.cnt))))
  
tagb.nls <- fit.exp(df$ndvi.50m.avg*1000, df$tagb.kgm2.avg)
sagb.nls <- fit.exp(df$ndvi.50m.avg*1000, df$sagb.kgm2.avg)

# check that both models were fit; if one wasn't, then refit both models
if (exists("tagb.nls") == F | exists("sagb.nls") == F){
  
  a <- rnorm(nrow(field.agb))
  b <- rnorm(nrow(field.agb))
  
  df <- data.frame(tagb.kgm2.avg=with(field.agb, total.agb.kgm2.avg+a*total.agb.kgm2.se),
                   sagb.kgm2.avg=with(field.agb, shrub.agb.kgm2.avg+a*shrub.agb.kgm2.se),
                   ndvi.50m.avg=with(field.agb, ndvi.50m.avg+b*(ndvi.50m.sd/sqrt(ndvi.50m.cnt))))
  
  tagb.nls <- fit.exp(df$ndvi.50m.avg*1000, df$tagb.kgm2.avg)
  sagb.nls <- fit.exp(df$ndvi.50m.avg*1000, df$sagb.kgm2.avg)
}

nls.coef <- cbind(data.frame(model=c('tagb','sagb')), rbind(tagb.nls$coefs, sagb.nls$coefs))
nls.coef

nls.outname <- paste('/scratch/lb968/agb_mc_iterations/nls_models/nslope_agb_kgm2_vs_ndvix1000_rep_', i, '.csv', sep='')

write.table(nls.coef, nls.outname, sep = ',', row.names = F, col.names = T)


# PREDICT PLANT BIOMASS -------------------------------------------------------------------------------------------

# set output name 
tagb.outname <- paste('/scratch/lb968/agb_mc_iterations/tagb/nslope_tagb_gm2_30m_rep_', i, '.tif', sep='')
sagb.outname <- paste('/scratch/lb968/agb_mc_iterations/sagb/nslope_sagb_gm2_30m_rep_', i, '.tif', sep='')
sdom.outname <- paste('/scratch/lb968/agb_mc_iterations/shrub_dominance/nslope_shrub_pcnt_of_tagb_rep_',i, '.tif', sep='')

# PREDICT ABOVEGROUND BIOMASS
tagb <- tagb.nls$my.nlm.summary$coefficients[1] * exp(tagb.nls$my.nlm.summary$coefficients[2] * ndvi) * 1000 # kg/m2 to g/m2
print('computed TAGB')
sagb <- sagb.nls$my.nlm.summary$coefficients[1] * exp(sagb.nls$my.nlm.summary$coefficients[2] * ndvi) * 1000 # kg/m2 to g/m2
print('computed SAGB')
shrub.pcnt <- sagb/tagb * 100
shrub.pcnt[shrub.pcnt>100] <- 100
print('computed SDOM')

# ROUND OFF TO NEAREST INTEGER
tagb <- round(tagb) 
sagb <- round(sagb) 
shrub.pcnt <- round(shrub.pcnt)

# WRITE OUTPUT TO DISK
writeRaster(tagb, tagb.outname, datatype='INT2U', overwrite=T)
writeRaster(sagb, sagb.outname, datatype='INT2U', overwrite=T)
writeRaster(shrub.pcnt, sdom.outname, datatype='INT1U', overwrite=T)

# clean up
gc()


# CHECK RASTER FOR WRITE ERRORS ------------------------------------------------------------------------------------------
# Specifically, check whether output raster has too many NA valuas and, if so, output an error log
tagb <- raster(tagb.outname)  
sagb <- raster(sagb.outname)  
sdom <- raster(sdom.outname)  

tagb[tagb >= 0] <- 1
tagb.n.valid.pxls <- cellStats(tagb, stat = sum)

sagb[sagb >= 0] <- 1
sagb.n.valid.pxls <- cellStats(sagb, stat = sum)

sdom[sdom >= 0] <- 1
sdom.n.valid.pxls <- cellStats(sdom, stat = sum)

n.valid <- 169711147 # number of non-NA values in the regional NDVI mosaic

if (tagb.n.valid.pxls != n.valid | sagb.n.valid.pxls != n.valid | sdom.n.valid.pxls != n.valid){
  outname <- paste('/scratch/lb968/Rtmp/agb_nodata_errors/too_many_na_in_rep_',i,'.txt', sep='')
  pxl.cnts <- data.frame(rep=i, tagb.n=tagb.n.valid.pxls, sagb.n=sagb.n.valid.pxls, sdom.n=sdom.n.valid.pxls)
  write.csv(pxl.cnts, outname)
}

# delete tmp folder
unlink(tmp.dir.name, recursive = T)

# END SCRIPT --------------------------------------------------------------------------------------------------------------