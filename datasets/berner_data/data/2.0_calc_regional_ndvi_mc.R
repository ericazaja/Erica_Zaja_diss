# This R script computes NDVI from red and near-infrared (NIR) surface reflectance measurements made by Landsat across the Alaskan North Slope.
# This script was run 1,000 times as part of Monte Carlo simulations, where each iteration randomly permuted red and NIR before 
# computing NDVI. 

rm(list=ls())
require(raster)

red <- raster('/projects/above_gedi/lberner/nslope_biomass/gis_data/Lsat57_nslope_B3_surfRef_ndviP80_day170to240_2007to2016_qualMos_aaea.tif')
nir <- raster('/projects/above_gedi/lberner/nslope_biomass/gis_data/Lsat57_nslope_B4_surfRef_ndviP80_day170to240_2007to2016_qualMos_aaea.tif')

# get slurm job array number
args <- commandArgs(TRUE)
i = as.numeric(args[1])

# COMPUTE NDVI ------------------------------------------------------------
red.i = red + red * runif(1, -0.07, 0.07)
nir.i = nir + nir * runif(1, -0.07, 0.07)
ndvi = (nir.i-red.i)/(nir.i+red.i) * 1000 # the biomass-ndvi model is fit using ndvi x 1000
ndvi <- round(ndvi)
ndvi[ndvi<0] <- 0

outname <- paste('/scratch/lb968/agb_mc_iterations/ndvi/nslope_ndvi_rep_', i, '.tif', sep='')
writeRaster(ndvi, outname, datatype='INT2U', overwrite=T)
print('computed NDVI')

# END SCRIPT ---------------------------------------------------------------