# This R script summarizes the Monte Carlo simulations used when modeling plant and shrub aboveground biomass across 
# the Alaskan North Slope. Specifically, it takes the 1,000 region-wide simulations and derives pixel-wise 
# median, 2.5 percencile, and 97.5 percentile for each biomass data set. Note: This script is very slow and requires lot of memory! 

rm(list=ls())
require(raster)


# PLANT ABOVEGROUND BIOMASS ---------------------------------------------------------------------------------
tagb.files <- list.files('/scratch/lb968/agb_mc_iterations/tagb', full.names = T) #[1:10]
tagb.stk <- raster::stack(tagb.files)
msk <- raster('/projects/above_gedi/lberner/nslope_biomass/gis_data/nslope_nowater_30m_aaea.tif')

tagb.med <- stackApply(tagb.stk, indices = rep(1,nlayers(tagb.stk)), fun = function(x,...){median(x, na.rm = T)})
tagb.med <- round(tagb.med)
tagb.med <- raster::mask(tagb.med, msk)
writeRaster(tagb.med, '/projects/above_gedi/lberner/nslope_biomass/gis_data/biomass/nslope_tagb_gm2_mc_med.tif', datatype='INT2U', overwrite=T)
print('tagb median')
rm(tagb.med)

tagb.q975 <- stackApply(tagb.stk, indices = rep(1,nlayers(tagb.stk)), fun = function(x,...){quantile(x, probs = 0.975, na.rm = T)})
tagb.q975 <- round(tagb.q975)
tagb.q975 <- raster::mask(tagb.q975, msk)
writeRaster(tagb.q975, '/projects/above_gedi/lberner/nslope_biomass/gis_data/biomass/nslope_tagb_gm2_mc_q975.tif', datatype='INT2U', overwrite=T)
print('tagb q 0.975')
rm(tagb.q975)

tagb.q025 <- stackApply(tagb.stk, indices = rep(1,nlayers(tagb.stk)), fun = function(x,...){quantile(x, probs = 0.025, na.rm = T)})
tagb.q025 <- round(tagb.q025)
tagb.q025 <- raster::mask(tagb.q025, msk)
writeRaster(tagb.q025, '/projects/above_gedi/lberner/nslope_biomass/gis_data/biomass/nslope_tagb_gm2_mc_q025.tif', datatype='INT2U', overwrite=T)
print('tagb q 0.025')
rm(tagb.q025)


# SHRUB ABOVEGROUND BIOMASS ----------------------------------------------------------------------------
sagb.files <- list.files('/scratch/lb968/agb_mc_iterations/sagb', full.names = T) #[1:10]
sagb.stk <- raster::stack(sagb.files)
msk <- raster('/projects/above_gedi/lberner/nslope_biomass/gis_data/nslope_nowater_30m_aaea.tif')

sagb.med <- stackApply(sagb.stk, indices = rep(1,nlayers(sagb.stk)), fun = function(x,...){median(x, na.rm = T)})
sagb.med <- round(sagb.med)
sagb.med <- raster::mask(sagb.med, msk)
writeRaster(sagb.med, '/projects/above_gedi/lberner/nslope_biomass/gis_data/biomass/nslope_sagb_gm2_mc_med.tif', datatype='INT2U', overwrite=T)
print('sagb median')
rm(sagb.med)
gc()

sagb.q975 <- stackApply(sagb.stk, indices = rep(1,nlayers(sagb.stk)), fun = function(x,...){quantile(x, probs = 0.975, na.rm = T)})
sagb.q975 <- round(sagb.q975)
sagb.q975 <- raster::mask(sagb.q975, msk)
writeRaster(sagb.q975, '/projects/above_gedi/lberner/nslope_biomass/gis_data/biomass/nslope_sagb_gm2_mc_q975.tif', datatype='INT2U', overwrite=T)
print('sagb q 0.975')
rm(sagb.q975)

sagb.q025 <- stackApply(sagb.stk, indices = rep(1,nlayers(sagb.stk)), fun = function(x,...){quantile(x, probs = 0.025, na.rm = T)})
sagb.q025 <- round(sagb.q025)
sagb.q025 <- raster::mask(sagb.q025, msk)
writeRaster(sagb.q025, '/projects/above_gedi/lberner/nslope_biomass/gis_data/biomass/nslope_sagb_gm2_mc_q025.tif', datatype='INT2U', overwrite=T)
print('sagb q 0.025')
rm(sagb.q025)


# Shrub dominance ------------------------------------------------------------------------
shrubpcnt.files <- list.files('/scratch/lb968/agb_mc_iterations/shrub_dominance/', full.names = T) #[1:10]
shrubpcnt.stk <- raster::stack(shrubpcnt.files)
msk <- raster('/projects/above_gedi/lberner/nslope_biomass/gis_data/nslope_nowater_30m_aaea.tif')

shrubpcnt.med <- stackApply(shrubpcnt.stk, indices = rep(1,nlayers(shrubpcnt.stk)), fun = function(x,...){median(x, na.rm = T)})
shrubpcnt.med <- round(shrubpcnt.med)
shrubpcnt.med <- mask(shrubpcnt.med, msk)
writeRaster(shrubpcnt.med, '/projects/above_gedi/lberner/nslope_biomass/gis_data/biomass/nslope_shrub_pcnt_mc_med.tif', datatype='INT2U', overwrite=T)
print('shrubpcnt median')
rm(shrubpcnt.med)

shrubpcnt.q975 <- stackApply(shrubpcnt.stk, indices = rep(1,nlayers(shrubpcnt.stk)), fun = function(x,...){quantile(x, probs = 0.975, na.rm = T)})
shrubpcnt.q975 <- round(shrubpcnt.q975)
shrubpcnt.q975 <- mask(shrubpcnt.q975, msk)
writeRaster(shrubpcnt.q975, '/projects/above_gedi/lberner/nslope_biomass/gis_data/biomass/nslope_shrub_pcnt_mc_q975.tif', datatype='INT2U', overwrite=T)
print('shrubpcnt q 0.975')
rm(shrubpcnt.q975)

shrubpcnt.q025 <- stackApply(shrubpcnt.stk, indices = rep(1,nlayers(shrubpcnt.stk)), fun = function(x,...){quantile(x, probs = 0.025, na.rm = T)})
shrubpcnt.q025 <- round(shrubpcnt.q025)
shrubpcnt.q025 <- mask(shrubpcnt.q025, msk)
writeRaster(shrubpcnt.q025, '/projects/above_gedi/lberner/nslope_biomass/gis_data/biomass/nslope_shrub_pcnt_mc_q025.tif', datatype='INT2U', overwrite=T)
print('shrubpcnt q 0.025')
rm(shrubpcnt.q025)

# END SCRIPT ------------------------------------------------------------------------------