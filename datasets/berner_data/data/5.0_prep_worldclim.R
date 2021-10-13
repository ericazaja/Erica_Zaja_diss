# This R prepares monthly WorldClimate v2 temperature data for the Alaskan North Slope and computes the summer warmth index.
rm(list=ls())
require(R.utils)
require(raster)
require(maptools)

setwd('/projects/above_gedi/')
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
aaea <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

nslope.poly.wgs84 <- readShapePoly('lberner/nslope_biomass/gis_data/nslope_wgs84', proj4string = wgs84)
tmplt.aaea.30m <- raster('lberner/nslope_biomass/gis_data/nslope_30m_aaea.tif')
dir <- 'geodata/climate/wordclim_v2/wc2.0_10m_tavg/'

# LOOP THROUGHT MONTHLY CLIMATOLOGIES ---------------------------------------------------------------
mon.num <- c('01','02','03','04','05','06','07','08','09','10','11','12')
for (i in mon.num){
  files <- list.files(dir, full.names = T)
  r.wgs84 <- raster(files[i])
  r.wgs84.nsl <- crop(r.wgs84, nslope.poly.wgs84) # crop to north slope
  r.aaea.nsl.1km <- projectRaster(r.wgs84.nsl, res = 1000, crs = aaea) # project from wgs84 to aaea
  r.aaea.nsl.30m <- resample(r.aaea.nsl.1km, tmplt.aaea.30m)
  r.aaea.nsl.30m <- mask(r.aaea.nsl.30m, tmplt.aaea.30m)
  r.aaea.nsl.30m <- round(r.aaea.nsl.30m*10)
  outname <- paste('lberner/nslope_biomass/gis_data/climate/wordclim_v2/nslope_worldclim2_tavg_',mon.num[i],'_times10.tif', sep = '')
  writeRaster(r.aaea.nsl.30m, outname, datatype='INT2S', overwrite=TRUE)
}


# COMPUTE SUMMER WARMTH INDEX (ie., sum of mean monthly temperatures > 0) ----------------------------------
swi.months <- 5:9
tavg.files <- list.files('lberner/nslope_biomass/gis_data/climate/wordclim_v2/', pattern = glob2rx('*tavg*'), full.names = T)[swi.months] # may - sept- only monthly with tavg >0
tavg.stk <- stack(tavg.files)
tavg.stk[tavg.stk<0] <- 0
swi.r <- sum(tavg.stk)
writeRaster(swi.r, 'lberner/nslope_biomass/gis_data/climate/wordclim_v2/nslope_worldclim2_tavg_swi_times10.tif', overwrite=T, datatype='INT2U')
