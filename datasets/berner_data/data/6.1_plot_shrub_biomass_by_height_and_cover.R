rm(list=ls())
require(raster)
require(maptools)
require(plotrix)
require(lattice)
require(dplyr)
setwd('C:/research/projects/above/')

#------------------------------------------------------------------------------------------------------------
# LOAD FILES AND SET VARIABLES
#------------------------------------------------------------------------------------------------------------
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
aaea <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

sagb <- raster('gis_data/nslope/biomass/nslope_shrub_agb_gm2_p50.tif')
sagb.mac <- raster('gis_data/nslope/biomass/nslope_shrub_agb_gm2_p50_macanderExtent.tif')
shrub.cov <- raster('gis_data/nslope/nslope_shrub_cover_aea_30m_macander.tif')

shrub.survey <- read.csv('field_data/NACP_Woody_Veg_N_Slope_AK_V2_1365/data/shrub_dimensions.csv')
shrub.survey$canopy.height.m[shrub.survey$canopy.height.m == -999] <- NA
shrub.survey$crown.radius.m[shrub.survey$crown.radius.m == -999] <- NA

shrub.coords <- read.csv('field_data/NACP_Woody_Veg_N_Slope_AK_V2_1365/data/shrub_sites.csv')
shrub.coords <- subset(shrub.coords, Site != 'Dalton201112')

#------------------------------------------------------------------------------------------------------------
# SUMMARIZE SHRUB DIMENSIONS FOR EACH FIELD SITE,  MAP SITE LOCATIONS AND EXTRACT AGB
#------------------------------------------------------------------------------------------------------------
head(shrub.survey)
shrub.survey$canopy.vol.m3 <- (pi*shrub.survey$crown.radius.m^2)*shrub.survey$canopy.height.m/3 # from Tape et al. 2016 GCB

shrub.sites <- shrub.survey %>% group_by(site) %>% summarise(canopy.height.m.avg=mean(canopy.height.m, na.rm=T), canopy.height.m.sd=sd(canopy.height.m, na.rm=T),
                                                             crown.radius.m.avg=mean(crown.radius.m, na.rm=T), crown.radius.m.sd=sd(crown.radius.m, na.rm=T), 
                                                             canopy.vol.m3.avg=mean(canopy.vol.m3, na.rm=T), canopy.vol.m3.sd=sd(canopy.vol.m3, na.rm=T), 
                                                             canopy.vol.m3.tot=sum(canopy.vol.m3, na.rm=T), n.samples= n())%>% 
  mutate(canopy.height.m.se = canopy.height.m.sd/sqrt(n.samples), 
         crown.radius.m.se=crown.radius.m.sd/sqrt(n.samples), canopy.vol.m3.se=canopy.vol.m3.sd/sqrt(n.samples))

shrub.sites$x.coord <- shrub.coords$x.coord
shrub.sites$y.coord <- shrub.coords$y.coord
shrub.pts <- SpatialPointsDataFrame(coords =shrub.sites[,c(13,14)], shrub.sites, proj4string = aaea)

shrub.sites$sagb.kgm2.avg <- extract(sagb, shrub.pts, buffer=100, fun=mean, na.rm=T)/1000
shrub.sites$sagb.kgm2.sd <- extract(sagb, shrub.pts, buffer=100, fun=sd, na.rm=T)/1000
shrub.sites$sagb.kgm2.n <- extract(sagb, shrub.pts, buffer=100, fun=length, na.rm=T)
shrub.sites$sagb.kgm2.se <- shrub.sites$sagb.kgm2.sd/sqrt(shrub.sites$sagb.kgm2.n)

#------------------------------------------------------------------------------------------------------------
# EXTRACT SHRUB AGB AND CANOPY COVER VALUES TO DATA FRAME, THEN SUBSAMPLE
#------------------------------------------------------------------------------------------------------------
sagb.cov.df <- data.frame(cov=values(shrub.cov), sagb.kgm2=values(sagb.mac)/1000)
sagb.cov.df <- na.omit(sagb.cov.df)
nrow(sagb.cov.df)
sagb.cov.df.smp <- sagb.cov.df[sample(1:nrow(sagb.cov.df), 2000),]

#------------------------------------------------------------------------------------------------------------
# CORRELATE MODELED SHRUB AGB vs SHRUB HEIGHT CANOPY COVER
#------------------------------------------------------------------------------------------------------------

# modeled shrub AGB vs canopy height
cor.sagb.height <- cor.test(shrub.sites$canopy.height.m.avg, shrub.sites$sagb.kgm2.avg, method = 'spearman')
cor.sagb.height

cor.sagb.cov <- cor.test(sagb.cov.df.smp$sagb.kgm2, sagb.cov.df.smp$cov, method = 'spearman')
cor.sagb.cov

#------------------------------------------------------------------------------------------------------------
# PLOT MODELES SHRUB AGB VS MEASURED SHRUB HEIGHT
#------------------------------------------------------------------------------------------------------------

agb.ylab  <- expression('Shrub aboveground biomass (kg m '^-2*')')
height.xlab <- "Shrub canopy height (m)" 
cov.xlab <- "Shrub canopy cover (%)"

my.pch.cex=1.2
my.cex.axis=1.2

cor.sagb.height.r <- bquote(r[s]*' = '*~.(round(as.numeric(cor.sagb.height$estimate),2)))
cor.sagb.cov.r <- bquote(r[s]*' = '*~.(round(as.numeric(cor.sagb.cov$estimate),2)))




jpeg('C:/Users/lb968/Google Drive/research/nau/above/figures/nslope_shrub_AGB_vs_height_cover.jpeg', width = 5, height = 9, res = 300, units = 'in')

par.op <- par(mfrow=c(2,1))
par.top <- par(mar=c(4,4.75,1,1))
plotCI(shrub.sites$canopy.height.m.avg, shrub.sites$sagb.kgm2.avg, uiw=shrub.sites$canopy.height.m.se, err='x', xaxt='n', yaxt='n', 
       xlim=c(0.5,2.2), ylim=c(0,1.55),xlab = '', ylab='',cex = my.cex.axis, pch=16, scol='grey50')
plotCI(shrub.sites$canopy.height.m.avg, shrub.sites$sagb.kgm2.avg, uiw=shrub.sites$sagb.kgm2.se, err='y', xaxt='n', yaxt='n', 
       xlim=c(0.5,2.2), ylim=c(0,1.55),xlab = '', ylab='',cex = my.cex.axis, pch=16, scol='grey50', add=T)
axis(1, at = seq(0.5,2,0.5), cex.axis=my.cex.axis, labels = T)
axis(2, seq(0,1.5,0.5), las=2, cex.axis=my.cex.axis)
mtext(side = 1, line = 3.0, cex = my.pch.cex, height.xlab)
mtext(side = 2, line = 3.0, cex = my.pch.cex, agb.ylab)
text(0.6, 1.50, "(a)", cex=my.cex.axis, font=2)
text(0.7, 1.25, cor.sagb.height.r, cex=my.cex.axis)
text(0.7, 1.10, "P < 0.001", cex=my.cex.axis)
par(par.top)

par.bot <- par(mar=c(4,4.75,1,1))
plot(sagb.kgm2 ~ cov, sagb.cov.df.smp, pch='*', xlim=c(0,100), ylim=c(0,1.55), xlab='', ylab='', xaxt='n', yaxt='n')
axis(1, at = seq(0,100,25), cex.axis=my.cex.axis, labels = T)
axis(2, seq(0,1.5,0.5), las=2, cex.axis=my.cex.axis)
mtext(side = 1, line = 3.0, cex = my.pch.cex, cov.xlab)
mtext(side = 2, line = 3.0, cex = my.pch.cex, agb.ylab)
text(3, 1.50, "(b)", cex=my.cex.axis, font=2)
text(10, 1.25, cor.sagb.cov.r, cex=my.cex.axis)
text(10, 1.10, "P < 0.001", cex=my.cex.axis)
par(par.bot)
par(par.op)

dev.off()