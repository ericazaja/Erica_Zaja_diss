#rm(list=ls())
require(lattice)
require(reshape2)
require(plotrix)
source('C:/Users/Logan/Google Drive/research/nau/nsf_arctic/nslope_biomass/code/fun_fit_exp.R')
setwd('C:/Users/Logan/Google Drive/research/nau/nsf_arctic/nslope_biomass/')

dat <- read.csv('data/field_data/tundra_biomass_harvests.csv')
nls.coef <- read.csv('D:/projects/above/output/model_coef_agb_vs_ndvi_smry.csv')

head(dat)
dat$ndvi.50m.se <- dat$ndvi.50m.sd/sqrt(dat$ndvi.50m.cnt)

cor.test(dat$total.agb.kgm2.avg, dat$ndvi.100m.avg, method = 'spearman')
cor.test(dat$total.agb.kgm2.avg, dat$ndvi.50m.avg, method = 'spearman')

cor.test(dat$shrub.agb.kgm2.avg, dat$ndvi.100m.avg, method = 'spearman')
cor.test(dat$shrub.agb.kgm2.avg, dat$ndvi.50m.avg, method = 'spearman')

nls.coef
#-------------------------------------------------------------------------------------------
# PLOT
#-------------------------------------------------------------------------------------------

plant.ylab  <- expression('Aboveground biomass (kg m'^-2*')')
shrub.ylab  <- expression('Aboveground biomass (kg m'^-2*')')
ndvi.xlab <- "Landsat peak summer NDVI (unitless)" 

plant.r2 <- bquote(r^2*' = '*~.(nls.coef$r2.med[2]))
shrub.r2 <- bquote(r^2*' = '*~.(nls.coef$r2.med[1]))

plant.eqn <- bquote('y = '*~.(nls.coef$a.med[2])*e^(.(nls.coef$b.med[2])*x))
shrub.eqn <- bquote('y = '*~.(nls.coef$a.med[1])*e^(.(nls.coef$b.med[1])*x))

ndvi.seq <- seq(min(dat$ndvi.50m.avg), max(dat$ndvi.50m.avg), 0.01)

tagb.preds <- data.frame(ndvi=ndvi.seq, agb=nls.coef$a.med[2]*exp(nls.coef$b.med[2]*ndvi.seq))
sagb.preds <- data.frame(ndvi=ndvi.seq, agb=nls.coef$a.med[1]*exp(nls.coef$b.med[1]*ndvi.seq))

my.pch.cex=1.7
my.cex.axis=1.7

jpeg('figures/nslope_biomass/Plant_biomass_vs_summer_NDVI.jpeg', width = 6, height = 9, res = 500, units = 'in')
#pdf('figures/nslope_biomass/Plant_biomass_vs_summer_NDVI.pdf', width = 6, height = 9)

par.op <- par(mfrow=c(2,1))
par.top <- par(mar=c(4,4.75,1,1))

# TOTAL PLANT AGB VS NDVI
plotCI(dat$ndvi.50m.avg, dat$total.agb.kgm2.avg, uiw=dat$total.agb.kgm2.se, err='y', xaxt='n', yaxt='n', 
       xlim=c(0,1), ylim=c(0,3.3),xlab = '', ylab='',cex = my.cex.axis, pch=16, scol='grey50')
plotCI(dat$ndvi.50m.avg, dat$total.agb.kgm2.avg, uiw=dat$ndvi.50m.se, err='x', xaxt='n', yaxt='n', 
       xlim=c(0,1), ylim=c(0,3.3),xlab = '', ylab='',cex = my.cex.axis, pch=16, scol='grey50', add=T)
axis(1, at = seq(0.1,1,0.2), cex.axis=my.cex.axis, labels = T)
axis(2, seq(0,3,1), las=2, cex.axis=my.cex.axis)
mtext(side = 1, line = 2.9, cex = my.pch.cex, ndvi.xlab)
mtext(side = 2, line = 2.7, cex = my.pch.cex, plant.ylab)
text(0.05,3.0,"(a)", cex=my.cex.axis, font=2)
text(0.5,3.0,"Plant", cex=my.cex.axis, font=2)
text(0.25, 2.0, plant.eqn, cex=my.cex.axis)
text(0.25, 1.5, plant.r2, cex=my.cex.axis)
lines(tagb.preds$ndvi, tagb.preds$agb, lwd=1.5, col='black', lty=2)
par(par.top)


par.bot <- par(mar=c(4,4.75,1,1))
# SHRUB AGB VS NDVI
plotCI(dat$ndvi.50m.avg, dat$shrub.agb.kgm2.avg, uiw=dat$shrub.agb.kgm2.se, err='y', xaxt='n', yaxt='n', 
       xlim=c(0,1), ylim=c(0,3.3),xlab = '', ylab='',cex = my.cex.axis, pch=16, scol='grey50')
plotCI(dat$ndvi.50m.avg, dat$shrub.agb.kgm2.avg, uiw=dat$ndvi.50m.se, err='x', xaxt='n', yaxt='n', 
       xlim=c(0,1), ylim=c(0,3.3),xlab = '', ylab='',cex = my.cex.axis, pch=16, scol='grey50', add=T)
axis(1, at = seq(0.1,1,0.2), cex.axis=my.cex.axis, labels = T)
axis(2, seq(0,3,1), las=2, cex.axis=my.cex.axis)
mtext(side = 1, line = 2.9, cex = my.pch.cex, ndvi.xlab)
mtext(side = 2, line = 2.7, cex = my.pch.cex, shrub.ylab)
text(0.05,3.0,"(b)", cex=my.cex.axis, font=2)
text(0.55,3.0,"Shrub", cex=my.cex.axis, font=2)
text(0.25, 2.0, shrub.eqn, cex=my.cex.axis)
text(0.25, 1.5, shrub.r2, cex=my.cex.axis)
lines(sagb.preds$ndvi, sagb.preds$agb, lwd=1.5, col='black', lty=2)
par(par.bot)
dev.off()
