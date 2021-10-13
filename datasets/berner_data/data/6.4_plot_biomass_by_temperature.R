rm(list=ls())
require(lattice)
require(latticeExtra)
require(reshape2)
require(plotrix)
require(grid)
setwd('C:/research/projects/above/')

#-------------------------------------------------------------------------------------------------------------
# READ IN AND SUBSET DATA SETS
#-------------------------------------------------------------------------------------------------------------
agb.tavg.df <- read.table('output/smry_agb_tavg_monthly.csv', sep = ',', header=T)
agb.tavg.df$month <- factor(agb.tavg.df$month, labels = c(month.abb,'SWI'), levels = c(1:12,'swi'))

agb.tavg.cor.df <- read.table('output/smry_agb_tavg_monthly_cor.csv', sep = ',', header=T)
agb.tavg.cor.df$month <- factor(agb.tavg.cor.df$month, labels = c(month.abb,'SWI'), levels = c(1:12,'swi'))

agb.tavg.june <- subset(agb.tavg.df, month == 'Jun' & tavg >= 2 & tavg <= 11)

head(agb.tavg.df)
head(agb.tavg.cor.df)
head(agb.tavg.june)
tail(agb.tavg.june)

#-------------------------------------------------------------------------------------------------------------
# SET UP AXIS LABELS, COLOLS, KEY, ETC
#-------------------------------------------------------------------------------------------------------------
cor.lab <- expression('Correlation (r '[s]~')')
agb.lab <- expression('Aboveground biomass (kg m '^-2~')')
tavg.lab <- expression('Mean June air temperature ('~degree~'C )')
spcnt.lab <- "Shrub dominance (%)"

err.cols=c('gray80','gray30')
#my.cols=c('tan','darkgreen','lightblue')
my.cols=c('tan','darkgreen','cyan3')

my.lty <- c(1,1)

#my.key <- list(title='', corner=c(0,0.98),text=list(c('plant AGB','shrub AGB','shrub pcnt')), rectangle=list(col=my.cols), cex=0.9, columns=1)
#my.key <- list(title='', corner=c(0.7,0.05),text=list(c('plant AGB','shrub AGB','shrub %')), rectangle=list(col=my.cols), cex=0.9, columns=1)
my.key <- list(title='', corner=c(0.98,0.01),text=list(c('plant AGB','shrub AGB','shrub %')), rectangle=list(col=my.cols), cex=0.7, columns=1)


#-------------------------------------------------------------------------------------------------------------
# CREATE PLOTS
#-------------------------------------------------------------------------------------------------------------

trellis.par.set(list(layout.widths=list(ylab.right=5, right.padding=0)))

#------------------------------------
# PLOT AGB-TAVG CORRELATIONS ACROSS MONTHS

cor.plot <- barchart(r.med~month, agb.tavg.cor.df, groups=agb, col=my.cols, ylab=cor.lab, ylim=c(-0.15,1.1),
                     scales=list(x=list(rot=90), y=list(at=seq(0,1,0.25)), alternating = F, tck = c(1,0)), 
                     panel = function(x, y, ...) {
                       panel.barchart(x, y, ...)
                       #panel.text(x=1.1,y=1.05, label='(a)', font=2)
                     }
)


#------------------------------------
# PLOT % SHRUB VS TAVG GRADIENT FOR AUGUST

sagb.pcnt.plot <- xyplot(shrub.pcnt.avg.med~tavg, data=agb.tavg.june, ylab=spcnt.lab, xlab=tavg.lab, ylim=c(0,85), 
                         scales=list(y=list(at=seq(0.0,80,20)), alternating = F, tck = c(1,1)), key=my.key, 
                         panel = function(x, y, ...){
                           panel.xyplot(x, y, type=c('l'), cex=0.6, lty=my.lty[1], lwd=2,col=my.cols[3], ...)
                         }
)



#------------------------------------
# PLOT AGB VS TAVG GRADIENT FOR AUGUST

tagb.plot.ci95 <- xyplot(tagb.kgm2.avg.med~tavg, data=agb.tavg.june, ylab=agb.lab, xlab=tavg.lab, ylim=c(0,1.1), scales=list(y=list(at=seq(0,1,0.25)), alternating = F, tck = c(1,0)),  
                         panel = function(x, y, ...){
                           upper = agb.tavg.june$tagb.kgm2.avg.q975
                           lower = agb.tavg.june$tagb.kgm2.avg.q025
                           panel.polygon(c(x, rev(x)), c(upper, rev(lower)), col = adjustcolor(err.cols[1], alpha.f=0.6), border = FALSE, ...)
                           panel.xyplot(x, y, type=c('l'), cex=0.6, lty=my.lty[1], lwd=2,col=my.cols[1], ...)
                         }
)

tagb.plot.ci95 <- doubleYScale(tagb.plot.ci95, sagb.pcnt.plot, add.ylab2 = T, style1 = 0, style2 = 0)

tagb.plot.ci50 <- xyplot(tagb.kgm2.avg.med~tavg, data=agb.tavg.june, ylab=agb.lab, xlab=tavg.lab, ylim=c(0,1.1), scales=list(y=list(at=seq(0,1,0.25)), alternating = F, tck = c(1,0)),
                         panel = function(x, y, ...){
                           upper = agb.tavg.june$tagb.kgm2.avg.q75
                           lower = agb.tavg.june$tagb.kgm2.avg.q25
                           panel.polygon(c(x, rev(x)), c(upper, rev(lower)), col = adjustcolor(err.cols[2], alpha.f=0.6), border = FALSE, ...)
                           panel.xyplot(x, y, type=c('l'), cex=0.6, lty=my.lty[1], lwd=2,col=my.cols[1], ...)
                         }
)
tagb.plot.ci50 <- doubleYScale(tagb.plot.ci50, sagb.pcnt.plot, add.ylab2 = T, style1 = 0, style2 = 0)


sagb.plot.ci95 <- xyplot(sagb.kgm2.avg.med~tavg, data=agb.tavg.june, ylab=agb.lab, xlab=tavg.lab, ylim=c(0,1.1), scales=list(y=list(at=seq(0,1,0.25)), alternating = F, tck = c(1,0)), 
                         panel = function(x, y, ...){
                           upper = agb.tavg.june$sagb.kgm2.avg.q975
                           lower = agb.tavg.june$sagb.kgm2.avg.q025
                           panel.polygon(c(x, rev(x)), c(upper, rev(lower)), col = adjustcolor(err.cols[1], alpha.f=0.6), border = FALSE, ...)
                           panel.xyplot(x, y, type=c('l'), cex=0.6, lty=my.lty[2],lwd=2,col=my.cols[2], ...)
                         }
)
sagb.plot.ci95 <- doubleYScale(sagb.plot.ci95, sagb.pcnt.plot, add.ylab2 = T, style1 = 0, style2 = 0)

sagb.plot.ci50 <- xyplot(sagb.kgm2.avg.med~tavg, data=agb.tavg.june, ylab=agb.lab, xlab=tavg.lab, ylim=c(0,1.1), scales=list(y=list(at=seq(0,1,0.25)), alternating = F, tck = c(1,0)),
                         panel = function(x, y, ...){
                           upper = agb.tavg.june$sagb.kgm2.avg.q75
                           lower = agb.tavg.june$sagb.kgm2.avg.q25
                           panel.polygon(c(x, rev(x)), c(upper, rev(lower)), col = adjustcolor(err.cols[2], alpha.f=0.6), border = FALSE, ...)
                           panel.xyplot(x, y, type=c('l'), cex=0.6, lty=my.lty[2],lwd=2,col=my.cols[2], ...)
                           #panel.text(x=2,y=1.05, label='(b)', font=2)
                         }
)
sagb.plot.ci50 <- doubleYScale(sagb.plot.ci50, sagb.pcnt.plot, add.ylab2 = T, style1 = 0, style2 = 0)

#------------------------------------
# COMBINE PLOTS

#jpeg('C:/Users/lb968/Google Drive/research/nau/above/figures/nslope_agb_by_clim_tavg_aug.jpg', width = 4, height = 10, res = 300, units = 'in')
# print(cor.plot, position=c(0,0.66,1,1), more=T)
# print(tagb.plot.ci95, position=c(0,0.33,1,0.7), more=T)
# print(tagb.plot.ci50, position=c(0,0.33,1,0.7), more=T)
# print(sagb.plot.ci95, position=c(0,0.33,1,0.7), more=T)
# print(sagb.plot.ci50, position=c(0,0.33,1,0.7), more=T)
# print(sagb.pcnt.plot.ci95, position=c(0,0,1,0.36), more=T)
# print(sagb.pcnt.plot.ci50, position=c(0,0,1,0.36))

#dev.off()

jpeg('C:/Users/lb968/Google Drive/research/nau/above/figures/nslope_agb_by_clim_tavg_jun.jpg', width = 8, height = 3.5, res = 300, units = 'in')
print(cor.plot, position=c(0,0,0.5,1), more=T)
print(tagb.plot.ci95, position=c(0.5,0,1,1), more=T)
print(tagb.plot.ci50, position=c(0.5,0,1,1), more=T)
print(sagb.plot.ci95, position=c(0.5,0,1,1), more=T)
print(sagb.plot.ci50, position=c(0.5,0,1,1))
dev.off()


#------------------------------------------------------------------------------------------------------
# EXPERIMENTAL
#------------------------------------------------------------------------------------------------------

plot(tagb.kgm2.avg.med~tavg, agb.tavg.june)
tagb.june.lm <- lm(tagb.kgm2.avg.med~tavg, agb.tavg.june)
summary(tagb.june.lm)
plot(agb.tavg.june$tagb.kgm2.avg.med, predict(tagb.june.lm))
abline(a=0, b=1)

plot(sagb.kgm2.avg.med~tavg, agb.tavg.june)
sagb.june.lm <- lm(sagb.kgm2.avg.med~tavg, agb.tavg.june)
summary(sagb.june.lm)
plot(agb.tavg.june$sagb.kgm2.avg.med, predict(sagb.june.lm))
abline(a=0, b=1)




# quick calculations using numbers from Loranty et al. 2016
tundra.area <- 6.58*10^6 # km2
greening <- 2382585+243201
browning <- 256130 + 93688

greening / tundra.area * 100
browning / tundra.area * 100
