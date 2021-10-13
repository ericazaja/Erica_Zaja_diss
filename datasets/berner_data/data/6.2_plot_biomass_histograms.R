rm(list=ls())
require(raster)
require(lattice)
require(data.table)
require(dplyr)
require(latticeExtra)

tagb <- raster('C:/research/projects/above/gis_data/nslope/biomass/nslope_plant_agb_gm2_p50.tif')
sagb <- raster('C:/research/projects/above/gis_data/nslope/biomass/nslope_shrub_agb_gm2_p50.tif')

cell.id <- sample(1:ncell(sagb), size = 2000000, replace = F)
dt <- data.table(tagb=extract(tagb, cell.id)/10^3, sagb=extract(sagb, cell.id)/10^3) # convert g/m2 to kg/m2
dt <- na.omit(dt)
dt$tagb.rnd <- round(dt$tagb, 1)
dt$sagb.rnd <- round(dt$sagb, 1)
head(dt)

#---------------------------------------------------------------------------------------------------------------------
# COMPUTE CUMULATIVE PERCENTAGE
#---------------------------------------------------------------------------------------------------------------------
#dt.smry <-
tagb.smry <- dt %>% group_by(tagb.rnd) %>% summarise(cnt=n()) %>% mutate(cnt.all=sum(cnt), cum.sum=cumsum(cnt), pcnt=cnt/cnt.all*100, cum.pcnt=cum.sum/cnt.all*100)
sagb.smry <- dt %>% group_by(sagb.rnd) %>% summarise(cnt=n()) %>% mutate(cnt.all=sum(cnt), cum.sum=cumsum(cnt), pcnt=cnt/cnt.all*100, cum.pcnt=cum.sum/cnt.all*100)

quantile(dt$tagb, probs=c(0,0.5,0.75,0.9,1))
quantile(dt$sagb, probs=c(0,0.5,0.75,0.9,1))

#---------------------------------------------------------------------------------------------------------------------
# PLOT
#---------------------------------------------------------------------------------------------------------------------

tagb.ylim <- c(0,17)
tagb.xlim <- c(-0.05,2.1)
tagb.boxwidth <- 0.1

sagb.ylim <- c(0,26)
sagb.xlim <- c(-0.05,2.1)
sagb.boxwidth <- 0.1


agb.xlab = expression("Aboveground biomass (kg m"^-2*')')
my.cols=c('tan','darkgreen')

tagb.hist <- xyplot(pcnt~tagb.rnd, tagb.smry, xlim=tagb.xlim, ylim=tagb.ylim, xlab=agb.xlab, ylab="Percent of total", 
                    scales=list(y=list(alternating=1, at=seq(0,20,5))), panel=function(x,y,...){
                      panel.barchart(x,y, horizontal = F, box.width=tagb.boxwidth, col=my.cols[1], origin = 0, ...)
                    })


tagb.cumpcnt <- xyplot(cum.pcnt~tagb.rnd, tagb.smry, type='l', lty=2, xlim=tagb.xlim, ylim=c(0,105), ylab='Cumulative percent', xlab=agb.xlab,
                       scales=list(y=list(alternating=1, at=seq(0,100,25))),
                       panel=function(x,y,...){
                         panel.xyplot(x,y,...)
                         panel.text(1.6,95, "Cumulative %", cex=0.5)
                         panel.text(0.3,98, "(a) Plant", font=2)
                       })

tagb.plot <- doubleYScale(tagb.hist, tagb.cumpcnt, add.ylab2 = T, add.axis=T)
tagb.plot <- update(tagb.plot, par.settings = simpleTheme(col=c('black','black'))) # change theme colorl


sagb.hist <- xyplot(pcnt~sagb.rnd, sagb.smry, xlim=sagb.xlim, ylim=sagb.ylim, xlab=agb.xlab, ylab="Percent of total", 
                    scales=list(y=list(alternating=1, at=seq(0,20,5))), panel=function(x,y,...){
                      panel.barchart(x,y, horizontal = F, box.width=sagb.boxwidth, col=my.cols[2], origin = 0, ...)
                    })


sagb.cumpcnt <- xyplot(cum.pcnt~sagb.rnd, sagb.smry, type='l', lty=2, xlim=sagb.xlim, ylim=c(0,105), ylab='Cumulative percent', xlab=agb.xlab,
                       scales=list(y=list(alternating=1, at=seq(0,100,25))),
                       panel=function(x,y,...){
                         panel.xyplot(x,y,...)
                         panel.text(0.3, 98, "(b) Shrub", font=2)
                       })

sagb.plot <- doubleYScale(sagb.hist, sagb.cumpcnt, add.ylab2 = T, add.axis=T)
sagb.plot <- update(sagb.plot, par.settings = simpleTheme(col=c('black','black'))) # change theme colorl



jpeg('C:/Users/lb968/Google Drive/research/nau/above/figures/nslope_agb_hist.jpg', width = 4, height = 6, res = 300, units = 'in')

print(sagb.plot, position=c(0,0,1,0.52), more=T)
print(tagb.plot, position=c(0,0.48,1,1))
dev.off()

