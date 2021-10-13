
rm(list=ls())
require(lattice)
require(reshape2)
require(plotrix)
require(grid)
setwd('C:/research/projects/above/')

landcov.mc.df <- read.table('output/smry_agb_by_landcov_mc_reps.csv', sep = ',', header=T)
head(landcov.mc.df)

landcov.df <- read.table('output/smry_agb_by_landcov.csv', sep = ',', header=T)
landcov.df$tagb.tot.pcnt <- landcov.df$tagb.Tg.tot.med/sum(landcov.df$tagb.Tg.tot.med)*100
landcov.df <- subset(landcov.df, tagb.tot.pcnt > 1 & landcov != 'water')
landcov.df$landcov <- c('regional tundra','shrub tundra','sedge tundra','mixed tundra','tussock tundra')
landcov.df$sagb.avg.pcnt <- paste(round(landcov.df$sagb.kgm2.avg.med/landcov.df$tagb.kgm2.avg.med*100), '%', sep='')
landcov.df$landcov <- factor(landcov.df$landcov, levels = landcov.df$landcov[order(landcov.df$tagb.Tg.tot.med, decreasing = T)])
head(landcov.df)
#unique(landcov.df$landcov)[2:4]
#---------------------------------------------------------------------------------------
# AVERAGE PLANT BIOMASS DENSITY
#---------------------------------------------------------------------------------------
my.cols=c('tan','darkgreen')

agb.avg.lab <- expression('Mean aboveground biomass (kg '*m^-2*')')

tagb.avg <- barchart(landcov~tagb.kgm2.avg.med,landcov.df, xlim=c(0,1.2), col=my.cols[1], xlab=list(agb.avg.lab),
                     scales=list(x=list(at=seq(0,1,0.25)), tck = c(1,0)), 
                     panel = function(x, y, ..., subscripts) {
                       panel.barchart(x, y, subscripts = subscripts, ...)
                       ll = landcov.df$tagb.kgm2.avg.q025[subscripts] 
                       ul = landcov.df$tagb.kgm2.avg.q975[subscripts] 
                       #vertical error bars
                       panel.segments(ll, as.numeric(y), ul, as.numeric(y), col = 'black', lwd = 1)
                       #lower horizontal cap 
                       panel.segments(ll, as.numeric(y) - 0.1, ll, as.numeric(y) + 0.1, ll, col = 'black', lwd = 1) 
                       #upper horizontal cap 
                       panel.segments(ul, as.numeric(y) - 0.1, ul, as.numeric(y) + 0.1, ul, col = 'black', lwd = 1)
                       }
                     )


sagb.avg <- barchart(landcov~sagb.kgm2.avg.med,landcov.df, xlim=c(0,1.2), col=my.cols[2], xlab=list(agb.avg.lab), 
                     scales=list(x=list(at=seq(0,1,0.25)), tck = c(1,0)),
                     panel=function(x,y,...){
                       panel.barchart(x, y, ...)
                       # % shurb 
                       panel.text(x=0.09,y=landcov.df$landcov, label=paste(landcov.df$shrub.pcnt.avg.med,'%',sep=''), col='white')
                       #panel.text(x=1.1,y=5.25, label='(a)', font=2)
                     }
                     )

#---------------------------------------------------------------------------------------
# TOTAL PLANT BIOMASS
#---------------------------------------------------------------------------------------
#landcov.df$landcov <- factor(landcov.df$landcov, levels = landcov.df$landcov[order(landcov.df$tagb.Tg.tot.med, decreasing = T)])

agb.tot.lab <- expression('Total aboveground biomass (Tg)'^' ')

landcov.key <- list(title='', corner=c(0.8,0.8),text=list(c('plant','shrub')), rectangle=list(col=my.cols), cex=1)

tagb.tot <- barchart(landcov~tagb.Tg.tot.med,landcov.df, xlim=c(0,140), col=my.cols[1], xlab=list(agb.tot.lab), ylab='',
                     scales=list(y=list(draw=F), x=list(at=seq(0,125,25)), tck = c(1,0)),
                     panel = function(x, y, ..., subscripts) {
                       panel.barchart(x, y, subscripts = subscripts, ...)
                       
                       ll = landcov.df$tagb.Tg.tot.q025[subscripts] 
                       ul = landcov.df$tagb.Tg.tot.q975[subscripts] 
                       #vertical error bars 
                       panel.segments(ll, as.numeric(y), ul, as.numeric(y), col = 'black', lwd = 1) 
                       #lower horizontal cap 
                       panel.segments(ll, as.numeric(y) - 0.1, ll, as.numeric(y) + 0.1, ll, col = 'black', lwd = 1) 
                       #upper horizontal cap 
                       panel.segments(ul, as.numeric(y) - 0.1, ul, as.numeric(y) + 0.1, ul, col = 'black', lwd = 1)
                       #
                       #panel.text(x=125,y=5.25, label='(b)', font=2)
                     }
)

sagb.tot <- barchart(landcov~sagb.Tg.tot.med,landcov.df, xlim=c(0,140), col=my.cols[2], xlab=list(agb.tot.lab), ylab='', 
                     scales=list(y=list(draw=F), x=list(at=seq(0,125,25)), tck = c(1,0)), key=landcov.key)

#---------------------------------------------------------------------------------------
# COMPOSITE FIGURE
#---------------------------------------------------------------------------------------
jpeg('C:/Users/lb968/Google Drive/research/nau/above/figures/nslope_agb_by_landcov.jpg', width = 8, height = 3, res = 300, units = 'in')
print(tagb.avg, position=c(0,0,0.55,1), more=T)
print(sagb.avg, position=c(0,0,0.55,1), more=T)
print(tagb.tot, position=c(0.55,0,1,1), more=T)
print(sagb.tot, position=c(0.55,0,1,1))
dev.off()
