# This R script was used to test of statistical differences in mean tundra aboveground biomass among land cover types
# using bootstrapped comparisons among across the Monte Carlo simulaitons.

rm(list=ls())
require(reshape2)
setwd('C:/research/projects/above/')

landcov.mc.df <- read.table('output/smry_agb_by_landcov_mc_reps.csv', sep = ',', header=T)
head(landcov.mc.df)

#---------------------------------------------------------------------------------------
# COMPARE AVERAGE PLANT BIOMASS AMONG PLANT COMMUNITIES
#---------------------------------------------------------------------------------------
n.reps <- 1000

sedge <- subset(landcov.mc.df, landcov == 'sedge')
tussock <- subset(landcov.mc.df, landcov == 'tussock tundra')
shrub <- subset(landcov.mc.df, landcov == 'low shrub')
mix <- subset(landcov.mc.df, landcov == 'shrub tussock sedge mix')
#----------------------------
# Function
#----------------------------
x <- shrub$tagb.kgm2.avg
y <- tussock$tagb.kgm2.avg
n <- n.reps

sig.dif <- function(x,y,n){
  paste('p = ', (1-sum(x[sample(1:n.reps, n.reps, replace = F)] > y[sample(1:n.reps, n.reps, replace = F)]) / n.reps))
}


#----------------------------
# PLANT AVERAGE
#----------------------------
# shrub > tussock tagb avg? 
sig.dif(shrub$tagb.kgm2.avg, tussock$tagb.kgm2.avg, n.reps)

# shrub > sedge tagb ? 
sig.dif(shrub$tagb.kgm2.avg, sedge$tagb.kgm2.avg, n.reps)

# shrub > mix tagb ? 
sig.dif(shrub$tagb.kgm2.avg, mix$tagb.kgm2.avg, n.reps)

# mix > tussock ? 
sig.dif(mix$tagb.kgm2.avg, tussock$tagb.kgm2.avg, n.reps)

# mix > sedge ? 
sig.dif(mix$tagb.kgm2.avg, sedge$tagb.kgm2.avg, n.reps)

# tussock > sedge tagb ? 
sig.dif(tussock$tagb.kgm2.avg, sedge$tagb.kgm2.avg, n.reps)


#----------------------------
# SHRUB AVERAGE
#----------------------------
# shrub > tussock sagb avg? 
sig.dif(shrub$sagb.kgm2.avg, tussock$sagb.kgm2.avg, n.reps)

# shrub > sedge sagb ? 
sig.dif(shrub$sagb.kgm2.avg, sedge$sagb.kgm2.avg, n.reps)

# shrub > mix sagb ? 
sig.dif(shrub$sagb.kgm2.avg, mix$sagb.kgm2.avg, n.reps)

# mix > tussock ? 
sig.dif(mix$sagb.kgm2.avg, tussock$sagb.kgm2.avg, n.reps)

# mix > sedge ? 
sig.dif(mix$sagb.kgm2.avg, sedge$sagb.kgm2.avg, n.reps)

# tussock > sedge sagb ? 
sig.dif(tussock$sagb.kgm2.avg, sedge$sagb.kgm2.avg, n.reps)

#----------------------------
# SHRUB PERCENT AVERAGE
#----------------------------
# shrub > tussock sagb avg? 
sig.dif(shrub$shrub.pcnt.avg, tussock$shrub.pcnt.avg, n.reps)

# shrub > sedge sagb ? 
sig.dif(shrub$shrub.pcnt.avg, sedge$shrub.pcnt.avg, n.reps)

# shrub > mix sagb ? 
sig.dif(shrub$shrub.pcnt.avg, mix$shrub.pcnt.avg, n.reps)

# mix > tussock ? 
sig.dif(mix$shrub.pcnt.avg, tussock$shrub.pcnt.avg, n.reps)

# mix > sedge ? 
sig.dif(mix$shrub.pcnt.avg, sedge$shrub.pcnt.avg, n.reps)

# tussock > sedge sagb ? 
sig.dif(tussock$shrub.pcnt.avg, sedge$shrub.pcnt.avg, n.reps)

#----------------------------
# PLANT TOTAL
#----------------------------
# shrub > tussock sagb avg? 
sig.dif(shrub$tagb.Tg.tot, tussock$tagb.Tg.tot, n.reps)

# shrub > sedge tagb ? 
sig.dif(shrub$tagb.Tg.tot, sedge$tagb.Tg.tot, n.reps)

# shrub > mix tagb ? 
sig.dif(shrub$tagb.Tg.tot, mix$tagb.Tg.tot, n.reps)

# mix > tussock ? 
sig.dif(mix$tagb.Tg.tot, tussock$tagb.Tg.tot, n.reps)

# mix > sedge ? 
sig.dif(mix$tagb.Tg.tot, sedge$tagb.Tg.tot, n.reps)

# tussock > sedge tagb ? 
sig.dif(tussock$tagb.Tg.tot, sedge$tagb.Tg.tot, n.reps)


#----------------------------
# SHRUB TOTAL
#----------------------------
# shrub > tussock sagb avg? 
sig.dif(shrub$sagb.Tg.tot, tussock$sagb.Tg.tot, n.reps)

# shrub > sedge sagb ? 
sig.dif(shrub$sagb.Tg.tot, sedge$sagb.Tg.tot, n.reps)

# shrub > mix sagb ? 
sig.dif(shrub$sagb.Tg.tot, mix$sagb.Tg.tot, n.reps)

# mix > tussock ? 
sig.dif(mix$sagb.Tg.tot, tussock$sagb.Tg.tot, n.reps)

# mix > sedge ? 
sig.dif(mix$sagb.Tg.tot, sedge$sagb.Tg.tot, n.reps)

# tussock > sedge sagb ? 
sig.dif(tussock$sagb.Tg.tot, sedge$sagb.Tg.tot, n.reps)
