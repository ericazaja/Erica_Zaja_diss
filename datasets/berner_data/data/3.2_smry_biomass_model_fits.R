# This R script summarizes regression models relating tundra aboveground biomass to Landsat NDVI across 1,000 Monte Carlo iterations.

rm(list=ls())
require(raster)
require(dplyr)

files <- list.files('/scratch/lb968/agb_mc_iterations/nls_models/', full.names = T)
nls.df <- do.call("rbind", lapply(files, read.csv, header=T))

nls.df.smry <- nls.df %>% group_by(model) %>% summarise(a.med=round(median(a),4), a.q025=round(quantile(a, 0.025),4),a.q975=round(quantile(a, 0.975),4),
                                         b.med=round(median(b)*1000,2), b.q025=round(quantile(b, 0.025)*1000,2),b.q975=round(quantile(b, 0.975)*1000,2),
                                         r2.med=round(median(r2),2), r2.q025=round(quantile(r2, 0.025),2),r2.q975=round(quantile(r2, 0.975),2),
                                         p.med=median(p), p.q025=quantile(p, 0.025),p.q975=quantile(p, 0.975),
                                         rmse.med=round(median(rmse),3), rmse.q025=round(quantile(rmse, 0.025),3), rmse.q975=round(quantile(rmse, 0.975),3))
nls.df.smry

# write out
write.table(nls.df, '/projects/above_gedi/lberner/nslope_biomass/output/model_coef_agb_vs_ndvi.csv', sep = ',', row.names = F, col.names = T)
write.table(nls.df.smry, '/projects/above_gedi/lberner/nslope_biomass/output/model_coef_agb_vs_ndvi_smry.csv', sep = ',', row.names = F, col.names = T)
