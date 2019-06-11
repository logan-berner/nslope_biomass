#rm(list=ls())
require(raster)
setwd('C:/research/projects/above/gis_data/nslope/')

sagb.mac <- raster('biomass/nslope_shrub_agb_gm2_p50_macanderExtent.tif')
shrub.cov <- raster('nslope_shrub_cover_aea_30m_macander.tif')

#shrub.cov <- mask(shrub.cov, shrub.dom)
#writeRaster(shrub.cov, 'nslope_shrub_cover_aea_30m_macander.tif', overwrite=T)

df <- data.frame(cov=values(shrub.cov), pcntAGB=values(shrub.dom))
df <- na.omit(df)

df.smp <- df[sample(1:nrow(df), 10000),]

cor.test(df.smp$pcntAGB, df.smp$cov, method = 'spearman')

cor.r <- expression(r[s]*' = 0.77')

#jpeg('C:/Users/lb968/Google Drive/research/nau/above/figures/nslope_shrubDom_vs_shrubCov.jpg', width = 6, height = 6, units = 'in', res = 300)
plot(pcntAGB ~ cov, df.smp, pch=1, xlim=c(0,100), ylim=c(0,100),
     xlab='Shrub cover (%)', ylab='Shrub dominance (%)')
abline(a=0,b=1,lty=2)
text(20, 90, cor.r, cex=1.2)

#dev.off()


cols <- colorRampPalette(c("white", "darkred"), space = "Lab")
smoothScatter(df.smp, colramp = cols, xlim=c(0,100), ylim=c(0,100))
