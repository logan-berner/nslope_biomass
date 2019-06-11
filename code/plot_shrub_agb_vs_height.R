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

shrub.survey <- read.csv('field_data/NACP_Woody_Veg_N_Slope_AK_V2_1365/data/shrub_dimensions.csv')
shrub.survey$canopy.height.m[shrub.survey$canopy.height.m == -999] <- NA
shrub.survey$crown.radius.m[shrub.survey$crown.radius.m == -999] <- NA

shrub.coords <- read.csv('field_data/NACP_Woody_Veg_N_Slope_AK_V2_1365/data/shrub_sites.csv')
shrub.coords <- subset(shrub.coords, Site != 'Dalton201112')
sagb <- raster('gis_data/nslope/biomass/nslope_shrub_agb_gm2_p50.tif')

#------------------------------------------------------------------------------------------------------------
# SUMMARIZE SHRUB DIMENSIONS FOR EACH SITE
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


#------------------------------------------------------------------------------------------------------------
# MAP SITE LOCATIONS AND EXTRACT AGB
#------------------------------------------------------------------------------------------------------------
shrub.pts <- SpatialPointsDataFrame(coords =shrub.sites[,c(13,14)], shrub.sites, proj4string = aaea)

#plot(sagb)
#plot(shrub.pts, add=T)

shrub.sites$sagb.kgm2.avg <- extract(sagb, shrub.pts, buffer=100, fun=mean, na.rm=T)/1000
shrub.sites$sagb.kgm2.sd <- extract(sagb, shrub.pts, buffer=100, fun=sd, na.rm=T)/1000
shrub.sites$sagb.kgm2.n <- extract(sagb, shrub.pts, buffer=100, fun=length, na.rm=T)
shrub.sites$sagb.kgm2.se <- shrub.sites$sagb.kgm2.sd/sqrt(shrub.sites$sagb.kgm2.n)

#------------------------------------------------------------------------------------------------------------
# CORRELATE MODELED SHRUB AGB vs MEASURED SHRUB HEIGHT, CANOPY RADIES, AND CANOPY VOLUME
#------------------------------------------------------------------------------------------------------------

# modeled shrub AGB vs canopy height
cor.sagb.height <- cor.test(shrub.sites$canopy.height.m.avg, shrub.sites$sagb.kgm2.avg, method = 'spearman')
cor.sagb.height

# modeled shrub AGB vs canopy radius
cor.sagb.radius <- cor.test(shrub.sites$crown.radius.m.avg, shrub.sites$sagb.kgm2.avg, method = 'spearman')
cor.sagb.radius

# modeled shrub AGB vs canopy volume
cor.sagb.vols <- cor.test(shrub.sites$canopy.vol.m3.avg, shrub.sites$sagb.kgm2.avg, method = 'spearman')
cor.sagb.vols

cor.sagb.vols.tot <- cor.test(shrub.sites$canopy.vol.m3.tot, shrub.sites$sagb.kgm2.avg, method = 'spearman')
cor.sagb.vols.tot



#------------------------------------------------------------------------------------------------------------
# PLOT MODELES SHRUB AGB VS MEASURED SHRUB HEIGHT
#------------------------------------------------------------------------------------------------------------

agb.ylab  <- expression('Modeled shrub aboveground biomass (kg m '^-2*')')
height.xlab <- "Measured mean shrub canopy height (m)" 

cor.r <- bquote(r[s]*' = '*~.(round(as.numeric(cor.sagb.height$estimate),2)))

my.pch.cex=1.2
my.cex.axis=1.2

jpeg('C:/Users/lb968/Google Drive/research/nau/above/figures/nslope_shrub_AGB_vs_height.jpeg', width = 6, height = 5, res = 300, units = 'in')

par.op <- par(mar=c(4,5,1,1))
plotCI(shrub.sites$canopy.height.m.avg, shrub.sites$sagb.kgm2.avg, uiw=shrub.sites$canopy.height.m.se, err='x', xaxt='n', yaxt='n', 
       xlim=c(0.5,2.2), ylim=c(0,1.5),xlab = '', ylab='',cex = my.cex.axis, pch=16, scol='grey50')
plotCI(shrub.sites$canopy.height.m.avg, shrub.sites$sagb.kgm2.avg, uiw=shrub.sites$sagb.kgm2.se, err='y', xaxt='n', yaxt='n', 
       xlim=c(0.5,2.2), ylim=c(0,1.5),xlab = '', ylab='',cex = my.cex.axis, pch=16, scol='grey50', add=T)
axis(1, at = seq(0.5,2,0.5), cex.axis=my.cex.axis, labels = T)
axis(2, seq(0,1.5,0.5), las=2, cex.axis=my.cex.axis)
mtext(side = 1, line = 3.0, cex = my.pch.cex, height.xlab)
mtext(side = 2, line = 3.5, cex = my.pch.cex, agb.ylab)
text(0.7, 0.80, cor.r, cex=my.cex.axis)
text(0.7, 0.75, "P < 0.001", cex=my.cex.axis)
par(par.op)
dev.off()

#------------------------------------------------------------------------------------------------------------
# PLOT MODELES SHRUB AGB VS MEASURED SHRUB CANOPY VOLUME
#------------------------------------------------------------------------------------------------------------

agb.ylab  <- expression('Modeled shrub aboveground biomass (kg m '^-2*')')
vol.xlab <- expression("Measured shrub canopy volume (m"^3*')') 

cor.r <- bquote(r[s]*' = '*~.(round(as.numeric(cor.sagb.vols$estimate),3)))

my.pch.cex=1.2
my.cex.axis=1.2

jpeg('C:/Users/lb968/Google Drive/research/nau/above/figures/nslope_shrub_AGB_vs_volume.jpeg', width = 6, height = 5, res = 300, units = 'in')

par.op <- par(mar=c(4,5,1,1))
plotCI(shrub.sites$canopy.vol.m3.avg, shrub.sites$sagb.kgm2.avg, uiw=shrub.sites$canopy.vol.m3.se, err='x', xaxt='n', yaxt='n', 
       xlim=c(0.0,8), ylim=c(0,1),xlab = '', ylab='',cex = my.cex.axis, pch=16, scol='grey50')
plotCI(shrub.sites$canopy.vol.m3.avg, shrub.sites$sagb.kgm2.avg, uiw=shrub.sites$sagb.kgm2.se, err='y', xaxt='n', yaxt='n', 
       xlim=c(0.0,8), ylim=c(0,1),xlab = '', ylab='',cex = my.cex.axis, pch=16, scol='grey50', add=T)
axis(1, at = seq(0,8,2), cex.axis=my.cex.axis, labels = T)
axis(2, seq(0,1,0.25), las=2, cex.axis=my.cex.axis)
mtext(side = 1, line = 3.0, cex = my.pch.cex, vol.xlab)
mtext(side = 2, line = 3.5, cex = my.pch.cex, agb.ylab)
text(0.7, 0.80, cor.r, cex=my.cex.axis)
text(0.7, 0.75, "P < 0.001", cex=my.cex.axis)
par(par.op)
dev.off()


plot(sqrt(shrub.sites$canopy.height.m.avg), shrub.sites$sagb.kgm2.avg)
plot(shrub.sites$canopy.height.m.avg, sqrt(shrub.sites$sagb.kgm2.avg))
plot(log10(shrub.sites$canopy.height.m.avg), shrub.sites$sagb.kgm2.avg)
x <- subset(shrub.sites, canopy.height.m.avg > 0.7)
plot(x$canopy.height.m.avg, x$sagb.kgm2.avg)
cor.test(x$canopy.height.m.avg, x$sagb.kgm2.avg)
