rm(list=ls())
require(raster)
require(maptools)
setwd('C:/research/projects/above/gis_data/nslope/')

#------------------------------------------------------------------------------------------------------------
# LOAD FILES AND SET VARIABLES
#------------------------------------------------------------------------------------------------------------
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
aaea <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

nslope.wgs <- readShapePoly('nslope_wgs84', proj4string = wgs84)
nslope.aaea <- readShapePoly('nslope_wgs84', proj4string = aaea)
nslope.aaea.30m <- raster('nslope_30m_aaea.tif')

tavg.files <- list.files('D:/data/climate/wordclim_v2/wc2.0_10m_tavg/', full.names = T)
tavg.wgs <- stack(tavg.files)


aggregate(nslope.aaea.30m, 


#------------------------------------------------------------------------------------------------------------
# PREP WORLDCLIM: (1) crop to aoi, (2) project to aea on 30 m foot print, (3) mask to nslope 
#------------------------------------------------------------------------------------------------------------

tavg.nslope.wgs <- crop(tavg.wgs, nslope.wgs)

tavg.nslope.aaea <- projectRaster(tavg.nslope.wgs, nslope.aaea.30m, method='bilinear')
tavg.nslope.aaea <- mask(tavg.nslope.aaea, nslope.aaea)