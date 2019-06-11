
rm(list=ls())
require(raster)
require(reshape2)
setwd('C:/research/projects/above/gis_data/nslope/')

tmplt.30m <- raster('nslope_30m_aaea.tif')

dem <- raster('D:/data/topography/edna/dem_aaea_30m.tif')
slp <- raster('D:/data/topography/edna/slope_aaea_30m.tif')
cti <- raster('D:/data/topography/edna/cti_aaea_30m.tif')
dstream <- raster('D:/data/topography/edna/dstream_aaea_30m.tif')
flowacc <- raster('D:/data/topography/edna/flowaccum_aaea_30m.tif')
asp <- raster('D:/data/topography/edna/aspect_aaea_30m.tif')

# DEM 
dem.crp <- crop(dem, tmplt.30m)
dem.crp <- mask(dem.crp, tmplt.30m)
dem.crp <- round(dem.crp)
dem.crp[dem.crp<0] <- 0
writeRaster(dem.crp, 'topo/nslope_edna_dem_aaea_30m.tif', overwrite=T, datatype='INT2U')

# SLOPE
slp.crp <- crop(slp, tmplt.30m)
slp.crp <- mask(slp.crp, tmplt.30m)
slp.crp <- round(slp.crp)
writeRaster(slp.crp, 'topo/nslope_edna_slope_aaea_30m.tif', overwrite=T, datatype='INT2U')

# ASPECT
asp.crp <- crop(asp, tmplt.30m)
asp.crp <- mask(asp.crp, tmplt.30m)
asp.crp <- round(asp.crp)
asp.crp[asp.crp<0] <- 0
writeRaster(asp.crp, 'topo/nslope_edna_aspect_aaea_30m.tif', overwrite=T, datatype='INT2U')

# CTI
cti.crp <- crop(cti, tmplt.30m)
cti.crp <- mask(cti.crp, tmplt.30m)
cti.crp <- round(cti.crp)
writeRaster(cti.crp, 'topo/nslope_edna_cti_aaea_30m.tif', overwrite=T, datatype='INT1U')

# FLOW ACCUMULATION
flowacc.crp <- crop(flowacc, tmplt.30m)
flowacc.crp <- raster::resample(flowacc.crp, tmplt.30m, method='ngb')
flowacc.crp <- mask(flowacc.crp, tmplt.30m)
flowacc.crp <- round(flowacc.crp)
writeRaster(flowacc.crp, 'topo/nslope_edna_flowacc_aaea_30m.tif', overwrite=T, datatype='INT4U')

# DISTANCE TO STREAM
dstream.crp <- crop(dstream, tmplt.30m)
dstream.crp <- mask(dstream.crp, tmplt.30m)
dstream.crp <- round(dstream.crp)
writeRaster(dstream.crp, 'topo/nslope_edna_dstream_aaea_30m.tif', overwrite=T, datatype='INT2U')






