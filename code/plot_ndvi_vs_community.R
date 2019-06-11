#-------------------------------------------------------------------------------------------------------------------
# MIXED-EFFECT LANDSAT NDVI TREND ANALYSIS FOR TUNDRA BIOME
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2017-07-03
#-------------------------------------------------------------------------------------------------------------------
rm(list=ls())
require(dplyr)
require(lattice)
require(reshape2)

# setwd('C:/Users/lb968/Google Drive/research/nau/nsf_arctic/arctic_greening/')
# source('C:/Users/lb968/Google Drive/code/nau/arctic_shrubs/fun_lsat_vi_phenocor.R')
# source('C:/Users/lb968/Google Drive/code/functions/fun_shade_CIs.r')
setwd('C:/Users/Logan/Google Drive/research/nau/nsf_arctic/arctic_greening//')
source('C:/Users/Logan/Google Drive/code/functions/fun_shade_CIs.r')

#--------------------------------------------------------------------------------------------------------
# READ IN LANDSAT VI TIME SERIES
#--------------------------------------------------------------------------------------------------------
ndvi.max.ts <- read.csv('output/Landsat_annual_ndvi_max_timeseries_1984to2016.csv')
ndvi.max.ts <- na.omit(ndvi.max.ts)
# ndvi.max.ts <- subset(ndvi.max.ts, community != 'barren')
head(ndvi.max.ts)
dim(ndvi.max.ts)

#--------------------------------------------------------------------------------------------------------
# SUMMARIZE NDVI BY CMI BOTH ACROSS SITES AND THROUGH TIME 
#--------------------------------------------------------------------------------------------------------

site.smry <- ndvi.max.ts %>% group_by(site) %>% summarise(ndvi.avg = mean(ndvi.pred.max), swi.avg = mean(swi), 
                                                          lat = first(lat), bioclim = first(bioclim), 
                                                          physiog = first(physiog), community = first(community))
site.smry

ndvi.comm.smry <- site.smry %>% group_by(community) %>% mutate(ndvi.comm.med = median(ndvi.avg)) %>% arrange(desc(ndvi.comm.med))
ndvi.comm.smry$community <- factor(ndvi.comm.smry$community, levels = unique(ndvi.comm.smry$community))
bwplot(ndvi.avg ~ community, ndvi.comm.smry, scales = list(x=list(rot=90)))

swi.comm.smry <- site.smry %>% group_by(community) %>% mutate(swi.comm.med = median(swi.avg)) %>% arrange(desc(swi.comm.med))
swi.comm.smry$community <- factor(swi.comm.smry$community, levels = unique(swi.comm.smry$community))

bwplot(swi.avg ~ community, swi.comm.smry, scales = list(x=list(rot=90)))

