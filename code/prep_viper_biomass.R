rm(list=ls())
require(raster)
require(dplyr)
require(reshape2)
setwd('C:/research/projects/above/')

agb <- read.csv('field_data/VIPER_biomass/viper-arctic-data.9943.1-DATA-biomass.csv')
agb <- na.omit(agb)
cords <-  read.csv('field_data/VIPER_biomass/viper-arctic-data.9941.1-DATA-cords.csv')
cords <- na.omit(cords)
tree.agb <- read.csv('field_data/VIPER_biomass/ViPER_arctic-data.9965.1-DATA-tree-bd.csv')
desc <- read.csv('field_data/VIPER_biomass/ViPER_US_biomass_site_information_2015.csv')
shrub <- read.csv('field_data/VIPER_biomass/viper-arctic-data.9942.1-DATA-shrub-bd.csv')

head(agb)
head(cords)
dsagb <- agb[agb$FG %in% c('DS'),] #,'O-DS'),]
sagb <- agb[agb$FG %in% c('DS','EG','O-DS'),]
vagb <- agb[agb$FG %in% c('DS','EG','O-DS','Herb'),]

# summarize site coordinates
site.cords <- cords %>% filter(Location == 0) %>% group_by(Site) %>% summarise(lat=mean(Lat), lon=mean(Long))

# aboveground biomass by pft
trans.pft <- agb %>% group_by(Site, Transect, FG) %>% summarize(trans.agb.gm2=sum(AGB))
site.pft <- trans.pft %>% group_by(Site, FG) %>% summarise(site.agb.gm2.avg=round(mean(trans.agb.gm2)), site.agb.gm2.se=round(sd(trans.agb.gm2)/sqrt(3)))
site.pft.wide <- dcast(site.pft, Site ~ FG, value.var = 'site.agb.gm2.avg')
site.pft.wide$name <- desc$Site.name

write.table(site.pft.wide, 'field_data/VIPER_biomass/viper_tundra_biomass_pft_avg.csv', sep = ',', row.names = F, col.names = T)


# total aboveground biomass
trans.tagb <- agb %>% group_by(Site, Transect) %>% summarize(trans.agb.gm2=sum(AGB))
site.tagb <- trans.tagb %>% group_by(Site) %>% summarise(site.agb.gm2.avg=mean(trans.agb.gm2), site.agb.gm2.se=sd(trans.agb.gm2)/sqrt(3))

# vascular aboveground biomass
trans.vagb <- vagb %>% group_by(Site, Transect) %>% summarize(trans.agb.gm2=sum(AGB))
site.vagb <- trans.vagb %>% group_by(Site) %>% summarise(site.agb.gm2.avg=mean(trans.agb.gm2), site.agb.gm2.se=sd(trans.agb.gm2)/sqrt(3))


# shrub aboveground biomass
trans.sagb <- sagb %>% group_by(Site, Transect) %>% summarize(trans.agb.gm2=sum(AGB))
site.sagb <- trans.sagb %>% group_by(Site) %>% summarise(site.agb.gm2.avg=mean(trans.agb.gm2), site.agb.gm2.se=sd(trans.agb.gm2)/sqrt(3))

# tree aboveground biomass
trans.tree.agb <- tree.agb %>% group_by(Site, Transect) %>% summarize(trans.agb.gm2=sum(Biomass/Area, na.rm=T))

site.tree.agb <- trans.tree.agb %>% group_by(Site) %>% summarise(site.agb.gm2.avg=mean(trans.agb.gm2, na.rm=T), site.agb.gm2.se=sd(trans.agb.gm2, na.rm=T)/sqrt(3))
site.tree.agb <- site.tree.agb[-which(is.na(site.tree.agb$Site)),]

# output data frame
viper.agb <- cbind(data.frame(site=desc$Site.name, region=NA), site.cords[,2:3], data.frame(year=2015, month=as.numeric(substr(desc$Sample.Date, 0,1))),
                   site.tagb[,2:3]/1000, site.vagb[,2:3]/1000, site.sagb[,2:3]/1000, site.tree.agb[,2:3]/1000, data.frame(ref='Natali et al. 2016'), desc=desc$Description)
colnames(viper.agb) <- c('site','region','lat','lon','year','month','tagb.kgm2.avg','tagb.kgm2.se','vagb.kgm2.avg','vagb.kgm2.se','sagb.kgm2.avg','sagb.kgm2.se','tree.agb.kgm2.avg','tree.agb.kgm2.se', 'ref','description')
#viper.agb$tree.agb.gm2.avg / (viper.agb$tagb.gm2.avg + viper.agb$tree.agb.gm2.avg) 
viper.agb.tundra <- subset(viper.agb, tree.agb.kgm2.avg == 0)

mean(viper.agb.tundra$tagb.kgm2.avg)
fivenum(viper.agb.tundra$tagb.kgm2.avg)

# write out data set  
write.table(viper.agb.tundra, 'data/field_data/VIPER_biomass/viper_tundra_biomass.csv', sep = ',', row.names = F, col.names = T)


desc

head(shrub)
shrub.agb <- shrub %>% group_by(Site, Transect) %>% summarise(agb.gm2=sum(AGB, na.rm=T)/0.25) %>% group_by(Site) %>% summarise(agb.kgm2.avg=round(mean(agb.gm2, na.rm=T)))


trans.dsagb <- dsagb %>% group_by(Site, Transect) %>% summarize(trans.agb.gm2=sum(AGB))
site.dsagb <- trans.dsagb %>% group_by(Site) %>% summarise(site.agb.gm2.avg=round(mean(trans.agb.gm2)))

site.dsagb
shrub.agb
