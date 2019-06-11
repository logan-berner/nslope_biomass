rm(list=ls())
require(lattice)
require(latticeExtra)
require(reshape2)
require(plotrix)
require(grid)
setwd('D:/projects/above/')

#-------------------------------------------------------------------------------------------------------------
# READ IN AND SUBSET DATA SETS
#-------------------------------------------------------------------------------------------------------------
agb.tavg.df <- read.table('output/smry_agb_tavg_monthly.csv', sep = ',', header=T)
agb.tavg.df$month <- factor(agb.tavg.df$month, labels = c(month.abb,'SWI'), levels = c(1:12,'swi'))

agb.tavg.cor.df <- read.table('output/smry_agb_tavg_monthly_cor.csv', sep = ',', header=T)
agb.tavg.cor.df$month <- factor(agb.tavg.cor.df$month, labels = c(month.abb,'SWI'), levels = c(1:12,'swi'))

agb.tavg.june <- subset(agb.tavg.df, month == 'Jun' & tavg >= 2 & tavg <= 11)

head(agb.tavg.df)
head(agb.tavg.cor.df)
head(agb.tavg.june)
tail(agb.tavg.june)

#-------------------------------------------------------------------------------------------------------------
# SET UP AXIS LABELS, COLOLS, KEY, ETC
#-------------------------------------------------------------------------------------------------------------
cor.lab <- expression('Correlation (r '[s]~')')
agb.lab <- expression('Shrub aboveground biomass (kg m '^-2~')')
tavg.lab <- expression('Mean June air temperature ('~degree~'C )')

err.cols=c('gray80','gray30')
#my.cols=c('tan','darkgreen','lightblue')
my.cols=c('tan','black','cyan3')

my.lty <- c(1,1)
my.cex = 1.1
#my.key <- list(title='', corner=c(0,0.98),text=list(c('plant AGB','shrub AGB','shrub pcnt')), rectangle=list(col=my.cols), cex=0.9, columns=1)
#my.key <- list(title='', corner=c(0.7,0.05),text=list(c('plant AGB','shrub AGB','shrub %')), rectangle=list(col=my.cols), cex=0.9, columns=1)
my.key <- list(title='', corner=c(0.98,0.01),text=list(c('plant AGB','shrub AGB','shrub %')), rectangle=list(col=my.cols), cex=0.7, columns=1)


#-------------------------------------------------------------------------------------------------------------
# CREATE PLOTS
#-------------------------------------------------------------------------------------------------------------

#------------------------------------
# PLOT Shrub AGB VS TAVG GRADIENT FOR June

sagb.plot.ci95 <- xyplot(sagb.kgm2.avg.med~tavg, data=agb.tavg.june, ylab=agb.lab, xlab=tavg.lab, ylim=c(0,0.65), 
                         scales=list(y=list(at=seq(0,0.60,0.20), cex = my.cex), x=list(cex=my.cex), alternating = F, tck = c(1,0)),  
                         panel = function(x, y, ...){
                           upper = agb.tavg.june$sagb.kgm2.avg.q975
                           lower = agb.tavg.june$sagb.kgm2.avg.q025
                           panel.polygon(c(x, rev(x)), c(upper, rev(lower)), col = adjustcolor(err.cols[1], alpha.f=0.6), border = FALSE, ...)
                           panel.xyplot(x, y, type=c('l'), cex=0.6, lty=my.lty[2],lwd=2,col=my.cols[2], ...)
                         }
)

sagb.plot.ci50 <- xyplot(sagb.kgm2.avg.med~tavg, data=agb.tavg.june, ylab=agb.lab, xlab=tavg.lab, ylim=c(0,0.65), 
                         scales=list(y=list(at=seq(0,0.60,0.20), cex = my.cex), x=list(cex=my.cex), alternating = F, tck = c(1,0)), 
                         panel = function(x, y, ...){
                           upper = agb.tavg.june$sagb.kgm2.avg.q75
                           lower = agb.tavg.june$sagb.kgm2.avg.q25
                           panel.polygon(c(x, rev(x)), c(upper, rev(lower)), col = adjustcolor(err.cols[2], alpha.f=0.6), border = FALSE, ...)
                           panel.xyplot(x, y, type=c('l'), cex=0.6, lty=my.lty[2],lwd=2,col=my.cols[2], ...)
                           #panel.text(x=2,y=1.05, label='(b)', font=2)
                         }
)

#------------------------------------
# COMBINE PLOTS
jpeg('C:/Users/lb968/Google Drive/grants and fellowships/helping_others/Tape_UAF_Epscor/figures/nslope_shrub_agb_by_clim_tavg_jun.jpg', width = 4, height = 4, res = 400, units = 'in')
print(sagb.plot.ci95, more=T)
print(sagb.plot.ci50)
dev.off()

