##### PR analysis
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 12/10/2019

##### INITIALIZATION
library(ggplot2)
library(ggExtra)
library(gridExtra)
library(ggpubr)
library(reshape2)
path.out = 'outData/PR_QC/afterPM/'

load('plc_models/plc_fy125sps.rda')

c2p <- read.csv("rawData/afterPM/cuvette.csv")
c2p$date <- as.character(c2p$date)
c2p$od <- as.character(c2p$od)
c2p$attempt <- as.character(c2p$attempt)

c2p$date[c2p$date == '1112'] <- '11/12'
c2p$date[c2p$date == '1113'] <- '11/13'
c2p$date[c2p$date == '1114'] <- '11/14'
c2p$date[c2p$date == '1115'] <- '11/15'

##### PATH LENGTH CORRECTION
for (ii in 7:10) {
  temp <- data.frame(ul125 = c2p[[ii]])
  c2p[[ii]] <- abs(predict(fit, temp))
}

c2p$avg <- rowMeans(c2p[7:10])
for(i in 1:dim(c2p)[1]) {
  c2p$sd[i] <- sd(c2p[i,7:10])
}
c2p$cv <- c2p$sd/c2p$avg * 100 

##### BIOLOGICAL REPLICATES
ggplot(c2p[c2p$date != '11/13' & c2p$cv < 70,]) +
  geom_point(aes(x = pr, y = cv, col = sample)) +
  geom_smooth(aes(x = pr, y = cv, col = sample), method = 'lm') +
  # scale_y_continuous(minor_breaks = seq(0,15,1)) +
  scale_color_discrete(name = 'Sample') +
  labs(title = 'Variability in Biological Replicate Plate OD Reading (w PLC)',
       subtitle = 'CV % = STD(bio rep) / MEAN(bio rep) * 100',
       x = 'Cuvette OD',
       y = 'Coef of Variance (%)') +
  facet_wrap(~date*varname, nrow = 3) +
  theme_linedraw()
ggsave(sprintf('%scv_br_plc.png',path.out),
       height = 9, width = 6,
       dpi = 300)

##### TECHNICAL REPLICATES
temp <- c2p[c(1:10)]
temp <- melt(temp, id = c('date','sample','od','pr','sp','attempt'))

att <- NULL
for (a in unique(temp$attempt)) {
  att <- cbind(att, temp$value[temp$attempt == a])
}
att <- cbind(temp[temp$attempt == a,c(1:5,7)], att)
colnames(att) <- c('date','sample','od','pr','sp','variable','techrep1','techrep2','techrep3')

att$avg_tr <- rowMeans(att[7:9])
for(i in 1:dim(att)[1]) {
  att$sd_tr[i] <- sd(att[i,7:9])
}
att$cv_tr <- att$sd_tr/att$avg_tr * 100 
att$varname[1:256] <- 'endpoint'
att$varname[257:512] <- 'kinetic'

# att$date[att$date == '1112'] <- '11/12'
# att$date[att$date == '1113'] <- '11/13'
# att$date[att$date == '1114'] <- '11/14'
# att$date[att$date == '1115'] <- '11/15'

ggplot(att[att$date != '11/13' & att$cv_tr < 70,]) +
  geom_point(aes(x = pr, y = cv_tr, col = sample)) +
  geom_smooth(aes(x = pr, y = cv_tr, col = sample), method = 'lm') +
  # scale_y_continuous(minor_breaks = seq(0,15,1)) +
  scale_color_discrete(name = 'Sample') +
  labs(title = 'Variability in Technical Replicate Plate OD Reading',
       subtitle = 'CV % = STD(tech rep) / MEAN(tech rep) * 100',
       x = 'Cuvette OD',
       y = 'Coef of Variance (%)') +
  facet_wrap(~date*varname, nrow = 3) +
  theme_linedraw()
ggsave(sprintf('%scv_tr_plc.png',path.out),
       height = 9, width = 6,
       dpi = 300)

##### BLANKS
bl <- read.csv("rawData/afterPM/blanks.csv")
bl$date <- as.character(bl$date)
bl$date[bl$date == '1112'] <- '11/12'
bl$date[bl$date == '1113'] <- '11/13'
bl$date[bl$date == '1114'] <- '11/14'
bl$date[bl$date == '1115'] <- '11/15'
bl$attempt <- as.character(bl$attempt)

ggplot(bl[bl$date != '11/13',]) +
  geom_boxplot(aes(x = date, y = value, fill = sample)) +
  facet_wrap(~varname) +
  theme_linedraw()

bl_stat <- NULL
i = 1
for (d in unique(bl$date)) {
  for (v in unique(bl$varname[bl$date == d])) {
    for ( s in unique(bl$sample[bl$date == d & bl$varname == v])) {
      # for (a in unique(bl$attempt[bl$date == d & bl$varname == v & bl$sample == s])) {
        bl_stat$date[i] <- d
        bl_stat$varname[i] <- v
        bl_stat$sample[i] <- s
        bl_stat$attempt[i] <- a
        bl_stat$avg[i] <- mean(bl$value[bl$date == d & bl$varname == v & bl$sample == s])
        bl_stat$sd[i] <- sd(bl$value[bl$date == d & bl$varname == v & bl$sample == s])
        # bl_stat$avg[i] <- mean(bl$value[bl$date == d & bl$varname == v & bl$sample == s & bl$attempt == a])
        # bl_stat$sd[i] <- sd(bl$value[bl$date == d & bl$varname == v & bl$sample == s & bl$attempt == a])
        bl_stat$cv[i] <- bl_stat$sd[i]/bl_stat$avg[i] * 100
        i <- i + 1 
      # }
    }
  }
}
bl_stat <- data.frame(bl_stat)

ggplot(bl_stat[bl_stat$date != '11/13',]) +
  geom_point(aes(x = date, y = cv, col = sample, shape = varname), size = 4) +
  labs(title = 'Variability in Plate OD Reading of Blanks',
       subtitle = 'CV % = STD(tech+bio rep) / MEAN(tech+bio rep) * 100',
       x = 'Date', y = 'Coef. of Variance (%)') +
  scale_shape_discrete(name = 'Measurement') +
  scale_color_discrete(name = 'Sample') +
  theme_linedraw()
ggsave(sprintf('%scv_bl.png',path.out),
       height = 6, width = 6,
       dpi = 300)

##### GROWTH CURVES
library(locfit)
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("cellGrowth")
library(growthrates)
library(stringr)
kin <- read.csv("rawData/afterPM/kinetic.csv")
plate.des <- read.csv("rawData/afterPM/platedesign.csv")
plate.des$Date <- as.character(plate.des$Date)
all.dat <- NULL
for (date in unique(kin$Date)) {
  d <- kin[kin$Date == date,]
  d <- d[,2:dim(d)[2]]
  
  ##### PATH LENGTH CORRECTION
  # for (ii in 3:dim(d)[2]) {
  #   temp <- data.frame(ul125 = d[[ii]])
  #   d[[ii]] <- predict(fit, temp)
  # }
  
  ##### INITIALIZE OUTPUT
  out = NULL
  maxgr = NULL
  dtime = NULL
  ltime = NULL
  sod = NULL
  
  ##### CLEAN DATA
  d$Time <- d$Time*60
  df <- d
  
  for (i in 3:length(df)) {
    fit0 <- fit_easylinear(df$Time, abs(df[[i]]), h=2, quota = 1);
    # jpeg(sprintf('%sgrowthcurves/%s_%s_GC.png',
    #              path.out,
    #              date,
    #              colnames(d[i])),
    #      width=600, height=600)
    plot(fit0, log = 'y',
         main=sprintf('%s | Doubling Time = %0.2f mins',
                      colnames(d[i]),
                      log(2)/coef(fit0)[[3]]),
         ylim = c(0.1,1.2))
    dev.off()
    maxgr[i-2] = coef(fit0)[[3]]
    dtime[i-2] = log(2)/coef(fit0)[[3]]
    ltime[i-2] = coef(fit0)[[4]]
    sod[i-2] = df[[14,i]]
  }
  
  out$MaxGrowthRate = maxgr
  out$DoubleTime = dtime
  out$SatOD = sod
  out <- data.frame(matrix(unlist(out), nrow=length(maxgr)), stringsAsFactors=FALSE)
  out$Date <- date
  out$Sample = plate.des$Sample[plate.des$Date == date]
  out$OD = plate.des$OD[plate.des$Date == date]
  out$PR_OD = plate.des$OOD[plate.des$Date == date]
  
  all.dat <- rbind(all.dat, out)
}

colnames(all.dat) <- c('MaxGR','DTime','SOD','Date','Sample','OD','PR')
all.dat$Date <- as.character(all.dat$Date)
# all.dat$PR <- as.double(all.dat$PR)
all.dat$Date[all.dat$Date == '1112'] <- '11/12'
all.dat$Date[all.dat$Date == '1114'] <- '11/14'
all.dat$Date[all.dat$Date == '1115'] <- '11/15'

# ggplot(all.dat[all.dat$Sample == 'CELL' & all.dat$DTime < 200,]) +
#   geom_point(aes(x = PR, y = DTime, col = Date))

ggplot() +
  geom_boxplot(data = all.dat[all.dat$Sample == 'CELL' & all.dat$DTime < 200,],
               aes(x = PR, y = DTime, group = PR, fill = Date),
               position = 'dodge',
               width = 0.05, alpha = 0.9) +
  geom_point(data = all.dat[all.dat$Sample == 'CELL' & all.dat$DTime < 200,],
               aes(x = PR, y = DTime), size = 1) +
  labs(title = 'Doubling Time of Cells',
       subtitle = '',
       x = 'Spectromax Cuvette OD (at t = 0)',
       y = 'Doubling Time (mins)') +
  # facet_wrap(~Date) +
  # scale_fill_discrete(name = 'Date',
  #                     breaks = c('918','919','920'),
  #                     labels = c('9/18','9/19','9/20')) +
  # scale_x_continuous(breaks = seq(0,1.5,0.1), minor_breaks = seq(0,1.5,0.05)) +
  # scale_y_continuous(breaks = seq(0,250,10), minor_breaks = seq(0,250,5)) +
  theme_linedraw()
ggsave(sprintf('%sfdt_dayod.png',path.out),
       height = 5, width = 5,
       dpi = 300)











