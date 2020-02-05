##### SIMPLE GROWTH RATE
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 01/07/2020

##### INITIALIZATION
library(ggplot2)
library(ggExtra)
library(gridExtra)
library(ggpubr)
library(reshape2)
library(locfit)
library(growthrates)
library(stringr)

path.out <- 'outData/spectromax/'
path.out.gc <- 'outData/biotek/gc/'
expt_name <- 'SPECTROMAX' 
df <- read.csv("rawData/spectromax/SPECTROMAX_KIN_CLEAN_ALL.csv")
map <- read.csv("rawData/spectromax/SPECTROMAX_PLATEMAP.csv")
colnames(map) <- c('date','well','eod','sample','media','replicate','induction','t0od')

# load('plc_models/biotek_plc.rda')
load('plc_models/plc_fy125sps.rda')

df$Time <- as.numeric(df$Time)
# for (d in unique(df$Date)) {
#   df$Time[df$Date == d] <- seq(30,30*dim(df[df$Date == d,])[1],30)
# }
##### BLANK CORRECTION
# blank <- NULL
# for (w in as.character(map$well[map$sample == "media"])) {
#   blank <- rbind(blank, df[,colnames(df) == w])
# }
# blank <- colMeans(blank)
# df[4:length(df)] <- df[4:length(df)] - blank
# 
# df[4:length(df)] < 0
# df[df < 0] <- NA

##### INITIALIZE OUTPUT
out       = NULL
date      = NULL
maxgr     = NULL
dtime     = NULL
ltime     = NULL
sod       = NULL
sample    = NULL
media     = NULL
replicate = NULL
induction = NULL
eod       = NULL
t0od      = NULL

# plc <- data.frame(KIN = map$t0od)
# map$t0od <- predict(fit.kin, plc)

plc <- data.frame(ul125= map$t0od)
map$t0od <- predict(fit, plc)

ii <- 1
min.od <- 0
min.t <- 0
for (d in unique(df$Date)) {
  temp <- df[df$Date == d,]
  for (i in 4:length(temp)) {
    # plc <- data.frame(KIN = temp[[i]])
    # temp[[i]] <- predict(fit.kin, plc)
    plc <- data.frame(ul125 = temp[[i]])
    temp[[i]] <- predict(fit, plc)
    if (sum(temp[[i]] > min.od, na.rm = T) > 5) {
      fit0 <- fit_easylinear(temp$Time[(temp[[i]] > min.od) & (temp$Time > min.t) & !is.na(temp[[i]])], 
                             temp[(temp[[i]] > min.od) & (temp$Time > min.t) & !is.na(temp[[i]]),i], h=5, quota = 1);
      # jpeg(sprintf('%s%s_%s-%s_%s_GC.png',
      #              path.out.gc,
      #              expt_name,
      #              map$sample[map$well == colnames(temp[i])],
      #              as.character(map$replicate[map$well == colnames(temp[i])]),
      #              as.character(map$induction[map$well == colnames(temp[i])])),
      # width=600, height=600)
      # plot(fit0, log = 'y',
      #      main=sprintf('%s | %s | %s \n Doubling Time = %0.2f mins',
      #                   colnames(df[i]), map$media[map$well == colnames(df[i])], map$sample[map$well == colnames(temp[i])],
      #                   log(2)/coef(fit0)[[3]]),
      #      ylim = c(0.001,2.2))
      # dev.off()
      date[ii] = d
      maxgr[ii] = coef(fit0)[[3]]
      dtime[ii] = log(2)/coef(fit0)[[3]] * 60
      ltime[ii] = coef(fit0)[[4]]
      sod[ii] = temp[[9,i]]
      eod[ii] = map$eod[map$date == d & map$well == colnames(temp[i])]
      t0od[ii] = map$t0od[map$date == d & map$well == colnames(temp[i])]
      sample[ii] = as.character(map$sample[map$date == d & map$well == colnames(temp[i])])
      replicate[ii] = as.character(map$replicate[map$date == d & map$well == colnames(temp[i])])
      media[ii] = as.character(map$media[map$date == d & map$well == colnames(temp[i])])
      induction[ii] = as.character(map$induction[map$date == d & map$well == colnames(temp[i])])
      ii <- ii + 1
    }
  }
  df[df$Date == d,] <- temp
}

out <- data.frame(Date = date,
                  MaxGR = maxgr,
                  DTime = dtime,
                  EOD = eod,
                  SOD = sod,
                  T0OD = t0od,
                  LagTime = ltime,
                  Sample = sample,
                  Media = media,
                  Replicate = replicate,
                  Induction = induction)

# out[5] = data.frame('Sample' = colnames(df[3:i]))
# out$MaxGR <- as.numeric(out$MaxGR)
# out$DTime <- as.numeric(out$DTime)
# out$SOD <- as.numeric(out$SOD)
# out$LagTime <- as.numeric(out$LagTime)

# out$Group[out$Sample == 'col' & out$Replicate == 'one'] <- 'Colony 1'
# out$Group[out$Sample == 'col' & out$Replicate == 'two'] <- 'Colony 2'
# out$Group[out$Sample == 'pop' & out$Replicate == 'one'] <- 'Population 1'
# out$Group[out$Sample == 'pop' & out$Replicate == 'two'] <- 'Population 2'

write.csv(out, file = sprintf("%s%s_GC_PLC_Result.csv",path.out,expt_name))

##### MULTISTRAIN EXPTS
# ggplot(out[out$Sample != 'media' & out$DTime < 500 & out$DTime > 10 &
#              !(out$Sample == 'YAR035C-A' & out$Replicate == 1),]) +
ggplot(out[out$Sample != 'blank' & out$Media == 'YPDA' & 
             out$DTime < 500 & out$DTime > 10,],
       aes(x = Media, y = DTime, fill = factor(Date))) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(), aes(col = T0OD)) +
  scale_y_continuous(breaks = seq(0,500,20),
                     minor_breaks = 0:500) +
  scale_fill_discrete(name = 'Date',
                      # breaks = c("20200122","20200123","20200203"),
                      # labels = c("01/22","01/23","02/03")) +
                      breaks = c("20190918","20190919","20190920",
                                 "20191112","20191114","20191115"),
                      labels = c("09/18","09/19","09/20",
                                 "11/12","11/14","11/15")) +
  scale_color_continuous(name = 'OD (t = 0)',
                         trans = 'log',
                         breaks = c(0,0.1,0.2,0.4,0.8),
                         labels = sprintf('%0.2f',c(0,0.1,0.2,0.4,0.8))) +
  labs(x = 'Media',
       y = 'Doubling Time (min.)') +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) #+
  # facet_wrap(.~Media*Date,
  #            ncol = 2)
ggsave(sprintf('%s%s_DTime_Box_PLC.png',path.out,expt_name),
       height = 5, width = 5.5,
       dpi = 300)

out$Induction <- as.numeric(as.character(out$Induction))
out$Induction[is.na(out$Induction)] <- -1

# ggplot(out[out$Sample != 'media' & out$DTime < 500 &
#              out$DTime > 10 &
#              !(out$Date == 20200124 & out$Sample == 'YAR035C-A' & out$Replicate == 1),]) +
#   geom_point(aes(x = Sample, y = DTime, col = as.character(Date), shape = as.character(Induction))) +
#   scale_y_continuous(breaks = seq(0,500,10),
#                      minor_breaks = 0:500) +
#   labs(title = expt_name,
#        x = 'Samples',
#        y = 'Doubling Time (min.)') +
#   scale_color_discrete(name = 'Date') +
#   scale_shape_discrete(name = "Induction",
#                        breaks = c(-1,0,16,24)) +
#   theme_linedraw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   facet_wrap(.~Media)
# ggsave(sprintf('%s%s_DTime_Point.png',path.out,expt_name),
#        height = 5, width = 8,
#        dpi = 300)

# out.gal <- out[out$Sample != 'media' & out$DTime < 500 &
#                  out$DTime > 10 & out$Media != 'CASE+GLU' &
#                  !(out$Date == 20200124 & out$Sample == 'YAR035C-A' & out$Replicate == 1),]
# out.summary <- NULL
# i <- 1
# for (d in unique(out.gal$Date)) {
#   for (s in unique(out.gal$Sample[out.gal$Date == d])) {
#     for (inn in unique(out.gal$Induction[out.gal$Date == d & out.gal$Sample == s])) {
#       out.summary$Date[i] <- d
#       out.summary$Sample[i] <- s
#       out.summary$Induction[i] <- inn
#       out.summary$DTime[i] <- mean(out.gal$DTime[out.gal$Date == d & out.gal$Sample == s & out.gal$Induction == inn])
#       i <- i + 1
#     }
#   }
# }
# out.summary <- data.frame(out.summary)
# 
# ggplot(out.summary) +
#   geom_point(aes(x = Induction, y = DTime, col = Sample, shape = as.character(Date)))

df.melt <- melt(df, id.vars = c('Date','Time','Temp'))
for (d in unique(df$Date)) {
  for (v in unique(df.melt$variable[df$Date == d])) {
    df.melt$Sample[df$Date == d & df.melt$variable == v] <- as.character(map$sample[map$date == d & map$well == v])
    df.melt$Replicate[df$Date == d & df.melt$variable == v] <- as.character(map$replicate[map$date == d & map$well == v])
    df.melt$Media[df$Date == d & df.melt$variable == v] <- as.character(map$media[map$date == d & map$well == v])
    df.melt$Induction[df$Date == d & df.melt$variable == v] <- as.character(map$induction[map$date == d & map$well == v])
    df.melt$EOD[df$Date == d & df.melt$variable == v] <- as.character(map$eod[map$date == d & map$well == v])
    df.melt$T0OD[df$Date == d & df.melt$variable == v] <- as.character(map$t0od[map$date == d & map$well == v])
  }
}

ggplot(df.melt[df.melt$Sample != 'blank' & df.melt$Media == 'YPDA',],
       aes(x = Time/60, y = value)) +
  geom_point(size = 0.1) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
               geom="errorbar", color="red", lwd = 0.2) +
  stat_summary(fun.y=mean, geom="point", color="red", size = 0.1) +
  labs(x = 'Time',
       y = 'Plate OD') +
  facet_wrap(.~Media*Date*EOD,
             ncol = 8,
             scales = 'free_x') +
  theme_linedraw()
ggsave(sprintf('%s%s_GC_Box_PLC.png',path.out,expt_name),
       height = 15, width = 15,
       dpi = 300)

ggplot(df.melt[df.melt$Sample != 'blank' & df.melt$Media == 'YPDA',],
      aes(x = Time/60, y = value, col = as.numeric(T0OD))) +
  geom_point(size = 0.4) +
  labs(x = 'Time',
       y = 'Plate OD') +
  theme_linedraw() +
  scale_color_continuous(name = 'OD (t = 0)',
                         # trans = 'log',
                         breaks = c(0,0.1,0.2,0.4,0.8),
                         labels = sprintf('%0.2f',c(0,0.1,0.2,0.4,0.8))) +
  facet_wrap(.~Media*Date,
             ncol = 3)
ggsave(sprintf('%s%s_GC_PLC.png',path.out,expt_name),
       height = 10, width = 10,
       dpi = 300)

##### VAR OD EXPTS
ggplot(out[out$Sample != 'blank' & out$Media == 'YPDA' & out$DTime < 500 & out$DTime > 10,]) +
  geom_point(aes(x = T0OD, y = DTime, shape = as.factor(Date), col = as.character(Replicate)), size = 3) +
  scale_y_continuous(breaks = seq(0,500,10)) +
  labs(
       # subtitle = sprintf('For Plates ODs > %0.2f', min.od),
       x = 'OD (t = 0)',
       y = 'Doubling Time (mins.)') +
  theme_linedraw() +
  scale_color_discrete(name = 'Replicate') +
  scale_shape_discrete(name = 'Date',
                       # breaks = c('20200122','20200123','20200203'),
                       # labels = c('01/22','01/23','02/03')) +
                       breaks = c("20190918","20190919","20190920",
                                  "20191112","20191114","20191115"),
                       labels = c("09/18","09/19","09/20",
                                  "11/12","11/14","11/15")) +
  facet_wrap(.~Media)
ggsave(sprintf('%s%s_DTimeVSEOD_PLC.png',path.out,expt_name),
       height = 5, width = 5.5,
       dpi = 300)


# ggplot(out[out$Sample != 'blank' & out$DTime < 500 & out$DTime > 10 &
#              out$Date == 20200122,]) +
#   geom_abline() +
#   geom_point(aes(x = DTime,
#                  y = out$DTime[out$Sample != 'blank' & out$DTime < 500 & out$DTime > 10 &
#                            out$Date == 20200123],
#                  col = EOD, shape = Replicate),
#              size = 3) +
#   scale_color_continuous(name = 'Cuvette\nOD') +
#   theme_linedraw() +
#   labs(title = expt_name,
#        x = '01/22/2020\nDoubling Time (min.)',
#        y = '01/23/2020\nDoubling Time (min.)') +
#   facet_wrap(.~Media)
# ggsave(sprintf('%s%s_DTimes.png',path.out,expt_name),
#        height = 5, width = 10,
#        dpi = 300)

# pod11 <- ggplot()
# for (i in 3:length(df)) {
#   if (sum(df[[i]] > 1.2) > 5) {
#     pod11 +
#       geom_line(data = data.frame(Time = seq(0,30*(sum(df[[i]] > 1.1)-1),30), OD = df[(df[[i]] > 1.1),i]),
#                 aes(x = Time, y = OD))
#   }
# }
# pod11

##### ENDPOINT VARIABILITY
dat.ep <- read.csv("rawData/spectromax/SPECTROMAX_EP_CLEAN_ALL.csv")
for (ii in 6:13) {
  # plc <- data.frame(KIN = dat.ep[[ii]])
  # dat.ep[[ii]] <- predict(fit.kin, plc)
  plc <- data.frame(ul125 = dat.ep[[ii]])
  dat.ep[[ii]] <- predict(fit, plc)
}

dat.new <- melt(dat.ep, id.vars = c('Date','ExpectedOD','EP','KIN','Sample','Media','Replicate'))
dat.new <- dat.new[dat.new$Sample != 'blank',]
# ggplot(dat.new) +
#   geom_point(aes(x = EP, y = value,
#                  col = variable, shape = as.character(Replicate)),
#              size = 3, alpha = 0.8) +
#   scale_color_discrete(name = 'Mode') +
#   scale_shape_discrete(name = 'Replicate') +
#   scale_y_continuous(breaks = seq(0,5,0.1)) +
#   labs(x = 'Cuvette OD',
#        y = 'Plate OD') +
#   theme_linedraw() +
#   facet_wrap(.~Date*Media)
# ggsave(sprintf('%s%s_EODvsPOD.png',path.out,expt_name),
#        height = 5, width = 7,
#        dpi = 300)

# BIOREP WISE
i <- 1
bio.cv <- NULL
for (d in unique(dat.new$Date)) {
  for (m in unique(dat.new$Media[dat.new$Date == d])) {
    for (eod in unique(dat.new$ExpectedOD[dat.new$Date == d & dat.new$Media == m])) {
      for (br in unique(dat.new$variable[dat.new$Date == d & dat.new$Media == m &
                                  dat.new$ExpectedOD == eod])) {
        temp.mean <- mean(dat.new$value[dat.new$Date == d & dat.new$Media == m &
                                     dat.new$ExpectedOD == eod & dat.new$variable == br])
        temp.std <- sd(dat.new$value[dat.new$Date == d & dat.new$Media == m &
                                         dat.new$ExpectedOD == eod & dat.new$variable == br])
        bio.cv$Date[i] <- d
        bio.cv$Media[i] <- m
        bio.cv$EOD[i] <- eod
        bio.cv$Mean[i] <- temp.mean
        bio.cv$BioRep[i] <- br
        bio.cv$BioCV[i] <- temp.std/temp.mean * 100
        i <- i + 1
      }
    }
  }
} 
bio.cv <- data.frame(bio.cv)

ggplot(bio.cv[abs(bio.cv$BioCV) < 40 & bio.cv$Media == 'YPDA' & !is.na(bio.cv$BioCV),]) +
  geom_point(aes(x = Mean, y = BioCV, col = BioRep, shape = Media), size = 3) +
  labs(x = 'Mean BioRep OD',
       y = 'CV% of Biological Replicates') +
  theme_linedraw() +
  facet_wrap(.~Media*Date,
             drop=T)
ggsave(sprintf('%s%s_EODvsBioCV_PLC.png',path.out,expt_name),
       height = 10, width = 10,
       dpi = 300)

# TECHREP WISE
i <- 1
tech.cv <- NULL
for (d in unique(dat.new$Date)) {
  for (m in unique(dat.new$Media[dat.new$Date == d])) {
    for (eod in unique(dat.new$ExpectedOD[dat.new$Date == d & dat.new$Media == m])) {
      for (r in unique(dat.new$Replicate[dat.new$Date == d & dat.new$Media == m &
                                         dat.new$ExpectedOD == eod])) {
        temp <- dat.new[dat.new$Date == d & dat.new$Media == m &
                  dat.new$ExpectedOD == eod & dat.new$Replicate == r &
                  str_detect(dat.new$variable, 'End'),]
        temp.mean <- mean(temp$value)
        temp.std <- sd(temp$value)
        
        tech.cv$Date[i] <- d
        tech.cv$Media[i] <- m
        tech.cv$EOD[i] <- eod
        tech.cv$Mean[i] <- temp.mean
        tech.cv$TechRep[i] <- r
        tech.cv$TechVar[i] <- 'EndPoint'
        tech.cv$TechCV[i] <- temp.std/temp.mean * 100
        i <- i + 1
        
        temp <- dat.new[dat.new$Date == d & dat.new$Media == m &
                          dat.new$ExpectedOD == eod & dat.new$Replicate == r &
                          str_detect(dat.new$variable, 'Kin'),]
        temp.mean <- mean(temp$value)
        temp.std <- sd(temp$value)
        
        tech.cv$Date[i] <- d
        tech.cv$Media[i] <- m
        tech.cv$EOD[i] <- eod
        tech.cv$Mean[i] <- temp.mean
        tech.cv$TechRep[i] <- r
        tech.cv$TechVar[i] <- 'Kinetic'
        tech.cv$TechCV[i] <- temp.std/temp.mean * 100
        i <- i + 1
      }
    }
  }
} 
tech.cv <- data.frame(tech.cv)

ggplot(tech.cv[abs(tech.cv$TechCV) < 40 & tech.cv$TechCV > 0 & tech.cv$Media == 'YPDA' & !is.na(tech.cv$TechCV),]) +
  geom_point(aes(x = Mean, y = TechCV, col = as.character(TechRep), shape = Media), size = 3) +
  labs(x = 'Mean TechRep OD',
       y = 'CV% of Technical Replicates') +
  scale_color_discrete(name = 'TechRep') +
  theme_linedraw() +
  guides(shape = guide_legend(order = 1),
         colour = guide_legend(order = 2)) +
  facet_wrap(.~Media*Date*TechVar,
             nrow = 1)
ggsave(sprintf('%s%s_EODvsTechCV_PLC.png',path.out,expt_name),
       height = 5, width = 10,
       dpi = 300)



