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

path.out <- 'outData/biotek/'
path.out.gc <- 'outData/biotek/gc/'
expt_name <- 'BIOTEK_VAROD_200123' 
df <- read.csv("rawData/biotek/VAROD_KIN_CLEAN_ALL.csv")
map <- read.csv("rawData/biotek/20200122_PLATEMAP.csv")
colnames(map) <- c('well','eod','sample','media','replicate')

# df$Time <- seq(30,30*dim(df)[1],30)

##### INITIALIZE OUTPUT
out = NULL
date = NULL
maxgr = NULL
dtime = NULL
ltime = NULL
sod = NULL
sample = NULL
media = NULL
replicate = NULL
eod = NULL

ii <- 1
min.od <- 0
for (d in unique(df$Date)) {
  temp <- df[df$Date == d,]
  for (i in 4:length(temp)) {
    if (sum(temp[[i]] > min.od) > 5) {
      fit0 <- fit_easylinear(temp$Time[(temp[[i]] > min.od)], temp[(temp[[i]] > min.od),i], h=5, quota = 1);
      # jpeg(sprintf('%s%s_%s_GC.png',
      #              path.out.gc,
      #              expt_name,
      #              colnames(df[i])),
      #      width=600, height=600)
      # plot(fit0, log = 'y',
      #      main=sprintf('%s | %s | %0.4f \n Doubling Time = %0.2f mins',
      #                   colnames(df[i]), map$media[map$well == colnames(df[i])], map$eod[map$well == colnames(df[i])],
      #                   log(2)/coef(fit0)[[3]]),
      #      ylim = c(0.1,2))
      # dev.off()
      date[ii] = d
      maxgr[ii] = coef(fit0)[[3]]
      dtime[ii] = log(2)/coef(fit0)[[3]]
      ltime[ii] = coef(fit0)[[4]]
      sod[ii] = temp[[32,i]]
      eod[ii] = map$eod[map$well == colnames(temp[i])]
      sample[ii] = as.character(map$sample[map$well == colnames(temp[i])])
      replicate[ii] = as.character(map$replicate[map$well == colnames(temp[i])])
      media[ii] = as.character(map$media[map$well == colnames(temp[i])])
      ii <- ii + 1
    }
  }
}


out <- data.frame(Date = date,
                  MaxGR = maxgr,
                  DTime = dtime,
                  EOD = eod,
                  SOD = sod,
                  LagTime = ltime,
                  Sample = sample,
                  Media = media,
                  Replicate = replicate)

# out[5] = data.frame('Sample' = colnames(df[3:i]))
# out$MaxGR <- as.numeric(out$MaxGR)
# out$DTime <- as.numeric(out$DTime)
# out$SOD <- as.numeric(out$SOD)
# out$LagTime <- as.numeric(out$LagTime)

# out$Group[out$Sample == 'col' & out$Replicate == 'one'] <- 'Colony 1'
# out$Group[out$Sample == 'col' & out$Replicate == 'two'] <- 'Colony 2'
# out$Group[out$Sample == 'pop' & out$Replicate == 'one'] <- 'Population 1'
# out$Group[out$Sample == 'pop' & out$Replicate == 'two'] <- 'Population 2'

write.csv(out, file = sprintf("%s%s_GC_Result.csv",path.out,expt_name))


##### MULTISTRAIN EXPTS
# ggplot(out[out$Sample != 'blank' & out$DTime < 500 & out$DTime > 10,]) +
#   geom_boxplot(aes(x = Sample, y = DTime, group = Sample), width = 0.5) +
#   geom_violin(aes(x = Sample, y = DTime, group = Sample), fill = 'transparent') +
#   geom_jitter(aes(x = Sample, y = DTime), width = 0.25) +
#   # scale_x_discrete(breaks = c('col', 'pop'),
#   #                  labels = c('Colony', 'Population')) +
#   scale_y_continuous(breaks = seq(0,500,5),
#                      minor_breaks = 0:500) +
#   # stat_compare_means(aes(x = Group, y = DTime),
#   #                    method = "t.test") +
#   labs(title = expt_name,
#        # subtitle = '4 Bio Rep | 14 Tech Rep',
#        x = 'Samples',
#        y = 'Doubling Time (min.)') +
#   # coord_cartesian(ylim = c(110, 130)) +
#   theme_linedraw() +
#   facet_wrap(.~Media)
# ggsave(sprintf('%s%s_DTime_Box.png',path.out,expt_name),
#        height = 6, width = 10,
#        dpi = 300)


##### VAR OD EXPTS
ggplot(out[out$Sample != 'blank' & out$DTime < 500 & out$DTime > 10,]) +
  geom_point(aes(x = EOD, y = DTime, col = Media, shape = Replicate), size = 3) +
  scale_y_continuous(breaks = seq(0,500,10)) +
  labs(title = expt_name,
       subtitle = sprintf('For Plates ODs > %0.2f', min.od),
       x = 'Cuvette OD',
       y = 'Doubling Time (mins.)') +
  theme_linedraw() +
  facet_wrap(.~Date*Media)
ggsave(sprintf('%s%s_DTimeVSEOD.png',path.out,expt_name),
       height = 10, width = 10,
       dpi = 300)


ggplot(out[out$Sample != 'blank' & out$DTime < 500 & out$DTime > 10 &
             out$Date == 20200122,]) +
  geom_abline() +
  geom_point(aes(x = DTime,
                 y = out$DTime[out$Sample != 'blank' & out$DTime < 500 & out$DTime > 10 &
                           out$Date == 20200123],
                 col = EOD, shape = Replicate),
             size = 3) +
  scale_color_continuous(name = 'Cuvette\nOD') +
  theme_linedraw() +
  labs(title = expt_name,
       x = '01/22/2020\nDoubling Time (min.)',
       y = '01/23/2020\nDoubling Time (min.)') +
  facet_wrap(.~Media)
ggsave(sprintf('%s%s_DTimes.png',path.out,expt_name),
       height = 5, width = 10,
       dpi = 300)

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
dat.ep <- read.csv("rawData/biotek/VAROD_EP_CLEAN_ALL.csv")
dat.new <- melt(dat.ep, id.vars = c('Date','ExpectedOD','Sample','Media','Replicate'))
dat.new <- dat.new[dat.new$Sample != 'blank',]
ggplot(dat.new) +
  geom_point(aes(x = ExpectedOD, y = value,
                 col = variable, shape = as.character(Replicate)),
             size = 3, alpha = 0.8) +
  scale_color_discrete(name = 'Mode') +
  scale_shape_discrete(name = 'Replicate') +
  scale_y_continuous(breaks = seq(0,5,0.1)) +
  labs(title = expt_name,
       x = 'Cuvette OD',
       y = 'Plate OD') +
  theme_linedraw() +
  facet_wrap(.~Date*Media)
ggsave(sprintf('%s%s_EODvsPOD.png',path.out,expt_name),
       height = 10, width = 10,
       dpi = 300)

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
        bio.cv$BioRep[i] <- br
        bio.cv$BioCV[i] <- temp.std/temp.mean * 100
        i <- i + 1
      }
    }
  }
} 
bio.cv <- data.frame(bio.cv)

ggplot(bio.cv) +
  geom_point(aes(x = EOD, y = BioCV, col = BioRep, shape = Media), size = 3) +
  labs(title = expt_name,
       x = 'Cuvette OD',
       y = 'CV% of Biological Replicates') +
  theme_linedraw() +
  facet_wrap(.~Date*Media)
ggsave(sprintf('%s%s_EODvsBioCV.png',path.out,expt_name),
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
        tech.cv$TechRep[i] <- r
        tech.cv$TechVar[i] <- 'Kinetic'
        tech.cv$TechCV[i] <- temp.std/temp.mean * 100
        i <- i + 1
      }
    }
  }
} 
tech.cv <- data.frame(tech.cv)

ggplot(tech.cv) +
  geom_point(aes(x = EOD, y = TechCV, col = as.character(TechRep), shape = Media), size = 3) +
  labs(title = expt_name,
       x = 'Cuvette OD',
       y = 'CV% of Technical Replicates') +
  scale_color_discrete(name = 'TechRep') +
  theme_linedraw() +
  guides(shape = guide_legend(order = 1),
         colour = guide_legend(order = 2)) +
  facet_wrap(.~Date*Media)
ggsave(sprintf('%s%s_EODvsTechCV.png',path.out,expt_name),
       height = 10, width = 10,
       dpi = 300)



