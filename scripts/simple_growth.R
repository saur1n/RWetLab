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

path.out = 'outData/'

df <- read.csv("rawData/012020SPanalysis.csv")

##### INITIALIZE OUTPUT
out = NULL
maxgr = NULL
dtime = NULL
ltime = NULL
sod = NULL

for (i in 2:length(df)) {
  fit0 <- fit_easylinear(df$Time, abs(df[[i]]), h=2, quota = 1);
  jpeg(sprintf('%s%s_GC.png',
               path.out,
               colnames(df[i])),
       width=600, height=600)
  plot(fit0, log = 'y',
       main=sprintf('%s | Doubling Time = %0.2f mins',
                    colnames(df[i]),
                    log(2)/coef(fit0)[[3]]),
       ylim = c(0.1,1.2))
  dev.off()
  maxgr[i-1] = coef(fit0)[[3]]
  dtime[i-1] = log(2)/coef(fit0)[[3]]
  ltime[i-1] = coef(fit0)[[4]]
  sod[i-1] = df[[9,i]]
}

out$MaxGrowthRate = maxgr
out$DoubleTime = dtime
out$SatOD = sod
out$LagTime = ltime
out <- data.frame(matrix(unlist(out), nrow=length(maxgr)), stringsAsFactors=FALSE)
colnames(out) <- c('MaxGR','DTime','SOD','LagTime')
out[5] = data.frame('Sample' = colnames(df[2:55]))

write.csv(out, file = sprintf("%s012020_Experiment_Analysis.csv",path.out))
