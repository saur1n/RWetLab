##### GROWTHRATE QC
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 04/10/2019

##### INITIALIZATION
#install.packages("readxl")
#install.packages("locfit0")
#install.packages("cellGrowth")
#install.packages("ggplot2")
#install.packages("ggpubr")
library(readxl)
library(locfit)
library(growthrates)
library(ggplot2)
library(ggpubr)
library(stringr)
source("functions/plcor.R")

d <- read_excel("rawData/Exp1019_selected.xlsx",col_types = "numeric")
expt.name = 'Expt_10'
plot.path.out = 'outData/plots/growthcurves/growthrates/'

##### CLEAN DATA
d$Time <- seq(0,15*(dim(d)[1]-1),15)
df <- plcor(d,125)


for (i in 2:dim(d)[2]) {
  fitplc <- fit_easylinear(df$Time, df[[i]], h=14, quota = 1);
  fit <- fit_easylinear(d$Time, d[[i]], h=14, quota = 1);
  
  jpeg(sprintf('%sGR_QC_%s_%d.png',
               plot.path.out,
               expt.name,
               i),
       width=2400, height=1200)
  par(mfrow=c(1,2))
  plot(fit, log = 'y',
       main=sprintf('%s - %s\nDT = %0.2f mins | OD0 = %0.3f',
                    expt.name,
                    colnames(df[i]),
                    log(2)/coef(fit)[[3]],
                    d[[i]][1]),
       ylim = c(0.006,1.5))
  plot(fitplc, log = 'y',
       main=sprintf('%s - %s\nDT = %0.2f mins | OD0 = %0.3f',
                    expt.name,
                    colnames(df[i]),
                    log(2)/coef(fitplc)[[3]],
                    df[[i]][1]),
       ylim = c(0.03,6.5))
  dev.off()
}


gev = c(154.39,155.79)
ybr = c(135.93,136.87)
ylr = c(132.72,128.36)

t.test(gev,ybr,alternative = 'g')$p.value
t.test(gev,ylr,alternative = 'g')$p.value
