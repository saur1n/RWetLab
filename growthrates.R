##### CELL GROWTH
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 09/13/2018

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

d <- read_excel("rawData/Exp1019.xlsx",col_types = "numeric")
sample.names <- read.table("rawData/Exp08_19.txt", header = TRUE, sep = "\t",stringsAsFactors = 0)
expt.name = 'Exp10'
ref.name = c('GEV','GEV')
path.out = 'outData/'
dir.create(file.path(path.out, 'plots'), showWarnings = FALSE)
dir.create(file.path(path.out, 'plots/growthcurves'), showWarnings = FALSE)
dir.create(file.path(path.out, 'plots/growthcurves/growthrates'), showWarnings = FALSE)
plot.path.out = 'outData/plots/growthcurves/growthrates/'

##### INITIALIZE OUTPUT
out_fit = NULL
maxgr_fit = NULL
dtime_fit = NULL
ltime_fit = NULL

out_res = NULL
maxgr_res = NULL
dtime_res = NULL

##### CLEAN DATA
d$Time <- seq(0,15*(dim(d)[1]-1),15)
#d$Time <- d$Time*60
#d <- d[,c(1,3:length(d))]

##### PATH LENGTH CORRECTION
df <- plcor(d,125)

##### TRY OUT FITS
fit <- fit_easylinear(df$Time, df$F6, h=14, quota = 1)
#summary(fit)
#coef(fit)

#par(mfrow = c(1, 2))
plot(fit, log = 'y')
#plot(fit)
log(2)/coef(fit)[[3]]
coef(fit)[[4]]

res <- fit_spline(df$Time, df$F6, optgrid = 5)
plot(res, log = 'y')
log(2)/coef(res)[[2]]/60

##### LOOP THROUGH ALL DATA
for (i in 2:length(df)) {
  if (sum(df[[i]] <= 0)) {
    maxgr_fit[i-1] = NaN
    dtime_fit[i-1] = NaN
    ltime_fit[i-1] = NaN
    maxgr_res[i-1] = NaN
    dtime_fit[i-1] = NaN
    
  } else {
    fit <- fit_easylinear(df$Time, df[[i]], h=14, quota = 1);
    jpeg(sprintf('%sLIN_%s_%s_%s_%s_GR.png',
                 plot.path.out,
                 expt.name,
                 sample.names$Well.Location[i-1],
                 sample.names$Sample.Name[i-1],
                 str_replace_all(sample.names$Group.Name[i-1], "[+]", "_")),
         width=600, height=600)
    plot(fit, log = 'y',
         main=sprintf('%s\n%s %s (%s)\nDoubling Time = %0.2f mins',
                      expt.name,
                      sample.names$Group.Name[i-1],
                      sample.names$Sample.Name[i-1],
                      sample.names$Well.Location[i-1],
                      log(2)/coef(fit)[[3]]))
    dev.off()
    maxgr_fit[i-1] = coef(fit)[[3]]
    dtime_fit[i-1] = log(2)/coef(fit)[[3]]
    ltime_fit[i-1] = coef(fit)[[4]]
    
    res <- fit_spline(df$Time, df[[i]], optgrid = 5);
    jpeg(sprintf('%sRES_%s_%s_%s_%s_GR.png',
                 plot.path.out,
                 expt.name,
                 sample.names$Well.Location[i-1],
                 sample.names$Sample.Name[i-1],
                 str_replace_all(sample.names$Group.Name[i-1], "[+]", "_")),
         width=600, height=600)
    plot(res, log = 'y',
         main=sprintf('%s\n%s %s (%s)\nDoubling Time = %0.2f mins',
                      expt.name,
                      sample.names$Group.Name[i-1],
                      sample.names$Sample.Name[i-1],
                      sample.names$Well.Location[i-1],
                      log(2)/coef(res)[[2]]))
    dev.off()
    maxgr_res[i-1] = coef(res)[[2]]
    dtime_fit[i-1] = log(2)/coef(res)[[2]]
  }
}
