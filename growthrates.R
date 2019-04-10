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

##### INITIALIZE OUTPUT
out = NULL
maxgr = NULL
dtime = NULL
stime = NULL

##### CLEAN DATA
d$Time <- seq(0,15*(dim(d)[1]-1),15)
d$Time <- d$Time*60
#d <- d[,c(1,3:length(d))]

##### PATH LENGTH CORRECTION
df <- plcor(d,125)

#####
fit <- fit_easylinear(df$Time, df$F4, h=14, quota = 1)
#summary(fit)
#coef(fit)

#par(mfrow = c(1, 2))
plot(fit, log = "y")
#plot(fit)
log(2)/coef(fit)[[3]]/60
coef(fit)[[4]]


