##### CELL GROWTH
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 09/13/2018

##### INITIALIZATION
#install.packages("readxl")
#install.packages("cellGrowth")
library(readxl)
library(locfit)
library(cellGrowth)
source("functions/plcor.R")

d <- read_excel("rawData/Exp15_19_rawData.xlsx",col_types = "numeric")
temp <- read.table("rawData/Exp15_19.txt", header = TRUE, sep = "\t",stringsAsFactors = 0)
title <- NULL
title <- c('Time',temp$Sample.Name)

##### CLEAN DATA
d$Time <- seq(0,15*(dim(d)[1]-1),15)
d$Time <- d$Time*60
d <- d[,c(1,3:length(d))]

##### PATH LENGTH CORRECTION
df <- plcor(d,125)
#colnames(df) <- title

##### FIT DATA
fit = fitCellGrowth(
  x=df$Time,
  z=log2(df$A2)
)

plot(fit, scaleX=1/(60*60), xlab="time (hours)")
attributes(fit)[c(3,4,5,6)]

