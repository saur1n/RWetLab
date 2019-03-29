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
sample.names <- read.table("rawData/Exp15_19.txt", header = TRUE, sep = "\t",stringsAsFactors = 0)

##### INITIALIZE OUTPUT
out = NULL
maxgr = NULL
dtime = NULL
stime = NULL

##### CLEAN DATA
d$Time <- seq(0,15*(dim(d)[1]-1),15)
d$Time <- d$Time*60
d <- d[,c(1,3:length(d))]

##### PATH LENGTH CORRECTION
df <- plcor(d,125)
#colnames(df) <- title

##### FIT DATA
#fit = fitCellGrowth(x=df$Time,z=log2(df$A9))
#plot(fit, scaleX=1/(60*60), xlab="time (hours)")
#attributes(fit)[c(3,4,5,6)]

#df$Time[attr(fit,"pointOfMax")]/(60*60)
#log(2)/attr(fit,"maxGrowthRate")/60

##### LOOP THROUGH ALL THE DATA
for (i in 2:length(df)) {
  OD = abs(matrix(unlist(df[i]), ncol = 1, byrow = TRUE))
  fit = fitCellGrowth(x=df$Time,z=log2(OD))
  maxgr[i-1] = attr(fit,"maxGrowthRate")/60
  dtime[i-1] = log(2)/attr(fit,"maxGrowthRate")/60
  stime[i-1] = df$Time[attr(fit,"pointOfMax")]/(60*60)
}

###### CREATING OUTPUT FILE

out$Sample = sample.names$Sample.Name
out$MaxGrowthRate = maxgr
out$DoubleTime = dtime
out$SaturationTime = stime

out <- data.frame(matrix(unlist(out), nrow=length(maxgr)), stringsAsFactors=FALSE)
colnames(out) <- c('Sample','MaxGrowthRate','DoubleTime','SaturationTime')

write.csv(out,'outData/cellGrowth_T1.csv')
