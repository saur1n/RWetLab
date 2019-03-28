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

d <- read_excel("rawData/Exp10_19.xlsx",col_types = "numeric")

##### PATH LENGTH CORRECTION
df <- plcor(d,125)

##### CLEAN DATA
df$Time[length(df$Time)] <- 1
df$Time <- df$Time*(24*60*60)
df <- df[,c(1,3:length(df))]

##### FIT DATA
fit = fitCellGrowth(
  x=df$Time,
  z=log2(df$A1)
)

plot(fit, scaleX=1/(60*60), xlab="time (hours)")
attributes(fit)[c(3,4,5,6)]


