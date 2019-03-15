##### CELL GROWTH
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 09/13/2018

##### INITIALIZATION
#install.packages("readxl")
#install.packages("cellGrowth")
library(cellGrowth)

d <- read_excel("R/TestFiles/DTT_log_3_col.xlsx",
                col_types = "numeric")

##### CLEAN DATA
d$Time[length(d$Time)] <- 1
d$Time <- d$Time*(24*60*60)
d <- d[,c(1,3:length(d))]

##### FIT DATA
fit = fitCellGrowth(
  x=d$Time,
  z=log2(d$A1)
)

plot(fit, scaleX=1/(60*60), xlab="time (hours)")
attributes(fit)[c(3,4,5,6)]


