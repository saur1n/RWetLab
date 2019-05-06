### R code from vignette source 'cellGrowth.Rnw'

###################################################
### code chunk number 1: set_width
###################################################
options( width = 60 )


###################################################
### code chunk number 2: loadlib
###################################################
library(cellGrowth)


###################################################
### code chunk number 3: cellGrowth.Rnw:61-62
###################################################
options(continue=" ")


###################################################
### code chunk number 4: loadex
###################################################
examplePath = system.file("extdata", package="cellGrowth")
dat = readYeastGrower(file.path(examplePath,"Plate2_YPFruc.txt"))
fit = fitCellGrowth(
  x=dat$time,
  z=log2(dat$OD[[which(getWellIdsTecan(dat) == "F02")]])
)
plot(fit, scaleX=1/(60*60), xlab="time (hours)")


###################################################
### code chunk number 5: attrex
###################################################
attributes(fit)[c(3,4,5,6)]


###################################################
### code chunk number 6: filemachinerunex
###################################################
mr_file = read.delim(file.path(examplePath,"machineRun.txt"))
mr_file


###################################################
### code chunk number 7: fileplatelayoutex
###################################################
pl_file = read.delim(file.path(examplePath,"plateLayout.txt"))
head(pl_file)


###################################################
### code chunk number 8: plateplot2ex
###################################################
well = wellDataFrame(
  file.path(examplePath,"plateLayout.txt"),
  file.path(examplePath,"machineRun.txt")
)
plot(well,labelColumn="strain",scaleX=1/3600,xlab="time in hours")


###################################################
### code chunk number 9: fitmultipleex
###################################################
fits <- fitCellGrowths(well)


###################################################
### code chunk number 10: fitmultipleexhead
###################################################
head(fits)


###################################################
### code chunk number 11: bandwidthex
###################################################
## Not run:
#	bw <- bandwidthCV(
#		well,
#		bandwidths=seq(0.5*3600,10*3600, length.out=30)
#	)
## End(Not run)


###################################################
### code chunk number 12: toolowbwex
###################################################
fit_small = fitCellGrowth(
  x=dat$time,
  z=log2(dat$OD[[which(getWellIdsTecan(dat) == "E09")]]),
  locfit.h=1800
)
plot(fit_small)


###################################################
### code chunk number 13: rightbwex
###################################################
fit_big = fitCellGrowth(
  x=dat$time,
  z=log2(dat$OD[[which(getWellIdsTecan(dat) == "E09")]]),
  locfit.h=24000
)
plot(fit_big)


###################################################
### code chunk number 14: owndataex
###################################################
own_file = read.delim(file.path(examplePath,"customDataFormat.txt"))
head(own_file)
x = own_file[[1]]
z = own_file[[2]]
fit = fitCellGrowth(x,z)
attr(fit,"maxGrowth")
attr(fit,"pointOfMaxGrowth")

