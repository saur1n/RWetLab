data=bactgrowth

L <- all_easylinear(value ~ time, data=bactgrowth)
summary(L)
coef(L)
rsquared(L)


test <- data.frame(Sample=character(0),
                   Time=integer(0),
                   InitOD=double(0),
                   OD=double(0),
                   stringsAsFactors=FALSE)



for (i in 2:dim(d)[2]) {
  test[i-1,1:dim(d)[1]] = list('foo')
  test$Sample[1]  = as.character('hello')
 }

test[1,] <- list("fowo")


library(readxl)
library(growthrates)
library(lattice)

plc_data <- read_excel("rawData/plc_data.xlsx")


L <- all_easylinear(Value ~ Time | OD + Sample, data=plc_data)
summary(L)
coef(L)
rsquared(L)
results <- results(L)

xyplot(mumax ~ OD|Sample, data=results)

write.csv(results,'outData/plc_results.csv')
  
  
  