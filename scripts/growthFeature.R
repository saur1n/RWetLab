##### CELL GROWTH FEATURES
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 09/11/2018

##### INITIALIZATION
#install.packages("readxl")
#install.packages("growthcurver")
library(readxl)
library(growthcurver)
library(reshape2)
library(ggplot2)
library(dplyr)

d <- read.csv("outData/Exp10_19C.csv",header = TRUE)

##### CLEAN DATA
d = d[,!names(d) == 'X']
#d$Time[length(d$Time)] <- 1 
d$Time <- d$Time/60 #convert to hours
#d <- d[,c(1,3:length(d))] #removing blanks

##### CHECK
gc_fit <- SummarizeGrowth(d$Time, d$C.YPR126C4_2..96)
plot(gc_fit)

##### FIT LINES FOR ALL WELLS
gc_out <- SummarizeGrowthByPlate(d)
#gc_out <- data.frame(t(gc_out))
head(gc_out)

##### OUTPUT CSV FILE
#write.csv(gc_out,'outData/growthFeatures.csv')

for (i in 1:dim(gc_out)[1]) {
  tmp = unlist(strsplit(gc_out['sample'][i,],'[.]'))[2]
  gc_out['sample'][i,] = tmp
}

gc_out$t_gen = gc_out$t_gen * 60

##### PLOT THE GENERATION TIMES

p <- ggplot(gc_out, aes(sample,t_gen,
                        palette = "jco",alpha=0.7)) 
p + geom_boxplot()


##### END
