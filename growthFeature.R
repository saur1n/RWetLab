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

d <- read_excel("Exp0719.xlsx",col_types = "numeric")

##### CLEAN DATA
#d$Time[length(d$Time)] <- 1 
d$Time <- d$Time/60 #convert to hours
#d <- d[,c(1,3:length(d))] #removing blanks

##### CHECK
gc_fit <- SummarizeGrowth(d$Time, d$YBR4.1..3)
plot(gc_fit)

##### FIT LINES FOR ALL WELLS
gc_out <- SummarizeGrowthByPlate(d)
#gc_out <- data.frame(t(gc_out))

##### OUTPUT CSV FILE
#write.csv(gc_out,'growthFeatures.csv')

for (i in 1:dim(gc_out)[1]) {
  tmp = unlist(strsplit(gc_out['sample'][i,],'[.]'))[1]
  gc_out['sample'][i,] = tmp
}

gc_out$t_gen = gc_out$t_gen * 60

##### PLOT THE GENERATION TIMES
#ybr_dat <- gc_out[grepl("YBR4",gc_out[,1]),]
#ybr_dat

ggplot(gc_out, aes(sample,t_gen)) + geom_boxplot()
##### END
