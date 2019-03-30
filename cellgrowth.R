##### CELL GROWTH
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 09/13/2018

##### INITIALIZATION
#install.packages("readxl")
#install.packages("locfit")
#install.packages("cellGrowth")
#install.packages("ggplot2")
#install.packages("ggpubr")
library(readxl)
library(locfit)
library(cellGrowth)
library(ggplot2)
library(ggpubr)
source("functions/plcor.R")

d <- read_excel("rawData/Exp15_19_rawData.xlsx",col_types = "numeric")
sample.names <- read.table("rawData/Exp15_19.txt", header = TRUE, sep = "\t",stringsAsFactors = 0)
expt.name = 'Exp15'
ref.name = 'REF'
path.out = 'outData/'
dir.create(file.path(path.out, 'plots'), showWarnings = FALSE)

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

##### FIT DATA
#fit = fitCellGrowth(x=df$Time,z=log2(df$B3))
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

###### CREATING OUTPUT FILE FOR GROWTH ATTRIBUTES
out$Media = sample.names$Group.Name
out$Sample = sample.names$Sample.Name
out$MaxGrowthRate = maxgr
out$DoubleTime = dtime
out$SaturationTime = stime

out <- data.frame(matrix(unlist(out), nrow=length(maxgr)), stringsAsFactors=FALSE)
colnames(out) <- c('Media','Sample','MaxGrowthRate','DoubleTime','SaturationTime')
out <- out[order(out$Media,out$Sample),]
rownames(out) <- NULL
out$Sample <- factor(out$Sample)
out$Media <- factor(out$Media)
out <- transform(out, MaxGrowthRate = as.numeric(MaxGrowthRate), 
          DoubleTime = as.numeric(DoubleTime),
          SaturationTime = as.numeric(SaturationTime))

write.csv(out,sprintf('%s%s_GROWTH_DATA.csv',path.out,expt.name))

###### PLOTTING DOUBLING TIME
out <- out[out$Sample != 'MEDIA',]
out <- out[out$Sample != 'BLANK',]

for (m in 1:length(unique(out$Media))) {
  temp <- out[out$Media == unique(out$Media)[m],]
  ylim1 = boxplot.stats(temp$DoubleTime)$stats[c(1, 5)]
  #change this depending on how clean the data is
  
  p0 <- ggplot(temp, aes(x=Sample,y=DoubleTime,fill=Sample)) + 
    geom_boxplot(na.rm = TRUE,alpha=0.7) + 
    geom_point(aes(fill = Sample),na.rm = TRUE,alpha=1) +
    theme(legend.position = 'right') +
    theme_light() +
    stat_compare_means(label = "p.signif",
                       method = "t.test",
                       ref.group = ref.name,
                       paired = FALSE,
                       na.rm = TRUE)
  p0 + ylim(ylim1)
  ggsave(sprintf('%splots/%s_%s_DTIME_BOX.png',
                 path.out,expt.name,unique(out$Media)[m]),
         width = 10, height = 10)
  
  p1 <- ggplot(temp, aes(x=Sample,y=DoubleTime,fill=Sample)) + 
    geom_violin(na.rm = TRUE,alpha=0.7) + 
    geom_point(aes(fill = Sample),na.rm = TRUE,alpha=1) +
    theme(legend.position = 'right') +
    theme_light() +
    stat_compare_means(label = "p.signif",
                       method = "t.test",
                       ref.group = ref.name,
                       paired = FALSE,
                       na.rm = TRUE)
  p1 + ylim(ylim1)
  ggsave(sprintf('%splots/%s_%s_DTIME_VIO.png',
                 path.out,expt.name,unique(out$Media)[m]),
         width = 10, height = 10)
}

#compare_means(DoubleTime~Sample,data=temp,ref.group="REF",
#              method="wilcox.test")
#geom_hline(yintercept = mean(temp$DoubleTime[temp$Sample == 'REF'])) +
