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
library(locfit0)
library(cellGrowth)
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
write.csv(df,sprintf('%s%s_PATHLENGTH.csv',path.out,expt.name))

##### LOOP THROUGH ALL THE DATA
model <- c('locfit','logistic','gompertz','rosso','baranyi')
model <- model[1]

for (i in 2:length(df)) {
  OD = abs(matrix(unlist(df[i]), ncol = 1, byrow = TRUE))
  logOD = log2(OD)
  fit0 = fitCellGrowth(x=df$Time,z=logOD,model=model)
  if (length(logOD[logOD >= -4]) != 0) {
    fit1 = fitCellGrowth(x=df$Time[logOD >= -4],
                         z=logOD[logOD >= -4],
                         model=model)
    attributes(fit0)[c(3,5)] = attributes(fit1)[c(3,5)]
    attributes(fit0)[4] = length(logOD) - length(logOD[logOD >= -4]) + attr(fit1,'pointOfMaxGrowthRate')
  }
  jpeg(sprintf('%splots/growthcurves/%s_%s_%s_%s_GC.png',
               path.out,
               expt.name,
               sample.names$Sample.Name[i-1],
               sample.names$Well.Location[i-1],
               str_replace_all(sample.names$Group.Name[i-1], "[+]", "_")),
       width=600, height=600)
  plot(fit0, scaleX=1/(60*60), xlab="Time (hrs)",
       main=sprintf('%s\n%s %s (%s)\nDoubling Time = %0.2f mins',
                    expt.name,
                    sample.names$Group.Name[i-1],
                    sample.names$Sample.Name[i-1],
                    sample.names$Well.Location[i-1],
                    log(2)/attr(fit0,"maxGrowthRate")/60))
  dev.off()
  maxgr[i-1] = attr(fit0,"maxGrowthRate")/60
  dtime[i-1] = log(2)/attr(fit0,"maxGrowthRate")/60
  stime[i-1] = df$Time[attr(fit0,"pointOfMax")]/(60*60)
}

###### CREATING OUTPUT FILE FOR GROWTH ATTRIBUTES
out$Media = sample.names$Group.Name
out$Sample = sample.names$Sample.Name
out$MaxGrowthRate = maxgr
out$DoubleTime = dtime
out$SaturationTime = stime

out <- data.frame(matrix(unlist(out), nrow=length(maxgr)), stringsAsFactors=FALSE)
colnames(out) <- c('Media','Sample','MaxGrowthRate','DoubleTime','SaturationTime')
out <- out[order(out$Sample),]
out <- out[order(out$Media),]
rownames(out) <- NULL
out$Sample <- factor(out$Sample)
out$Media <- factor(out$Media)
out <- transform(out, MaxGrowthRate = as.numeric(MaxGrowthRate), 
          DoubleTime = as.numeric(DoubleTime),
          SaturationTime = as.numeric(SaturationTime))

write.csv(out,sprintf('%s%s_GROWTH_DATA_%s.csv',path.out,expt.name,model))

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
    labs(title=sprintf("%s: %s",expt.name,out$Media[m]),
         x ="Sample", y = "Doubling Time (mins)") +
    theme(legend.position = 'right') +
    theme_light() +
    stat_compare_means(label = "p.signif",
                       method = "wilcox.test",
                       ref.group = ref.name[m],
                       paired = FALSE,
                       na.rm = TRUE)
  p0 + ylim(ylim1)
  ggsave(sprintf('%splots/%s_%s_DTIME_BOX_%s.png',
                 path.out,expt.name,
                 str_replace_all(unique(out$Media)[m], "[+]", "_"),
                 model),
         width = 10, height = 10)
  
  p1 <- ggplot(temp, aes(x=Sample,y=DoubleTime,fill=Sample)) + 
    geom_violin(na.rm = TRUE,alpha=0.7) + 
    geom_point(aes(fill = Sample),na.rm = TRUE,alpha=1) +
    labs(title=sprintf("%s: %s",expt.name,out$Media[m]),
          x ="Sample", y = "Doubling Time (mins)") +
    theme(legend.position = 'right') +
    theme_light() +
    stat_compare_means(label = "p.signif",
                       method = "wilcox.test",
                       ref.group = ref.name[m],
                       paired = FALSE,
                       na.rm = TRUE)
  p1 + ylim(ylim1)
  ggsave(sprintf('%splots/%s_%s_DTIME_VIO_%s.png',
                 path.out,expt.name,
                 str_replace_all(unique(out$Media)[m], "[+]", "_"),
                 model),
         width = 10, height = 10)
}

#compare_means(DoubleTime~Sample,data=temp,ref.group="REF",
#              method="wilcox.test")
#geom_hline(yintercept = mean(temp$DoubleTime[temp$Sample == 'REF'])) +
