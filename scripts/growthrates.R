##### GROWTH RATE
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 04/10/2019

##### INITIALIZATION
#install.packages("readxl")
#install.packages("locfit0")
#install.packages("cellGrowth")
#install.packages("ggplot2")
#install.packages("ggpubr")
library(readxl)
library(locfit)
library(growthrates)
library(ggplot2)
library(ggpubr)
library(stringr)
library(smoother)
#source("functions/plcor.R")
load('plc_models/plc_fy125spns.rda')

d <- read_excel("rawData/Exp16_19_RawData.xlsx",col_types = "numeric")
sample.names <- read.table("rawData/Exp16_19_SpreedSheet.txt", header = TRUE, sep = "\t",stringsAsFactors = 0)
expt.name = 'Exp16'
#ref.name = c('GEV','GEV')
path.out = 'outData/'
dir.create(file.path(path.out, 'plots'), showWarnings = FALSE)
dir.create(file.path(path.out, 'plots/growthcurves'), showWarnings = FALSE)
dir.create(file.path(path.out, 'plots/growthcurves/growthrates'), showWarnings = FALSE)
plot.path.out = 'outData/plots/growthcurves/growthrates/'

##### INITIALIZE OUTPUT
out_fit = NULL
maxgr_fit = NULL
dtime_fit = NULL
ltime_fit = NULL
spoint_fit = NULL

##### CLEAN DATA
d$Time <- seq(0,15*(dim(d)[1]-1),15)
#d$Time <- d$Time*60
#d <- d[,c(1,3:length(d))]
d <- d[1:89,] # just need the first 22 hours in Expt16
# blank1 = d$A1[1]
# blank2 = d$E1[1]
# d[,3:49] <- d[,3:49] - blank1[1]
# d[,51:97] <- d[,51:97] - blank2

  
##### PATH LENGTH CORRECTION
df <- d
for (i in 2:dim(df)[2]) {
  temp <- data.frame(ul125 = df[[i]])
  df[[i]] <- predict(fit, temp)
}
#df <- plcor(d,125)
blank1 = df$A1[1]
blank2 = df$E1[1]
df[,3:49] <- df[,3:49] - blank1
df[,51:97] <- df[,51:97] - blank2
colnames(df) <- c('Time',sample.names$Sample.Name)

##### APPLY FILTERS
# for (i in 2:dim(df)[2]) {
#   df[[i]] <- smth(df[[i]],window = 0.08,method = "gaussian")
# }
# 
# df <- df[4:85,]

##### LOOP THROUGH ALL DATA
for (i in 2:length(df)) {
  if (sum(df[[i]] <= 0)) {
    maxgr_fit[i-1] = NaN
    dtime_fit[i-1] = NaN
    ltime_fit[i-1] = NaN
    spoint_fit[i-1] = NaN
    
    
  } else {
    fit <- fit_easylinear(df$Time, df[[i]], h=14, quota = 1);
    maxgr_fit[i-1] = coef(fit)[[3]]
    dtime_fit[i-1] = log(2)/coef(fit)[[3]]
    ltime_fit[i-1] = coef(fit)[[4]]
    spoint_fit[i-1] = df[[i]][dim(df)[1]-5:dim(df)[1]]
    
    # jpeg(sprintf('%s%s %s %s.png',
    #              plot.path.out,
    #              expt.name,
    #              sample.names$Sample.Name[i-1],
    #              #sample.names$Well.Location[i-1],
    #              sample.names$Descriptor1.Value[i-1]#,
    #              #str_replace_all(sample.names$Group.Name[i-1], "[+]", "_")
    #              ),
    #      width=1200, height=1200)
    # plot(fit, log = 'y',
    #      main=sprintf('%s\n%s %s (%s) | %s\nDoubling Time = %0.2f mins',
    #                   expt.name,
    #                   sample.names$Group.Name[i-1],
    #                   sample.names$Sample.Name[i-1],
    #                   sample.names$Well.Location[i-1],
    #                   sample.names$Descriptor1.Value[i-1],
    #                   log(2)/coef(fit)[[3]]),
    #      ylim = c(0.04,6))
    # dev.off()
  }
}

###### CREATING OUTPUT FILE FOR GROWTH ATTRIBUTES
out_fit$Media = sample.names$Descriptor1.Name
out_fit$Sample = sample.names$Sample.Name
out_fit$StartingOD = substr(sample.names$Descriptor1.Value,4,15)
out_fit$ObsSOD = df[1,2:97]
out_fit$MaxGrowthRate = maxgr_fit
out_fit$DoubleTime = dtime_fit
out_fit$LagTime = ltime_fit
out_fit$SatPoint = spoint_fit

out_fit <- data.frame(matrix(unlist(out_fit), nrow=length(maxgr_fit)), stringsAsFactors=FALSE)
colnames(out_fit) <- c('Media','Sample','StartingOD','ObsSOD','MaxGrowthRate','DoubleTime','LagTime','SatPoint')
out_fit <- transform(out_fit,
                     StartingOD = as.numeric(StartingOD),
                     ObsSOD = as.numeric(ObsSOD),
                     MaxGrowthRate = as.numeric(MaxGrowthRate),
                     DoubleTime = as.numeric(DoubleTime),
                     LagTime = as.numeric(LagTime),
                     SatPoint = as.numeric(SatPoint))
out_fit <- out_fit[!grepl("BLANK",out_fit$Sample),]
# out_fit <- out_fit[1:48,] #only for exp16 ypda data
# out_fit <- out_fit[order(out_fit$StartingOD, out_fit$Sample),]
out_fit <- out_fit[order(out_fit$Media, out_fit$StartingOD),]
#out_fit <- out_fit[order(out_fit$Sample),]
# out_fit <- out_fit[order(out_fit$Media),]
rownames(out_fit) <- NULL
out_fit$Sample <- factor(out_fit$Sample)
out_fit$Media <- factor(out_fit$Media)


write.csv(out_fit,sprintf('%s%s_NEWPLC_GR.csv',path.out,expt.name))

###### PLOTTING DOUBLING TIME
# out_fit <- out_fit[out_fit$Sample != 'MEDIA',]
# out_fit <- out_fit[out_fit$Sample != 'BLANK',]

out_fit$Colony <- as.numeric(substr(out_fit$Sample, 1,1))
out_fit <- out_fit[!is.na(out_fit$Colony),]
out_fit$Colony[out_fit$Colony==1] = 'One'
out_fit$Colony[out_fit$Colony==2] = 'Two'
out_fit$Colony[out_fit$Colony==3] = 'Three'
out_fit$Colony[out_fit$Colony==4] = 'Four'

for (m in 1:length(unique(out_fit$Media))) {
  temp <- out_fit[out_fit$Media == unique(out_fit$Media)[m],]
  temp$StartingOD <- as.character(temp$StartingOD)
  # ylim1 = boxplot.stats(temp$DoubleTime)$stats[c(1, 5)]
  # ylim2 = boxplot.stats(temp$MaxGrowthRate)$stats[c(1, 5)]
  # change this depending on how clean the data is
  
  # p0 <- ggplot(temp, aes(x=Sample,y=DoubleTime,fill=Sample)) + 
  #   geom_boxplot(na.rm = TRUE,alpha=0.7) + 
  #   geom_point(aes(fill = Sample),na.rm = TRUE,alpha=1) +
  #   labs(title=sprintf("%s: %s",expt.name,unique(out_fit$Media)[m]),
  #        x ="Sample", y = "Doubling Time (mins)") +
  #   theme(legend.position = 'right') +
  #   theme_light() +
  #   stat_compare_means(label = "p.signif",
  #                      method = "wilcox.test",
  #                      ref.group = ref.name[m],
  #                      paired = FALSE,
  #                      na.rm = TRUE)
  # p0 + ylim(ylim1)
  # ggsave(sprintf('%splots/LIN_%s_%s_DTIME_BOX_GR.png',
  #                path.out,expt.name,
  #                str_replace_all(unique(out_fit$Media)[m], "[+]", "_")),
  #        width = 10, height = 10)
  
  # ggplot(temp, aes(x=DoubleTime,y=SatPoint,col=StartingOD,shape=Colony)) + 
  #   geom_point(size=5) +
  #   labs(title = "Start At? Double When? Saturate Where? Sourced How?",
  #        subtitle = "Are doubling time, saturation OD and source information and starting OD related?",
  #        x = "Doubling Time",y = "Saturation OD") +
  #   scale_shape_manual(name="Colony Source",
  #                      breaks=c('One','Two','Three','Four'),
  #                      values=c(15,1,17,7)) +
  #   scale_colour_manual(name="Starting OD",
  #                       values=c("0.03125"="#388E3C","0.0625"="#536DFE","0.125"="#FBC02D","0.25"="#FF5722")) +
  #   scale_x_continuous(breaks = seq(50,150,10), minor_breaks = seq(50,150,2.5)) +
  #   scale_y_continuous(breaks = seq(1,15,1), minor_breaks = seq(1,15,0.25)) +
  #   theme_light() +
  #   theme(axis.text.x = element_text(size=10),
  #         axis.title.x = element_text(size=15),
  #         axis.text.y = element_text(size=10),
  #         axis.title.y = element_text(size=15),
  #         plot.title = element_text(size=20,hjust = 0.5),
  #         plot.subtitle = element_text(size=13,hjust = 0.5))
  # ggsave(sprintf('%s_%s.png',
  #                expt.name,
  #                unique(out_fit$Media)[[1]]),
  #        width = 21,height = 14)
}

out_fit$StartingOD <- as.character(out_fit$StartingOD)
ggplot(out_fit, aes(x=DoubleTime,y=SatPoint,col=Media,shape=Colony,fill=StartingOD)) + 
  geom_point(size=5,stroke=2) +
  labs(title = "Start At? Double When? Saturate Where? Sourced How?",
       subtitle = "Are doubling time, saturation OD and source information and starting OD related?",
       x = "Doubling Time",y = "Saturation OD") +
  scale_shape_manual(name="Colony Source",
                     breaks=c('One','Two','Three','Four'),
                     values=c(21,22,23,24)) +
  scale_colour_manual(name="Start OD & Media",
                      values=c("0.03125"="#388E3C","0.0625"="#536DFE","0.125"="#FBC02D","0.25"="#FF5722",
                               "YPDA"="#212121","SC+GLU"="#CFD8DC"),
                      aesthetics = c("color","fill")) +
  # scale_fill_manual(name="Media Type",
  #                   values=c("YPDA"="#BDBDBD","SC+GLU"="#F5F5F5")) +
  scale_x_continuous(breaks = seq(50,150,10), minor_breaks = seq(50,150,2.5)) +
  scale_y_continuous(breaks = seq(1,15,1), minor_breaks = seq(1,15,0.25)) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size=10),
        axis.title.x = element_text(size=15),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=15),
        plot.title = element_text(size=20,hjust = 0.5),
        plot.subtitle = element_text(size=13,hjust = 0.5))
ggsave(sprintf('outData/plots/%s_OVERALL.png',
               expt.name),
       width = 21,height = 14)


