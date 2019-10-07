library(ggplot2)
library(ggExtra)
library(gridExtra)
library(ggpubr)
library(reshape2)

library(locfit)
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("cellGrowth")
library(growthrates)
library(stringr)
load('plc_models/plc_fy125sps.rda')
path.out = 'outData/'

kin <- read.csv("rawData/nelson_mutants.csv")
kin <- kin[c(2:12,14:24),1:18]
all.dat <- NULL

for (s in unique(kin$Sugar)) {
  d <- kin[kin$Sugar == s,]
  d <- d[1:dim(d)[1],2:dim(d)[2]]
  ##### INITIALIZE OUTPUT
  out = NULL
  maxgr = NULL
  dtime = NULL
  ltime = NULL
  sod = NULL
  
  ##### PATH LENGTH CORRECTION
  # for (ii in 2:17) {
  #   temp <- data.frame(ul125 = d[[ii]])
  #   d[[ii]] <- predict(fit, temp)
  # }
  
  df <- d
  
  for (i in 2:length(df)) {
    fit0 <- fit_easylinear(df$Time, abs(df[[i]]), h=6, quota = 1);
    jpeg(sprintf('%s%s_%s_GC.png',
                 path.out,
                 s,
                 colnames(df[i])),
         width=600, height=600)
    plot(fit0, log = 'y',
         main=sprintf('%s | Doubling Time = %0.2f mins',
                      colnames(df[i]),
                      log(2)/coef(fit0)[[3]]),
         ylim = c(0.005,5))
    dev.off()
    maxgr[i-1] = coef(fit0)[[3]]
    dtime[i-1] = log(2)/coef(fit0)[[3]]
    ltime[i-1] = coef(fit0)[[4]]
    sod[i-1] = df[[10,i]]
  }
  
  out$MaxGrowthRate = maxgr
  out$DoubleTime = dtime
  out$SatOD = sod
  out <- data.frame(matrix(unlist(out), nrow=length(maxgr)), stringsAsFactors=FALSE)
  out$Sugar <- s
  out$Sample = colnames(df[2:17])
  
  all.dat <- rbind(all.dat, out)
}

colnames(all.dat) <- c('MaxGR','DTime','SOD','Sugar','Sample')

for (s in 1:length(all.dat$Sample)) {
  l <- length(strsplit(all.dat$Sample[s],'[.]')[[1]])
  all.dat$biorep[s] <-strsplit(all.dat$Sample[s],'[.]')[[1]][1]
  all.dat$techrep[s] <- sprintf('%s.%s',strsplit(all.dat$Sample[s],'[.]')[[1]][1],strtrim(strsplit(all.dat$Sample[s],'[.]')[[1]][l],1))
} 

ggplot(all.dat) +
  geom_point(aes(x = Sample, y = DTime, col = Sugar), size = 3) +
  labs(x = 'Strains',
       y = 'Generation Time (min.)') +
  theme_linedraw() +
  scale_y_continuous(breaks = seq(0,300,10)) +
  theme(axis.text.x = element_text(angle = 90))
ggsave(sprintf('%snelson_mutants.png',path.out),
       height = 5, width = 5,
       dpi = 300)

ggplot(all.dat) +
  geom_boxplot(aes(x = biorep, y = DTime, fill = biorep)) +
  facet_grid(~Sugar) +
  labs(x = '',
       y = 'Generation Time (min.)') +
  scale_y_continuous(breaks = seq(0,300,10)) +
  scale_fill_discrete(name = 'Strains',
                      breaks = c('No','IO','Scer','Spar'),
                      labels = c('No ORF', 'IO', 'Scer', 'Spar')) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90))
ggsave(sprintf('%snelson_mutants_biorep.png',path.out),
       height = 5, width = 10,
       dpi = 300)

ggplot(all.dat) +
  geom_boxplot(aes(x = techrep, y = DTime, fill = biorep)) +
  facet_grid(~Sugar) +
  labs(x = '',
       y = 'Generation Time (min.)') +
  scale_y_continuous(breaks = seq(0,300,10)) +
  scale_fill_discrete(name = 'Strains',
                      breaks = c('No','IO','Scer','Spar'),
                      labels = c('No ORF', 'IO', 'Scer', 'Spar')) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90))
ggsave(sprintf('%snelson_mutants_techrep.png',path.out),
       height = 5, width = 10,
       dpi = 300)

write.csv(all.dat,sprintf('%snelson_mutants.csv',path.out))
