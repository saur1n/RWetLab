##### BIOTEK PLC
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 02/04/2020

##### INITIALIZATION
library(ggplot2)
library(ggExtra)
library(gridExtra)
library(ggpubr)
library(reshape2)
library(stringr)

path.out <- 'outData/biotek/'
path.out.gc <- 'outData/biotek/gc/'

df <- read.csv("rawData/biotek/PLC_CLEAN.csv")

# df$EP <- rowMeans(df[,6:8])
# df$KIN <- rowMeans(df[,9:10])

df$EP_blank[df$Media == 'YPDA'] <- mean(df$EP[df$Sample == 'blank' & df$Media == 'YPDA'])
df$EP_blank[df$Media == 'SC-URA+GLU'] <- mean(df$EP[df$Sample == 'blank' & df$Media == 'YPDA'])

fit.kin <- lm(CuvetteOD~KIN+I(KIN^2)+I(KIN^3), data=df[df$Media != 'water',])
summary(fit.kin)
pOD.kin <- predict(fit.kin)

fit.kin.ypda <- lm(CuvetteOD~KIN+I(KIN^2)+I(KIN^3), data=df[df$Media == 'YPDA',])
summary(fit.kin.ypda)
pOD.kin.ypda <- predict(fit.kin.ypda)


fit.kin.sc <- lm(CuvetteOD~KIN+I(KIN^2)+I(KIN^3), data=df[df$Media == 'SC-URA+CASE+GLU',])
summary(fit.kin.sc)
pOD.kin.sc <- predict(fit.kin.sc)


ggplot(df[df$Media != 'water',]) +
  # geom_point(aes(x = CuvetteOD, y = EP, col = Media, shape = 'EP')) +
  geom_point(aes(x = CuvetteOD, y = KIN, col = Media, shape = 'KIN')) +
  geom_line(aes(x=pOD.kin, y=KIN),alpha=0.5,col=2,size=1.5) +
  geom_line(data = df[df$Media == 'YPDA',],
            aes(x=pOD.kin.ypda, y=KIN),alpha=0.5,col=1,size=1.5) +
  geom_line(data = df[df$Media == 'SC-URA+CASE+GLU',],
            aes(x=pOD.kin.sc, y=KIN),alpha=0.5,col=3,size=1.5)

save(fit.kin, file = 'plc_models/biotek_plc.rda')

