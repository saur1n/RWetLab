##### PLATE READER EXPLORATION 
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 08/15/2019

##### INITIALIZATION
library(readxl)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(growthrates)

path.out = 'outData/final_analysis/'

plc1 <- read_excel("rawData/pathlengthdata_old.xlsx",col_types = "numeric") #blank corrected data
colnames(plc1) <- c('ul150','ul125','ul100','ul80','C')
plc2 <- read_excel("rawData/pathlengthdata.xlsx",col_types = "numeric") #blank corrected data
colnames(plc2) <- c('ul80','ul100','ul125','ul150','C')
plc3 <- read_excel("rawData/plcnewdata.xlsx",col_types = "numeric") #no blank correction
colnames(plc3) <- c('C','ul125','ul125s','ul125ns9','ul125s9')
plc4 <- read_excel("rawData/pathlength_michael.xlsx",col_types = "numeric") #no blank correction
colnames(plc4) <- c('C','rep1','rep2','rep3','rep4','rep5','ul125')

##### MAKING THE MODELS
fit1 <- lm(C~ul125+I(ul125^2)+I(ul125^3), data=plc1)
# summary(fit1)
summary(fit1)$r.squared
pC1 <- predict(fit1, data.frame(ul125 = seq(0,1.4,0.01)))

fit2 <- lm(C~ul125+I(ul125^2)+I(ul125^3), data=plc2)
# summary(fit2)
summary(fit2)$r.squared
pC2 <- predict(fit2, data.frame(ul125 = seq(0,1.4,0.01)))

fit3 <- lm(C~ul125+I(ul125^2)+I(ul125^3), data=plc3)
# summary(fit3)
summary(fit3)$r.squared
pC3 <- predict(fit3, data.frame(ul125 = seq(0,1.4,0.01)))

fit4 <- lm(C~ul125+I(ul125^2)+I(ul125^3), data=plc4)
# summary(fit4)
summary(fit4)$r.squared
pC4 <- predict(fit4, data.frame(ul125 = seq(0,1.4,0.01)))

ggplot() +
  geom_line(data = data.frame(pC1), aes(x=pC1, y=seq(0,1.4,0.01),col='plc1')) +
  geom_point(data = plc1, aes(C,ul125,col='plc1'), size = 3, shape = 7) +
  geom_line(data = data.frame(pC2), aes(x=pC2, y=seq(0,1.4,0.01),col='plc2')) +
  geom_point(data = plc2, aes(C,ul125,col='plc2'), size = 3, shape = 7) +
  geom_line(data = data.frame(pC3), aes(x=pC3, y=seq(0,1.4,0.01),col='plc3')) +
  geom_point(data = plc3, aes(C,ul125,col='plc3'), size = 3, shape = 7) +
  geom_line(data = data.frame(pC4), aes(x=pC4, y=seq(0,1.4,0.01),col='plc4')) +
  geom_point(data = plc4, aes(C,ul125,col='plc4'), size = 3, shape = 7) +
  labs(title="Path Length Correction Attempts",
       subtitle="FYV4 | YPDA | ul125", 
       x ="Cuvette OD", y = "Plate OD") +
  scale_color_manual(name = 'Attempts',
                     breaks = c('plc1','plc2','plc3','plc4'),
                     values = c('plc1'='#D32F2F', 'plc2'='#536DFE', 'plc3'='#388E3C', 'plc4'='#E040FB'),
                     labels = c('#1', '#2', '#3 W/O BC', '#4 W M')) +
  theme_light() + 
  theme(panel.grid.minor = element_line(colour="grey30", size=0.1)) +
  scale_y_continuous(breaks = seq(0,10,0.2), minor_breaks = seq(0,10,0.1)) +
  scale_x_continuous(breaks = seq(0,10,1), minor_breaks = seq(0,10,0.5)) +
  coord_cartesian(xlim = c(0,10), ylim = c(0,1.4))
ggsave(sprintf('%sPLC_ATTEMPTS.png',path.out),
       width = 10, height = 10) 

ggplot(plc4) +
  geom_line(aes(C,ul125)) +
  geom_point(aes(C,rep1,col='rep1'), size = 3) +
  geom_point(aes(C,rep2,col='rep2'), size = 3) +
  geom_point(aes(C,rep3,col='rep3'), size = 3) +
  geom_point(aes(C,rep4,col='rep4'), size = 3) +
  geom_point(aes(C,rep5,col='rep5'), size = 3) +
  labs(title = 'Variability in Measurements',
       x = 'Cuvette OD',
       y = 'Plate OD') +
  scale_color_discrete(name = 'Replicate',
                       breaks = c('rep1','rep2','rep3','rep4','rep5'),
                       labels = c('','','','','')) +
  theme_linedraw()
ggsave(sprintf('%sVARIABILITY.png',path.out),
       width = 10, height = 10) 
# save(fit, file = 'plc_models/plc_fy125mps.rda')

##### USING MODELS ON REAL DATA
d <- read_excel("rawData/Exp16_19_RawData.xlsx",col_types = "numeric")
sample.names <- read.table("rawData/Exp16_19_SpreedSheet.txt", header = TRUE, sep = "\t",stringsAsFactors = 0)
expt.name = 'Exp16'

d$Time <- seq(0,15*(dim(d)[1]-1),15)
#d$Time <- d$Time*60
d <- d[1:89,] # just need the first 22 hours in Expt16
df3 <- d
blank1 = d$A1[1]
blank2 = d$E1[1]
d[,3:49] <- d[,3:49] - blank1
d[,51:97] <- d[,51:97] - blank2

df1 <- d
for (i in 2:dim(df1)[2]) {
  temp <- data.frame(ul125 = df1[[i]])
  df1[[i]] <- predict(fit1, temp)
}
# colnames(df1) <- c('Time',sample.names$Sample.Name)

df2 <- d
for (i in 2:dim(df2)[2]) {
  temp <- data.frame(ul125 = df2[[i]])
  df2[[i]] <- predict(fit2, temp)
}
# colnames(df2) <- c('Time',sample.names$Sample.Name)

for (i in 2:dim(df3)[2]) {
  temp <- data.frame(ul125 = df3[[i]])
  df3[[i]] <- predict(fit3, temp)
}
# colnames(df3) <- c('Time',sample.names$Sample.Name)
blank1 = df3$A1[1]
blank2 = df3$E1[1]
df3[,3:49] <- df3[,3:49] - blank1
df3[,51:97] <- df3[,51:97] - blank2

s1 <- ggplot() +
  geom_line(data = df1, aes(x=df1$Time, y=df1$A2, col = 'plc1')) +
  geom_line(data = df2, aes(x=df2$Time, y=df2$A2, col = 'plc2')) +
  geom_line(data = df3, aes(x=df3$Time, y=df3$A2, col = 'plc3')) +
  geom_line(data = d, aes(x=d$Time, y=d$A2, col = 'raw')) +
  labs(title="Path Length Correction Results \nFYV4 | YPDA | ul125 | 0.25",
       subtitle="Sample 1", 
       x ="Time", y = "PLC OD") +
  scale_color_manual(name = 'Attempts',
                     breaks = c('plc1','plc2','plc3','raw'),
                     values = c('plc1'='#D32F2F', 'plc2'='#536DFE', 'plc3'='#388E3C', 'raw' = 'black'),
                     labels = c('#1', '#2', '#3', 'RAW')) +
  theme_light() + 
  theme(panel.grid.minor = element_line(colour="grey30", size=0.1)) +
  scale_y_continuous(breaks = seq(-10,10,0.5), minor_breaks = seq(-10,10,0.1)) +
  scale_x_continuous(breaks = seq(0,5000,200), minor_breaks = seq(0,5000,100)) +
  coord_cartesian(xlim = c(0,1300), ylim = c(0, 7))

s2 <- ggplot() +
  geom_line(data = df1, aes(x=df1$Time, y=df1$A5, col = 'plc1')) +
  geom_line(data = df2, aes(x=df2$Time, y=df2$A5, col = 'plc2')) +
  geom_line(data = df3, aes(x=df3$Time, y=df3$A5, col = 'plc3')) +
  geom_line(data = d, aes(x=d$Time, y=d$A5, col = 'raw')) +
  labs(title="\n",
       subtitle="Sample 2", 
       x ="Time", y = "PLC OD") +
  scale_color_manual(name = 'Attempts',
                     breaks = c('plc1','plc2','plc3','raw'),
                     values = c('plc1'='#D32F2F', 'plc2'='#536DFE', 'plc3'='#388E3C','raw'='black'),
                     labels = c('#1', '#2', '#3', 'RAW')) +
  theme_light() + 
  theme(panel.grid.minor = element_line(colour="grey30", size=0.1)) +
  scale_y_continuous(breaks = seq(-10,10,0.5), minor_breaks = seq(-10,10,0.1)) +
  scale_x_continuous(breaks = seq(0,5000,200), minor_breaks = seq(0,5000,100)) +
  coord_cartesian(xlim = c(0,1300), ylim = c(0, 7))

s3 <- ggplot() +
  geom_line(data = df1, aes(x=df1$Time, y=df1$A8, col = 'plc1')) +
  geom_line(data = df2, aes(x=df2$Time, y=df2$A8, col = 'plc2')) +
  geom_line(data = df3, aes(x=df3$Time, y=df3$A8, col = 'plc3')) +
  geom_line(data = d, aes(x=d$Time, y=d$A8, col = 'raw')) +
  labs(title="\n",
       subtitle="Sample 3", 
       x ="Time", y = "PLC OD") +
  scale_color_manual(name = 'Attempts',
                     breaks = c('plc1','plc2','plc3','raw'),
                     values = c('plc1'='#D32F2F', 'plc2'='#536DFE', 'plc3'='#388E3C','raw'='black'),
                     labels = c('#1', '#2', '#3', 'RAW')) +
  theme_light() + 
  theme(panel.grid.minor = element_line(colour="grey30", size=0.1)) +
  scale_y_continuous(breaks = seq(-10,10,0.5), minor_breaks = seq(-10,10,0.1)) +
  scale_x_continuous(breaks = seq(0,5000,200), minor_breaks = seq(0,5000,100)) +
  coord_cartesian(xlim = c(0,1300), ylim = c(0, 7)) 

ggarrange(s1, s2, s3,
          nrow = 1,
          common.legend = T,
          legend = 'bottom')           
ggsave(sprintf('%sRES_YPDA_25.png',path.out),
       width = 12, height = 5)            

##### GROWTHRATE DATA
maxgr_plc1 <- NULL
dtime_plc1 <- NULL
ltime_plc1 <- NULL
maxgr_plc2 <- NULL
dtime_plc2 <- NULL
ltime_plc2 <- NULL
maxgr_plc3 <- NULL
dtime_plc3 <- NULL
ltime_plc3 <- NULL


sample <- c(3,6,9,14,17,20,26,29,32)
sod <- c(0.25,0.25,0.25,0.125,0.125,0.125,0.0625,0.0625,0.0625)
i = 0
for (s in sample) {
  i = i + 1
  fit <- fit_easylinear(df1$Time, df1[[s]], h=14, quota = 1)
  maxgr_plc1[i] = coef(fit)[[3]]
  dtime_plc1[i] = log(2)/coef(fit)[[3]]
  ltime_plc1[i] = coef(fit)[[4]]
  
  fit <- fit_easylinear(df2$Time, df2[[s]], h=14, quota = 1)
  maxgr_plc2[i] = coef(fit)[[3]]
  dtime_plc2[i] = log(2)/coef(fit)[[3]]
  ltime_plc2[i] = coef(fit)[[4]]
  
  fit <- fit_easylinear(df3$Time, df3[[s]], h=14, quota = 1)
  maxgr_plc3[i] = coef(fit)[[3]]
  dtime_plc3[i] = log(2)/coef(fit)[[3]]
  ltime_plc3[i] = coef(fit)[[4]]
}

growth.dat <- data.frame(sod,maxgr_plc1,dtime_plc1,ltime_plc1,
                         maxgr_plc2,dtime_plc2,ltime_plc2,
                         maxgr_plc3,dtime_plc3,ltime_plc3)
write.csv(growth.dat, sprintf('%sgrowth_data_16.csv',path.out))
