##### PATH LENGTH CORRECTION EXPLORATION
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 05/23/2019

##### INITIALIZATION
#install.packages("readxl")
library(readxl)
library(ggplot2)
library(gridExtra)

dd <- read_excel("rawData/plcnewdata.xlsx",col_types = "numeric")
#colnames(dd) <- c('ul80','ul100','ul125','ul150','C')
colnames(dd) <- c('C','ul125','ul125s','ul125ns9','ul125s9')
##### FIT
fit <- lm(C~ul125+I(ul125^2)+I(ul125^3), data=dd)
summary(fit)
pC <- predict(fit)

ggplot(dd, aes(x=C,y=ul125)) + 
  geom_point(na.rm = TRUE,alpha=0.7,col=4,size=3,shape=7) +
  geom_line(aes(x=pC, y=ul125),alpha=0.5,col=2,size=1.5) +
  labs(title="Path Length Correction",
       subtitle="FYV4 | YPDA | ul125 | Single Point | No Shake", 
       x ="Cuvette OD", y = "Plate OD") +
  theme_light() + 
  theme(panel.grid.minor = element_line(colour="grey30", size=0.1)) +
  scale_y_continuous(breaks = seq(0,10,0.2), minor_breaks = seq(0,10,0.1)) +
  scale_x_continuous(breaks = seq(0,10,1), minor_breaks = seq(0,10,0.5)) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,0.25))

summary(fit)$r.squared
summary(fit)$adj.r.squared
save(fit, file = 'plc_models/plc_fy125mps.rda')

##### CHECKING STARTING ODs
invfit <- lm(ul125~C+I(C^2)+I(C^3), data=dd)

temp <- data.frame(C = c(0.25,0.125,0.0625,0.03125))
cstart <- predict(invfit, temp)
cstart

##### BLANK CORRECTION TRIAL
bc <- seq(0,0.5,0.005)
temp.c <- data.frame(C = 0.25)
temp.cstart <- NULL
for (b in bc) {
  temp.dd <- dd
  temp.dd$ul125 <- temp.dd$ul125 + b
  temp.invfit <- lm(ul125~C+I(C^2)+I(C^3), data=temp.dd)
  temp.cstart <- rbind(temp.cstart, c(b, predict(temp.invfit, temp.c)))
}
colnames(temp.cstart) <- c('Blank','ul125')
temp.cstart <- data.frame(temp.cstart)

p1 <- ggplot(temp.cstart, aes(x=Blank,y=ul125)) + 
  geom_point(na.rm = TRUE,alpha=0.7,col=4,size=1,shape=21) +
  labs(title="Blank vs 125ul Starting OD for Cuvette OD of 0.25",
       x ="Blank", y = "125ul Starting OD") +
  theme_light() + 
  theme(panel.grid.minor = element_line(colour="grey30", size=0.1)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.025)) +
  scale_x_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.025))
p1
##### UNDO BLANK CORRECTION
dd$ul125 <- dd$ul125 + 0.15
fit_nobc <- lm(C~ul125+I(ul125^2)+I(ul125^3), data=dd)
summary(fit_nobc)
pC_nobc <- predict(fit_nobc)
save(fit_nobc, file = 'plc_models/plcor125_nobc.rda')

p2 <- ggplot(dd, aes(x=C,y=ul125)) + 
  geom_point(na.rm = TRUE,alpha=0.7,col=4,size=3,shape=7) +
  geom_line(aes(x=pC_nobc, y=ul125),alpha=0.5,col=2,size=1.5) +
  labs(title="Path Length Correction",
       x ="Cuvette OD", y = "Plate OD") +
  theme_light() + 
  theme(panel.grid.minor = element_line(colour="grey30", size=0.1)) +
  scale_y_continuous(breaks = seq(0,10,0.2), minor_breaks = seq(0,10,0.1)) +
  scale_x_continuous(breaks = seq(0,10,1), minor_breaks = seq(0,10,0.5))
grid.arrange(p2, p1, nrow = 1)

##### OLD DATA
dd0 <- read_excel("rawData/pathlengthdata_old.xlsx",col_types = "numeric")
colnames(dd0) <- c('ul150','ul125','ul100','ul80','C')

fit0 <- lm(C~ul125+I(ul125^2)+I(ul125^3)+I(ul125^4), data=dd0)
pC0 <- predict(fit0)

ggplot(dd0, aes(x=C,y=ul125)) + 
  geom_point(na.rm = TRUE,alpha=0.7,col=4,size=3,shape=7) +
  geom_line(aes(x=pC0, y=ul125),alpha=0.5,col=2,size=1.5) +
  labs(title="Path Length Correction (Old Data)",
       x ="Cuvette OD", y = "Plate OD") +
  theme_light() + 
  theme(panel.grid.minor = element_line(colour="grey30", size=0.1)) +
  scale_y_continuous(breaks = seq(0,10,0.2), minor_breaks = seq(0,10,0.1)) +
  scale_x_continuous(breaks = seq(0,10,1), minor_breaks = seq(0,10,0.5))

summary(fit0)$r.squared
summary(fit0)$adj.r.squared
save(fit0, file = 'plc_models/plcor125_old.rda')

invfit0 <- lm(ul125~C+I(C^2)+I(C^3)+I(C^4), data=dd0)

temp0 <- data.frame(C = c(0.25,0.125,0.0625,0.03125))
cstart0 <- predict(invfit0, temp0)
cstart0

##### PLOTTING BOTH

ggplot() + 
  geom_line(data=dd0,aes(x=ul125,y=pC0,col='Old Fit'),alpha=1,size=1.5) +
  geom_point(data=dd0,aes(x=ul125,y=C,col='Old Data'),na.rm = TRUE,alpha=1,size=4,shape=20) +
  geom_line(data=dd,aes(x=ul125,y=pC,col='New Fit'),alpha=1,size=1.5) +
  geom_point(data=dd,aes(x=ul125,y=C,col='New Data'),na.rm=TRUE,alpha=1,size=3,shape=7) +
  scale_color_manual(name="Legend", 
                     labels = c("New Data", 
                                "New Fit", 
                                "Old Data", 
                                "Old Fit"), 
                     values = c("blue",
                                "red", 
                                "darkgreen", 
                                "orange")) +
  labs(title="Path Length Correction (125 ul)",
       x ="Plate OD", y = "Cuvette OD") +
  guides(col = guide_legend()) +
  theme_linedraw() + 
  theme(panel.grid.minor = element_line(colour="grey30", size=0.1)) +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = seq(0,10,0.2), minor_breaks = seq(0,10,0.1)) +
  scale_y_continuous(breaks = seq(0,10,1), minor_breaks = seq(0,10,0.5))
  
  
  
  
  