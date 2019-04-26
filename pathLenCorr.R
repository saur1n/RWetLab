##### PATH LENGTH CORRECTION EXPLORATION
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 05/23/2019

##### INITIALIZATION
#install.packages("readxl")
library(readxl)
library(ggplot2)

dd <- read_excel("rawData/pathlengthdata.xlsx",col_types = "numeric")
colnames(dd) <- c('ul80','ul100','ul125','ul150','C')

fit <- lm(C~ul125+I(ul125^2)+I(ul125^3)+I(ul125^4), data=dd)
pC <- predict(fit)

ggplot(dd, aes(x=C,y=ul125)) + 
  geom_point(na.rm = TRUE,alpha=0.7,col=4,size=3,shape=7) +
  geom_line(aes(x=pC, y=ul125),alpha=0.5,col=2,size=1.5) +
  labs(title="Path Length Correction",
       x ="Cuvette OD", y = "Plate OD") +
  theme_light() + 
  theme(panel.grid.minor = element_line(colour="grey30", size=0.1)) +
  scale_y_continuous(breaks = seq(0,10,0.2), minor_breaks = seq(0,10,0.1)) +
  scale_x_continuous(breaks = seq(0,10,1), minor_breaks = seq(0,10,0.5))

summary(fit)$r.squared
summary(fit)$adj.r.squared
save(fit, file = 'plc_models/plcor125.rda')

##### CHECKING STARTING ODs
invfit <- lm(ul125~C+I(C^2)+I(C^3)+I(C^4), data=dd)

temp <- data.frame(C = c(0.25,0.125,0.0625,0.03125))
cstart <- predict(invfit, temp)
cstart

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
  
  
  
  
  