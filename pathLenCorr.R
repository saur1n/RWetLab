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
  theme(panel.grid.minor = element_line(colour="blue", size=0.1)) +
  scale_y_continuous(breaks = seq(0,10,0.2), minor_breaks = seq(0,10,0.1)) +
  scale_x_continuous(breaks = seq(0,10,1), minor_breaks = seq(0,10,0.5))

summary(fit)$r.squared
summary(fit)$adj.r.squared
save(fit, file = 'plc_models/plcor125.rda')

##### CHECKING STARTING ODs
invfit <- lm(ul125~C+I(C^2)+I(C^3)+I(C^4), data=dd)

temp <- data.frame(C = c(0.25,0.125,0.0625,0.03125))
cstart <- predict(invfit, temp)


pr <- read_excel("rawData/Exp16_19_RawData.xlsx",col_types = "numeric")

dim(pr)
pr$Time <- seq(0,15*(dim(pr)[1]-1),15)
pr <- pr[1:89,]

for (i in 2:dim(pr)[2]) {
  temp <- data.frame(ul125 = pr[[i]])
  pr[[i]] <- predict(fit, temp)
}

df<- pr


