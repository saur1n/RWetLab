##### PATH LENGTH CORRECTION EXPLORATION
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 05/23/2019

##### INITIALIZATION
#install.packages("readxl")
library(readxl)
library(ggplot2)

d <- read_excel("rawData/pathlengthdata.xlsx",col_types = "numeric")
colnames(d) <- c('ul80','ul100','ul125','ul150','C')

fit <- lm(C~ul125+I(ul125^2)+I(ul125^3)+I(ul125^4), data=d)
pC <- predict(fit)

qplot(C, ul125, data=d, geom="line")
last_plot() + geom_line(aes(x=pC, y=ul125), col=2)

summary(fit)$r.squared
summary(fit)$adj.r.squared


pr <- read_excel("rawData/Exp16_19_RawData.xlsx",col_types = "numeric")

dim(pr)
pr$Time <- seq(0,15*(dim(pr)[1]-1),15)
pr <- pr[1:89,]

for (i in 2:dim(pr)[2]) {
  temp <- data.frame(ul125 = pr[[i]])
  pr[[i]] <- predict(fit, temp)
}

df<- pr


