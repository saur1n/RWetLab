##### GENTIME
##### Date    : 02/18/2018

#####
data<-read.csv("~/R/Scripts/Exp0719.csv")
slopes<-array()
gentime<-array()
for(i in 2:length(data[1,]))
{
  sel<-which(data[,i]>.2 & data[,i]<.8)
  if(length(sel)>1)
  {
    x<-data[sel,1]
    y<-log(data[sel,i])
    slopes[i]<-lm(y~x)[[1]][2]
    gentime[i]<-log(2)/slopes[i]
  } else {
    slopes[i] <- NA
    gentime[i] <- NA
  }
}
plot(x,y)
abline(lm(y~x))

write.csv(cbind(slopes,gentime),"Exp0719_0208.csv")
