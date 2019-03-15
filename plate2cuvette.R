##### CUVETTEDATA
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 09/12/2018
##### Edited  : 02/22/2019 by Nelson Coelho
##### Edited  : 02/27/2019 by Saurin Parikh

##### INITIALIZATION
#install.packages("readxl")
library(readxl)
d <- read_excel("Exp0719.xlsx",
                col_types = "numeric") #choose input file here

##### MODELS
cdata80<-function(x) {
  cdata80 <- 0.0361 + 4.826*x + -0.7644*x^2 + 3.8851*x^3
  cdata80
}

cdata100<-function(x) {
  cdata100 <- 0.1144 + 2.4593*x + 2.8649*x^2 + 1.3729*x^3
  cdata100
}

cdata125<-function(x) {
  cdata125 <- 0.006 + 3.523*x + -1.3029*x^2 + 3.3713*x^3
  cdata125
}

cdata150<-function(x) {
  cdata150 <- 0.0081 + 3.1832*x + -1.5675*x^2 + 3.4147*x^3
  cdata150
}

##### CONVERSION
mod <- cdata125 #choose model here

out = NULL
out$Time = d$Time

for (i in 2:length(d)){
  #out[colnames(d)[i]] <- d[i]
  out[paste('C', colnames(d)[i])] <- mod(d[i])
}

##### OUTPUT
write.csv(out,'Exp0719C.csv') #name output file

##### END
