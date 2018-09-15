##### PLATE2CUVETTE DATA
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 09/12/2018

##### INITIALIZATION
#install.packages("readxl")
library(readxl)
d <- read_excel("R/TestFiles/ExampleFile_toConvert.xlsx",
                col_types = "numeric") #choose input file here

##### MODELS
cdata80<-function(x) {
  cdata80 <- -0.006736978 + 9.512395672*x + -19.558627408*x^2 + 13.647990483*x^3
  cdata80
}

cdata100<-function(x) {
  cdata100 <- 0.01216283 + 7.00126637*x + -12.01036645*x^2 + 9.30138057*x^3
  cdata100
}

cdata125<-function(x) {
  cdata125 <- -0.006138794 + 5.063691454*x + -5.912728578*x^2 + 3.637915896*x^3
  cdata125
}

cdata150<-function(x) {
  cdata150 <- -0.01249084 + 4.44896381*x + -4.89253447*x^2 + 2.82561886*x^3
  cdata150
}

##### CONVERSION
mod <- cdata125 #choose model here

out = NULL
out$Time = d$Time

for (i in 2:length(d)){
  out[colnames(d)[i]] <- d[i]
  out[paste('cuvette', colnames(d)[i])] <- mod(d[i])
}

##### OUTPUT
write.csv(out,'R/TestFiles/cuvette values.csv') #name output file

##### END
