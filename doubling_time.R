##### WELL-WISE DOUBLING TIME FROM PLATEREADER DATA
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 08/31/2018

##### INITIALIZATION
#install.packages("readxl")
library(readxl)
dat <- read_excel("~/Desktop/Plate Reader excel format.xlsx",
                  col_types = "numeric")
dat$Time[length(dat$Time)] <- 1
dat$Time <- dat$Time*(24*60)

##### SLOPE FUNCTION
deriv_coef<-function(x) {
  x <- coef(x)
  stopifnot(names(x)[1]=="(Intercept)")
  y <- x[-1]
  stopifnot(all(grepl("^poly", names(y))))
  px <- as.numeric(gsub("poly\\(.*\\)","",names(y)))
  rr <- setNames(c(y * px, 0), names(x))
  rr[is.na(rr)] <- 0
  rr
}

##### LOOP THROUGH WELLS
dt = dat[0,3:length(dat)]
dt[1,] = 0

for (i in 3:length(dat[,3:length(dat)])){
  cell <- dat[,c(1,i)]
  colnames(cell)[2] <- 'Value'
  fit <- lm(log(cell$Value)~poly(cell$Time,6,raw=TRUE))
  slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)
  cell$Slope <- slope
  dt[c(i-2)] = log(2, base=exp(1))/max(cell$Slope)
  rm(cell)
}

##### OUTPUT
write.csv(dt,'~/Desktop/doubling time.csv')

##### END
