##### (P)ATH (L)ENGTH (COR)RECTION
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 03/28/2019

plcor <- function(d,vol) {
  mod <- function(x,vol) {
    if (vol == 80) {
      mod <- 0.0361 + 4.826*x + -0.7644*x^2 + 3.8851*x^3
    } else if (vol == 100) {
      mod <- 0.1144 + 2.4593*x + 2.8649*x^2 + 1.3729*x^3
    } else if (vol == 125) {
      mod <- 0.006 + 3.523*x + -1.3029*x^2 + 3.3713*x^3
    } else {
      mod <- 0.0081 + 3.1832*x + -1.5675*x^2 + 3.4147*x^3
    }
  }
  
  out = NULL
  out$Time = d$Time
  
  for (i in 2:length(d)){
    out[colnames(d)[i]] <- mod(d[i],vol)
  }
  
  df <- data.frame(matrix(unlist(out), nrow=dim(d)[1]), stringsAsFactors=FALSE)
  colnames(df) <- names(d)
  return(df)
}
