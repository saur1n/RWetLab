##### CELL GROWTH FEATURES
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 09/11/2018

##### INITIALIZATION
#install.packages("readxl")
#install.packages("growthcurver")
library(readxl)
library(growthcurver)

d <- read_excel("Exp0719.xlsx",
                  col_types = "numeric")

##### CLEAN DATA
d$Time[length(d$Time)] <- 2
d$Time <- d$Time*(24*60)
d <- d[,c(1,3:length(d))]

##### CHECK
gc_fit <- SummarizeGrowth(d$Time, d$A1)
plot(gc_fit)

##### FIT LINES FOR ALL WELLS
gc_out <- SummarizeGrowthByPlate(d, plot_fit = TRUE, 
                                 plot_file = "R/TestFiles/gc_plots.pdf")

##### OUTPUT CSV FILE
write.csv(gc_out,'R/TestFiles/cellgrowth.csv')

##### END
