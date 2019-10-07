##### before/after PM
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 09/23/2019

##### INITIALIZATION
library(ggplot2)
library(ggExtra)
library(gridExtra)
library(ggpubr)
library(reshape2)

path.out = 'outData/PR_QC/'

c2p <- read.csv("rawData/cuvette.csv")
c2p$date <- as.character(c2p$date)
c2p$od <- as.character(c2p$od)
c2p <- c2p[1:48,1:25]
load('plc_models/plc_fy125sps.rda')

for (ii in 6:25) {
  temp <- data.frame(ul125 = c2p[[ii]])
  c2p[[ii]] <- predict(fit, temp)
}

##### f(well) = variability
ggplot() +
  geom_abline() +
  geom_point(data = c2p,
             aes(x = endpoint_1, y = endpoint_2, col = date, shape = 'rep1-2'),
             size = 3) +
  geom_point(data = c2p,
             aes(x = endpoint_1, y = endpoint_3, col = date, shape = 'rep1-3'),
             size = 3) +
  geom_point(data = c2p,
             aes(x = endpoint_1, y = endpoint_4, col = date, shape = 'rep1-4'),
             size = 3) +
  geom_point(data = c2p,
             aes(x = endpoint_2, y = endpoint_3, col = date, shape = 'rep2-3'),
             size = 3) +
  geom_point(data = c2p,
             aes(x = endpoint_2, y = endpoint_4, col = date, shape = 'rep2-4'),
             size = 3) +
  geom_point(data = c2p,
             aes(x = endpoint_3, y = endpoint_4, col = date, shape = 'rep3-4'),
             size = 3) +
  labs(title = 'Variability in Plate Endpoint OD Reading (w PLC)',
       subtitle = 'when comparing within replicates',
       x = 'Plate Endpoint OD',
       y = 'Plate Endpoint OD') +
  scale_shape_discrete(name = 'REP.') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  theme_linedraw() +
  facet_grid(~sample)
ggsave(sprintf('%sf_well_plc.png',path.out),
       height = 5, width = 10,
       dpi = 300)

# mean(abs(c2p$endpoint_1[c2p$sample == 'BSA'] - c2p$endpoint_2[c2p$sample == 'BSA'])/
#        (rowSums(cbind(c2p$endpoint_1[c2p$sample == 'BSA'], c2p$endpoint_2[c2p$sample == 'BSA']))/2) * 100)
# mean(abs(c2p$endpoint_1[c2p$sample == 'BSA'] - c2p$endpoint_3[c2p$sample == 'BSA'])/
#        (rowSums(cbind(c2p$endpoint_1[c2p$sample == 'BSA'], c2p$endpoint_3[c2p$sample == 'BSA']))/2) * 100)
# mean(abs(c2p$endpoint_1[c2p$sample == 'BSA'] - c2p$endpoint_4[c2p$sample == 'BSA'])/
#        (rowSums(cbind(c2p$endpoint_1[c2p$sample == 'BSA'], c2p$endpoint_4[c2p$sample == 'BSA']))/2) * 100)
# mean(abs(c2p$endpoint_2[c2p$sample == 'BSA'] - c2p$endpoint_3[c2p$sample == 'BSA'])/
#        (rowSums(cbind(c2p$endpoint_2[c2p$sample == 'BSA'], c2p$endpoint_3[c2p$sample == 'BSA']))/2) * 100)
# mean(abs(c2p$endpoint_2[c2p$sample == 'BSA'] - c2p$endpoint_4[c2p$sample == 'BSA'])/
#        (rowSums(cbind(c2p$endpoint_2[c2p$sample == 'BSA'], c2p$endpoint_4[c2p$sample == 'BSA']))/2) * 100)
# mean(abs(c2p$endpoint_3[c2p$sample == 'BSA'] - c2p$endpoint_4[c2p$sample == 'BSA'])/
#        (rowSums(cbind(c2p$endpoint_3[c2p$sample == 'BSA'], c2p$endpoint_4[c2p$sample == 'BSA']))/2) * 100)
# 
# 
# mean(abs(c2p$endpoint_1[c2p$sample == 'CELL'] - c2p$endpoint_2[c2p$sample == 'CELL'])/
#        (rowSums(cbind(c2p$endpoint_1[c2p$sample == 'CELL'], c2p$endpoint_2[c2p$sample == 'CELL']))/2) * 100)
# mean(abs(c2p$endpoint_1[c2p$sample == 'CELL'] - c2p$endpoint_3[c2p$sample == 'CELL'])/
#        (rowSums(cbind(c2p$endpoint_1[c2p$sample == 'CELL'], c2p$endpoint_3[c2p$sample == 'CELL']))/2) * 100)
# mean(abs(c2p$endpoint_1[c2p$sample == 'CELL'] - c2p$endpoint_4[c2p$sample == 'CELL'])/
#        (rowSums(cbind(c2p$endpoint_1[c2p$sample == 'CELL'], c2p$endpoint_4[c2p$sample == 'CELL']))/2) * 100)
# mean(abs(c2p$endpoint_2[c2p$sample == 'CELL'] - c2p$endpoint_3[c2p$sample == 'CELL'])/
#        (rowSums(cbind(c2p$endpoint_2[c2p$sample == 'CELL'], c2p$endpoint_3[c2p$sample == 'CELL']))/2) * 100)
# mean(abs(c2p$endpoint_2[c2p$sample == 'CELL'] - c2p$endpoint_4[c2p$sample == 'CELL'])/
#        (rowSums(cbind(c2p$endpoint_2[c2p$sample == 'CELL'], c2p$endpoint_4[c2p$sample == 'CELL']))/2) * 100)
# mean(abs(c2p$endpoint_3[c2p$sample == 'CELL'] - c2p$endpoint_4[c2p$sample == 'CELL'])/
#        (rowSums(cbind(c2p$endpoint_3[c2p$sample == 'CELL'], c2p$endpoint_4[c2p$sample == 'CELL']))/2) * 100)

##### f(od) = variability
c2p$cv_ep <- NULL
c2p$cv_kin <- NULL
c2p$cv_kin3 <- NULL
c2p$cv_kin5 <- NULL
c2p$cv_kin10 <- NULL
for (i in seq(1,dim(c2p)[1])) {
  c2p$cv_ep[i] <- sd(c2p[i,6:9])
  c2p$cv_kin[i] <- sd(c2p[i,10:13])
  c2p$cv_kin3[i] <- sd(c2p[i,14:17])
  c2p$cv_kin5[i] <- sd(c2p[i,18:21])
  c2p$cv_kin10[i] <- sd(c2p[i,22:25])
}
c2p$cv_ep <- c2p$cv_ep/rowMeans(c2p[6:9]) * 100
c2p$avg_ep <- rowMeans(c2p[6:9])
c2p$cv_kin <- c2p$cv_kin/rowMeans(c2p[10:13]) * 100
c2p$avg_kin <- rowMeans(c2p[10:13])
c2p$cv_kin3 <- c2p$cv_kin3/rowMeans(c2p[14:17]) * 100
c2p$avg_kin3 <- rowMeans(c2p[14:17])
c2p$cv_kin5 <- c2p$cv_kin5/rowMeans(c2p[18:21]) * 100
c2p$avg_kin5 <- rowMeans(c2p[18:21])
c2p$cv_kin10 <- c2p$cv_kin10/rowMeans(c2p[22:25]) * 100
c2p$avg_kin10 <- rowMeans(c2p[22:25])

ggplot(c2p) +
  geom_point(aes(x = avg_ep, y = cv_ep, col = date), size = 3) +
  theme_linedraw() +
  labs(title = 'Variability in Plate Endpoint OD Reading (w PLC)',
       subtitle = 'CV % = STD(replicates) / MEAN(replicates) * 100',
       x = 'Mean Endpoint Plate OD',
       y = 'Coef of Variance (%) of Endpoint Plate OD') +
  scale_y_continuous(breaks = seq(0,100,5)) +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  facet_grid(~sample)
ggsave(sprintf('%sf_od_plc.png',path.out),
       height = 5, width = 10,
       dpi = 300)

ggplot(c2p[c2p$sample == 'CELL',]) +
  geom_hline(yintercept = 0) +
  # geom_abline() +
  geom_point(aes(x = pr, y = cv_kin - cv_kin3, col = date, shape = 't0-3'), size = 3) +
  geom_point(aes(x = pr, y = cv_kin - cv_kin5, col = date, shape = 't0-5'), size = 3) +
  geom_point(aes(x = pr, y = cv_kin3 - cv_kin5, col = date, shape = 't3-5'), size = 3) +
  # geom_point(aes(x = cv_kin - cv_kin3, y = cv_kin - cv_kin5, col = date), size = 3) +
  theme_linedraw() +
  labs(title = 'Change in Variability of Kinetic OD Reading (w PLC)',
       subtitle = 'difference in CV % of replicates',
       x = 'Spectromax Cuvette OD (t = 0)',
       y = 'Difference in Coef of Var. (%) of Kinetic OD') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  scale_shape_discrete(name = 'KIN.') +
  coord_cartesian(xlim = c(0, 1.2),
                  ylim = c(-15, 15)) #+
  # facet_grid(~sample)
ggsave(sprintf('%sf_var_kin_plc.png',path.out),
       height = 5, width = 5,
       dpi = 300)

wilcox.test(x = c2p$cv_kin[c2p$sample == 'CELL'], y = c2p$cv_kin3[c2p$sample == 'CELL'], paired = TRUE)

summary(lm(cv_kin5 ~ sample + pr + date + cv_kin + cv_kin3, c2p))
summary(lm(cv_kin5 ~ cv_kin3, c2p))

# c2p$avg_pr <- NULL
# c2p$rng_pr <- NULL
# c2p$cv_pr <- NULL
# c2p$avg_sp <- NULL
# c2p$rng_sp <- NULL
# c2p$cv_sp <- NULL
# for (s in unique(c2p$sample)) {
#   for (o in unique(c2p$od[c2p$sample == s])) {
#     c2p$avg_pr[c2p$od == o & c2p$sample == s] <- mean(c2p$pr[c2p$od == o & c2p$sample == s])
#     c2p$avg_sp[c2p$od == o & c2p$sample == s] <- mean(c2p$sp[c2p$od == o & c2p$sample == s])
#     c2p$cv_pr[c2p$od == o & c2p$sample == s] <- sd(c2p$pr[c2p$od == o & c2p$sample == s])/mean(c2p$pr[c2p$od == o & c2p$sample == s]) * 100
#     c2p$cv_sp[c2p$od == o & c2p$sample == s] <- sd(c2p$sp[c2p$od == o & c2p$sample == s])/mean(c2p$sp[c2p$od == o & c2p$sample == s]) * 100
#     c2p$rng_pr[c2p$od == o & c2p$sample == s] <- max(c2p$pr[c2p$od == o & c2p$sample == s]) - min(c2p$pr[c2p$od == o & c2p$sample == s])
#     c2p$rng_sp[c2p$od == o & c2p$sample == s] <- max(c2p$sp[c2p$od == o & c2p$sample == s]) - min(c2p$sp[c2p$od == o & c2p$sample == s])
#   }
# }

##### f(day) = variability
# ggplot() +
#   geom_abline() +
#   geom_point(data = c2p[c2p$date == '918',],
#              aes(x = endpoint_1, y = c2p$endpoint_1[c2p$date == '919'], col = '18-19', shape = 'rep1'),
#              size = 3) +
#   geom_point(data = c2p[c2p$date == '918',],
#              aes(x = endpoint_1, y = c2p$endpoint_1[c2p$date == '920'], col = '18-20', shape = 'rep1'),
#              size = 3) +
#   geom_point(data = c2p[c2p$date == '919',],
#              aes(x = endpoint_1, y = c2p$endpoint_1[c2p$date == '920'], col = '19-20', shape = 'rep1'),
#              size = 3) +
#   geom_point(data = c2p[c2p$date == '918',],
#              aes(x = endpoint_2, y = c2p$endpoint_2[c2p$date == '919'], col = '18-19', shape = 'rep2'),
#              size = 3) +
#   geom_point(data = c2p[c2p$date == '918',],
#              aes(x = endpoint_2, y = c2p$endpoint_2[c2p$date == '920'], col = '18-20', shape = 'rep2'),
#              size = 3) +
#   geom_point(data = c2p[c2p$date == '919',],
#              aes(x = endpoint_2, y = c2p$endpoint_2[c2p$date == '920'], col = '19-20', shape = 'rep2'),
#              size = 3) +
#   geom_point(data = c2p[c2p$date == '918',],
#              aes(x = endpoint_3, y = c2p$endpoint_3[c2p$date == '919'], col = '18-19', shape = 'rep3'),
#              size = 3) +
#   geom_point(data = c2p[c2p$date == '918',],
#              aes(x = endpoint_3, y = c2p$endpoint_3[c2p$date == '920'], col = '18-20', shape = 'rep3'),
#              size = 3) +
#   geom_point(data = c2p[c2p$date == '919',],
#              aes(x = endpoint_3, y = c2p$endpoint_3[c2p$date == '920'], col = '19-20', shape = 'rep3'),
#              size = 3) +
#   geom_point(data = c2p[c2p$date == '918',],
#              aes(x = endpoint_4, y = c2p$endpoint_4[c2p$date == '919'], col = '18-19', shape = 'rep4'),
#              size = 3) +
#   geom_point(data = c2p[c2p$date == '918',],
#              aes(x = endpoint_4, y = c2p$endpoint_4[c2p$date == '920'], col = '18-20', shape = 'rep4'),
#              size = 3) +
#   geom_point(data = c2p[c2p$date == '919',],
#              aes(x = endpoint_4, y = c2p$endpoint_4[c2p$date == '920'], col = '19-20', shape = 'rep4'),
#              size = 3) +
#   theme_linedraw() +
#   labs(title = 'Variability in Reading',
#        subtitle = 'as a function of Day',
#        x = 'Plate OD',
#        y = 'Plate OD') +
#   scale_color_discrete(name = 'Dates') +
#   scale_shape_discrete(name = 'REP.') +
#   coord_cartesian(xlim = c(0.05,0.55),
#                   ylim = c(0.05,0.55)) +
#   facet_grid(~sample)
# ggsave(sprintf('%sf_day.png',path.out),
#        height = 10, width = 20,
#        dpi = 300)

##### f(method) = variability
ggplot(c2p) +
  geom_abline() +
  geom_point(aes(x = pr, y = endpoint_1, col = date, shape = 'rep1'),
             size = 3) +
  geom_point(aes(x = pr, y = endpoint_2, col = date, shape = 'rep2'),
             size = 3) +
  geom_point(aes(x = pr, y = endpoint_3, col = date, shape = 'rep3'),
             size = 3) +
  geom_point(aes(x = pr, y = endpoint_4, col = date, shape = 'rep4'),
             size = 3) +
  theme_linedraw() +
  labs(title = 'Relationship Between Cuvette OD & Plate Endpoint OD',
       subtitle = '',
       x = 'Spectromax Cuvette OD',
       y = 'Plate Endpoint OD') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  scale_y_continuous(breaks = seq(0,2,0.25)) +
  scale_shape_discrete(name = 'REP.') +
  # coord_cartesian(xlim = c(0,1.2),
  #                 ylim = c(0.05,0.55)) +
  facet_grid(~sample)
ggsave(sprintf('%sf_meth1.png',path.out),
       height = 5, width = 10,
       dpi = 300)

ggplot() +
  geom_abline() +
  geom_point(data = c2p,
             aes(x = endpoint_1, y = kinetic_1, shape = 'rep1', col = date),
             size = 3) +
  geom_point(data = c2p,
             aes(x = endpoint_2, y = kinetic_2, shape = 'rep2', col = date),
             size = 3) +
  geom_point(data = c2p,
             aes(x = endpoint_3, y = kinetic_3, shape = 'rep3', col = date),
             size = 3) +
  geom_point(data = c2p,
             aes(x = endpoint_4, y =kinetic_4, shape = 'rep4', col = date),
             size = 3) +
  labs(title = 'Relationship Between Plate Endpoint OD and Kinetic_0 OD (w PLC)',
       subtitle = '',
       x = 'Plate Endpoint OD',
       y = 'Plate Kinetic_0 OD') +
  scale_shape_discrete(name = 'REP.') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  facet_wrap(~sample, drop = T) +
  theme_linedraw() #+
  # coord_cartesian(xlim = c(0.1, 0.55),
  #                 ylim = c(0.1, 0.55))
ggsave(sprintf('%sf_meth2_plc.png',path.out),
       height = 5, width = 10,
       dpi = 300)

ggplot() +
  geom_hline(yintercept = 0) +
  geom_point(data = c2p,
             aes(x = pr, y = endpoint_1 - kinetic_1, shape = 'rep1', col = date),
             size = 3) +
  geom_point(data = c2p,
             aes(x = pr, y = endpoint_2 - kinetic_2, shape = 'rep2', col = date),
             size = 3) +
  geom_point(data = c2p,
             aes(x = pr, y = endpoint_3 - kinetic_3, shape = 'rep3', col = date),
             size = 3) +
  geom_point(data = c2p,
             aes(x = pr, y = endpoint_4 - kinetic_4, shape = 'rep4', col = date),
             size = 3) +
  labs(title = 'Difference in Plate Endpoint OD and Kinetic_0 OD (w PLC)',
       subtitle = '',
       x = 'Spectromax Cuvette OD',
       y = 'Plate Endpoint - Kinetic_0 OD') +
  scale_shape_discrete(name = 'REP.') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  facet_wrap(~sample, drop = T) +
  theme_linedraw() #+
  # coord_cartesian(xlim = c(0, 1.2),
  #                 ylim = c(-0.06, 0.06))
ggsave(sprintf('%sf_meth3_plc.png',path.out),
       height = 5, width = 10,
       dpi = 300)

##### f(machine) = variability
ggplot(c2p) +
  geom_point(aes(x = pr, y = sp, col = date),
             size = 3) +
  theme_linedraw() +
  labs(title = 'Relationship Between Spectromax & Spectrophotometer Cuvette Readings',
       subtitle = '',
       x = 'Spectromax Cuvette OD',
       y = 'Spectrophotometer Cuvette OD') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  coord_cartesian(xlim = c(0,1.2),
                  ylim = c(0,1.2)) +
  facet_grid(~sample)
ggsave(sprintf('%sf_mach.png',path.out),
       height = 5, width = 10,
       dpi = 300)

summary(lm(pr ~ sp, data = c2p[c2p$sample == 'CELL',]))

##### LINEAR MODELS
library(lme4)

fit_lm <- lm(cv_ep ~ sample + pr + date, c2p)
summary(fit_lm)

fit.sample <- lmer(cv_ep ~ sample + pr + (sample|date),
                   data = c2p)
summary(fit.sample)
fit.null <- lmer(cv_ep ~ pr + (1|date),
                 data = c2p)
summary(fit.null)

anova(fit.null, fit.sample)


##### KINETIC
library(locfit)
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("cellGrowth")
library(growthrates)
library(stringr)
kin <- read.csv("rawData/kinetic.csv")
plate.des <- read.csv("rawData/platedesign.csv")
plate.des$Date <- as.character(plate.des$Date)
all.dat <- NULL

for (date in unique(kin$Date)) {
  d <- kin[kin$Date == date,]
  d <- d[,2:dim(d)[2]]
  ##### INITIALIZE OUTPUT
  out = NULL
  maxgr = NULL
  dtime = NULL
  ltime = NULL
  sod = NULL
  
  ##### CLEAN DATA
  d$Time <- d$Time*60
  
  ##### PATH LENGTH CORRECTION
  # df <- plcor(d,125)
  df <- d
  
  for (i in 3:length(df)) {
    fit0 <- fit_easylinear(df$Time, df[[i]], h=2, quota = 1);
    jpeg(sprintf('%sgrowthcurves/%s_%s_GC.png',
                 path.out,
                 date,
                 colnames(d[i])),
         width=600, height=600)
    plot(fit0, log = 'y',
         main=sprintf('%s | Doubling Time = %0.2f mins',
                      colnames(d[i]),
                      log(2)/coef(fit0)[[3]]),
         ylim = c(0.1,1.2))
    dev.off()
    maxgr[i-2] = coef(fit0)[[3]]
    dtime[i-2] = log(2)/coef(fit0)[[3]]
    ltime[i-2] = coef(fit0)[[4]]
    sod[i-2] = df[[14,i]]
  }
  
  out$MaxGrowthRate = maxgr
  out$DoubleTime = dtime
  out$SatOD = sod
  out <- data.frame(matrix(unlist(out), nrow=length(maxgr)), stringsAsFactors=FALSE)
  out$Date <- date
  out$Sample = plate.des$Sample[plate.des$Date == date]
  out$OD = plate.des$OD[plate.des$Date == date]
  out$PR_OD = plate.des$OOD[plate.des$Date == date]
  
  all.dat <- rbind(all.dat, out)
}

colnames(all.dat) <- c('MaxGR','DTime','SOD','Date','Sample','OD','PR')
all.dat$Date <- as.character(all.dat$Date)
# all.dat$PR <- as.double(all.dat$PR)


ggplot() +
  # geom_smooth(data = all.dat[all.dat$Sample == 'CELL',],
  #             aes(x = PR, y = DTime, col = Date),
  #             method = 'loess',
  #             # se = F,
  #             show.legend = FALSE) +
  # geom_violin(data = all.dat[all.dat$Sample == 'CELL',],
  #             aes(x = PR, y = DTime, group = PR, fill = Date),
  #             col = 'transparent',
  #             position = 'dodge',
  #             width = 0.2, alpha = 0.5) +
  geom_boxplot(data = all.dat[all.dat$Sample == 'CELL',],
               aes(x = PR, y = DTime, group = PR, fill = Date),
               position = 'dodge',
               width = 0.05, alpha = 0.9) +
  labs(title = 'Doubling Time of Cells',
       subtitle = '',
       x = 'Spectromax Cuvette OD (at t = 0)',
       y = 'Doubling Time (mins)') +
  scale_fill_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  scale_x_continuous(breaks = seq(0,1.5,0.1), minor_breaks = seq(0,1.5,0.05)) +
  scale_y_continuous(breaks = seq(0,250,10), minor_breaks = seq(0,250,5)) +
  theme_linedraw()
ggsave(sprintf('%sfdt_dayod.png',path.out),
       height = 5, width = 5,
       dpi = 300)

##### KINETICS WITH PLC
fit.ep.918 <- lm(pr~ep+I(ep^2)+I(ep^3), data=c2p[c2p$date == 918,])
fit.kin.918 <- lm(pr~kin+I(kin^2)+I(kin^3), data=c2p[c2p$date == 918,])

fit.ep.919 <- lm(pr~ep+I(ep^2)+I(ep^3), data=c2p[c2p$date == 919,])
fit.kin.919 <- lm(pr~kin+I(kin^2)+I(kin^3), data=c2p[c2p$date == 919,])

fit.ep.920 <- lm(pr~ep+I(ep^2)+I(ep^3), data=c2p[c2p$date == 920,])
fit.kin.920 <- lm(pr~kin+I(kin^2)+I(kin^3), data=c2p[c2p$date == 920,])

summary(fit.ep.919)

ggplot() +
  geom_line(data = data.frame(ep = seq(0,.5,0.01)),
            aes(y=predict(fit.ep.918, data.frame(ep = seq(0,.5,0.01))), x=seq(0,.5,0.01),
                col='918'), lwd = 1.2) +
  geom_line(data = data.frame(ep = seq(0,.5,0.01)),
            aes(y=predict(fit.ep.919, data.frame(ep = seq(0,.5,0.01))), x=seq(0,.5,0.01),
                col='919'), lwd = 1.2) +
  geom_line(data = data.frame(ep = seq(0,.5,0.01)),
            aes(y=predict(fit.ep.920, data.frame(ep = seq(0,.5,0.01))), x=seq(0,.5,0.01),
                col='920'), lwd = 1.2) +
  geom_line(data = data.frame(ep = seq(0,.5,0.01)),
            aes(y=predict(fit.kin.918, data.frame(kin = seq(0,.5,0.01))), x=seq(0,.5,0.01),
                col='K918'), lwd = 1.2) +
  geom_line(data = data.frame(ep = seq(0,.5,0.01)),
            aes(y=predict(fit.kin.919, data.frame(kin = seq(0,.5,0.01))), x=seq(0,.5,0.01),
                col='K919'), lwd = 1.2) +
  geom_line(data = data.frame(ep = seq(0,.5,0.01)),
            aes(y=predict(fit.kin.920, data.frame(kin = seq(0,.5,0.01))), x=seq(0,.5,0.01),
                col='K920'), lwd = 1.2) +
  coord_cartesian(ylim = c(0,1),
                  xlim = c(0,0.5)) +
  theme_linedraw() +
  labs(title = 'Path Length Correction Models',
       subtitle = 'By Day and Plate OD Data Type',
       x = 'Plate OD',
       y = 'Cuvette OD') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920','K918','K919','K920'),
                       labels = c('9/18','9/19','9/20','9/18 - K','9/19 - K','9/20 - K'))
ggsave(sprintf('%splcs.png',path.out),
       height = 10, width = 10,
       dpi = 300)

all.dat <- NULL
for (date in unique(kin$Date)) {
  d <- kin[kin$Date == date,]
  d <- d[,2:dim(d)[2]]
  
  for (ii in 3:dim(d)[2]) {
    temp <- data.frame(ul125 = d[[ii]])
    d[[ii]] <- predict(fit, temp)
  }
  
  ##### INITIALIZE OUTPUT
  out = NULL
  maxgr = NULL
  dtime = NULL
  ltime = NULL
  sod = NULL
  
  ##### CLEAN DATA
  d$Time <- d$Time*60
  
  ##### PATH LENGTH CORRECTION
  # df <- plcor(d,125)
  df <- d
  
  for (i in 3:length(df)) {
    fit0 <- fit_easylinear(df$Time, abs(df[[i]]), h=2, quota = 1);
    jpeg(sprintf('%sgrowthcurves/%s_%s_GC.png',
                 path.out,
                 date,
                 colnames(d[i])),
         width=600, height=600)
    plot(fit0, log = 'y',
         main=sprintf('%s | Doubling Time = %0.2f mins',
                      colnames(d[i]),
                      log(2)/coef(fit0)[[3]]),
         ylim = c(0.1,1.2))
    dev.off()
    maxgr[i-2] = coef(fit0)[[3]]
    dtime[i-2] = log(2)/coef(fit0)[[3]]
    ltime[i-2] = coef(fit0)[[4]]
    sod[i-2] = df[[14,i]]
  }
  
  out$MaxGrowthRate = maxgr
  out$DoubleTime = dtime
  out$SatOD = sod
  out <- data.frame(matrix(unlist(out), nrow=length(maxgr)), stringsAsFactors=FALSE)
  out$Date <- date
  out$Sample = plate.des$Sample[plate.des$Date == date]
  out$OD = plate.des$OD[plate.des$Date == date]
  out$PR_OD = plate.des$OOD[plate.des$Date == date]
  
  all.dat <- rbind(all.dat, out)
}

colnames(all.dat) <- c('MaxGR','DTime','SOD','Date','Sample','OD','PR')
all.dat$Date <- as.character(all.dat$Date)
# all.dat$PR <- as.double(all.dat$PR)


ggplot() +
  # geom_smooth(data = all.dat[all.dat$Sample == 'CELL',],
  #             aes(x = PR, y = DTime, col = Date),
  #             method = 'loess',
  #             # se = F,
  #             show.legend = FALSE) +
  # geom_violin(data = all.dat[all.dat$Sample == 'CELL',],
  #             aes(x = PR, y = DTime, group = PR, fill = Date),
  #             col = 'transparent',
  #             position = 'dodge',
  #             width = 0.2, alpha = 0.5) +
  geom_boxplot(data = all.dat[all.dat$Sample == 'CELL',],
               aes(x = PR, y = DTime, group = PR, fill = Date),
               position = 'dodge',
               width = 0.05, alpha = 0.9) +
  labs(title = 'Doubling Time of Cells (w PLC)',
       subtitle = '',
       x = 'Spectromax Cuvette OD (at t = 0)',
       y = 'Doubling Time (mins)') +
  scale_fill_discrete(name = 'Date',
                      breaks = c('918','919','920'),
                      labels = c('9/18','9/19','9/20')) +
  scale_x_continuous(breaks = seq(0,1.5,0.1), minor_breaks = seq(0,1.5,0.05)) +
  scale_y_continuous(breaks = seq(0,250,10), minor_breaks = seq(0,250,5)) +
  theme_linedraw()
ggsave(sprintf('%sfdt_dayod_plc.png',path.out),
       height = 5, width = 5,
       dpi = 300)

##### BLANKS
blnks <- read.csv("rawData/blanks.csv")
blnks$Date <- as.character(blnks$Date)
blnks$Sample <- as.character(blnks$Sample)


# kin[kin$Time == 0, c(4,9,16,21,28,33,40,45,52,57,64,69,76,81,88,93)]
# kin[kin$Time == 0, c(10,15,22,27,34,39,46,51,58,63,70,75,82,87,94,99)]
# kin.blnks <- kin[kin$Time == 0, c(4,9,16,21,28,33,40,45,52,57,64,69,76,81,88,93)]
# kin.blnks <- cbind(kin.blnks, kin[kin$Time == 0, c(10,15,22,27,34,39,46,51,58,63,70,75,82,87,94,99)])
# kin.blnks <- data.frame(kin.blnks)
# kin.blnks <- cbind(kin.blnks, data.frame(date = rbind(918,919,920)))
# 
# kin.blnks <- melt(kin.blnks, id.vars = 'date')
# kin.blnks$variable[1:48] <- 'MEDIA'
# kin.blnks$variable[49:96] <- 'BSA'
# kin.blnks$date <- as.character(kin.blnks$date)

cv.blnks <- NULL
i = 1
for (d in unique(blnks$Date)) {
  for (s in unique(blnks$Sample[blnks$Date == d])) {
    cv.blnks$Date[i] <- d
    cv.blnks$Sample[i] <- s
    cv.blnks$ep_std[i] <- sd(blnks$EP[blnks$Date == d & blnks$Sample == s])
    cv.blnks$ep_avg[i] <- mean(blnks$EP[blnks$Date == d & blnks$Sample == s])
    cv.blnks$ep_cv[i] <- cv.blnks$ep_std[i]/cv.blnks$ep_avg[i] * 100
    cv.blnks$kin_std[i] <- sd(blnks$Kin[blnks$Date == d & blnks$Sample == s])
    cv.blnks$kin_avg[i] <- mean(blnks$Kin[blnks$Date == d & blnks$Sample == s])
    cv.blnks$kin_cv[i] <- cv.blnks$kin_std[i]/cv.blnks$kin_avg[i] * 100
    i = i + 1
  }
}
cv.blnks <- data.frame(cv.blnks)

ggplot(cv.blnks) +
  geom_point(aes(x = Date, y = ep_cv, col = Sample, shape = 'EP'), size = 3) +
  geom_point(aes(x = Date, y = kin_cv, col = Sample, shape = 'KIN'), size = 3) +
  labs(title = 'Variability in BLANK Measurements',
       subtitle = 'CV % = STD(replicates) / MEAN(replicates) * 100',
       x = 'Date',
       y = 'Coef. of Variance %') +
  scale_x_discrete(labels = c('9/18','9/19','9/20')) +
  scale_shape_discrete(name = 'Type of\nMeasure') +
  theme_linedraw()
ggsave(sprintf('%sf_blnk_cv.png',path.out),
       height = 5, width = 5,
       dpi = 300)

ggplot(blnks) +
  geom_point(aes(x = EP, y = Kin, col = Date), size = 3) +
  theme_linedraw() +
  labs(title = 'Relationship Between Plate Endpoint and Kinetic_0 OD',
       subtitle = 'For Media and Dye',
       x = 'Plate Endpoint OD',
       y = 'Plate Kinetic_0 OD') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  facet_grid(~Sample)
ggsave(sprintf('%sf_blnk.png',path.out),
       height = 5, width = 10,
       dpi = 300)

ep.box <- ggplot(blnks) +
  geom_boxplot(aes(x = Date, y = EP, fill = Sample)) +
  theme_linedraw() +
  labs(title = 'Plate Endpoint and Kinetic_0 OD for Media & Dye',
       subtitle = '',
       x = 'Date',
       y = 'Plate Endpoint OD')

kin.box <- ggplot(blnks) +
  geom_boxplot(aes(x = Date, y = Kin, fill = Sample)) +
  theme_linedraw() +
  labs(title = '',
       subtitle = '',
       x = 'Date',
       y = 'Plate Kinetic_0 OD')

ggarrange(ep.box, kin.box, nrow = 1,
          common.legend = T, legend = 'right')
ggsave(sprintf('%sf_blnk_box.png',path.out),
       height = 5, width = 10,
       dpi = 300)
