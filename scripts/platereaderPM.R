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
c2p <- c2p[1:48,1:13]


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
  labs(title = 'Variability in Reading',
       subtitle = 'as a function of well position',
       x = 'Plate OD',
       y = 'Plate OD') +
  scale_shape_discrete(name = 'REP.') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  theme_linedraw() +
  facet_grid(~sample)
ggsave(sprintf('%sf_well.png',path.out),
       height = 10, width = 20,
       dpi = 300)

mean(abs(c2p$endpoint_1[c2p$sample == 'BSA'] - c2p$endpoint_2[c2p$sample == 'BSA'])/
       (rowSums(cbind(c2p$endpoint_1[c2p$sample == 'BSA'], c2p$endpoint_2[c2p$sample == 'BSA']))/2) * 100)
mean(abs(c2p$endpoint_1[c2p$sample == 'BSA'] - c2p$endpoint_3[c2p$sample == 'BSA'])/
       (rowSums(cbind(c2p$endpoint_1[c2p$sample == 'BSA'], c2p$endpoint_3[c2p$sample == 'BSA']))/2) * 100)
mean(abs(c2p$endpoint_1[c2p$sample == 'BSA'] - c2p$endpoint_4[c2p$sample == 'BSA'])/
       (rowSums(cbind(c2p$endpoint_1[c2p$sample == 'BSA'], c2p$endpoint_4[c2p$sample == 'BSA']))/2) * 100)
mean(abs(c2p$endpoint_2[c2p$sample == 'BSA'] - c2p$endpoint_3[c2p$sample == 'BSA'])/
       (rowSums(cbind(c2p$endpoint_2[c2p$sample == 'BSA'], c2p$endpoint_3[c2p$sample == 'BSA']))/2) * 100)
mean(abs(c2p$endpoint_2[c2p$sample == 'BSA'] - c2p$endpoint_4[c2p$sample == 'BSA'])/
       (rowSums(cbind(c2p$endpoint_2[c2p$sample == 'BSA'], c2p$endpoint_4[c2p$sample == 'BSA']))/2) * 100)
mean(abs(c2p$endpoint_3[c2p$sample == 'BSA'] - c2p$endpoint_4[c2p$sample == 'BSA'])/
       (rowSums(cbind(c2p$endpoint_3[c2p$sample == 'BSA'], c2p$endpoint_4[c2p$sample == 'BSA']))/2) * 100)


mean(abs(c2p$endpoint_1[c2p$sample == 'CELL'] - c2p$endpoint_2[c2p$sample == 'CELL'])/
       (rowSums(cbind(c2p$endpoint_1[c2p$sample == 'CELL'], c2p$endpoint_2[c2p$sample == 'CELL']))/2) * 100)
mean(abs(c2p$endpoint_1[c2p$sample == 'CELL'] - c2p$endpoint_3[c2p$sample == 'CELL'])/
       (rowSums(cbind(c2p$endpoint_1[c2p$sample == 'CELL'], c2p$endpoint_3[c2p$sample == 'CELL']))/2) * 100)
mean(abs(c2p$endpoint_1[c2p$sample == 'CELL'] - c2p$endpoint_4[c2p$sample == 'CELL'])/
       (rowSums(cbind(c2p$endpoint_1[c2p$sample == 'CELL'], c2p$endpoint_4[c2p$sample == 'CELL']))/2) * 100)
mean(abs(c2p$endpoint_2[c2p$sample == 'CELL'] - c2p$endpoint_3[c2p$sample == 'CELL'])/
       (rowSums(cbind(c2p$endpoint_2[c2p$sample == 'CELL'], c2p$endpoint_3[c2p$sample == 'CELL']))/2) * 100)
mean(abs(c2p$endpoint_2[c2p$sample == 'CELL'] - c2p$endpoint_4[c2p$sample == 'CELL'])/
       (rowSums(cbind(c2p$endpoint_2[c2p$sample == 'CELL'], c2p$endpoint_4[c2p$sample == 'CELL']))/2) * 100)
mean(abs(c2p$endpoint_3[c2p$sample == 'CELL'] - c2p$endpoint_4[c2p$sample == 'CELL'])/
       (rowSums(cbind(c2p$endpoint_3[c2p$sample == 'CELL'], c2p$endpoint_4[c2p$sample == 'CELL']))/2) * 100)

##### f(od) = variability
c2p$cv_ep <- NULL
c2p$cv_kin <- NULL
for (i in seq(1,dim(c2p)[1])) {
  c2p$cv_ep[i] <- sd(c2p[i,6:9])
  c2p$cv_kin[i] <- sd(c2p[i,10:13])
}
c2p$cv_ep <- c2p$cv_ep/rowMeans(c2p[6:9]) * 100
c2p$avg_ep <- rowMeans(c2p[6:9])
c2p$cv_kin <- c2p$cv_kin/rowMeans(c2p[10:13]) * 100
c2p$avg_kin <- rowMeans(c2p[10:13])

# od_data <- melt(c2p, id = c('sample','od','date'))

ggplot(c2p) +
  geom_point(aes(x = avg_ep, y = cv_ep, col = date), size = 3) +
  theme_linedraw() +
  labs(title = 'Variability in Reading',
       subtitle = 'as a function of OD',
       x = 'Mean Endpoint Plate OD',
       y = 'Coef of Variance (%) of Endpoint Plate OD') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  facet_grid(~sample)
ggsave(sprintf('%sf_od.png',path.out),
       height = 10, width = 20,
       dpi = 300)


c2p$avg_pr <- NULL
c2p$rng_pr <- NULL
c2p$cv_pr <- NULL
c2p$avg_sp <- NULL
c2p$rng_sp <- NULL
c2p$cv_sp <- NULL
for (s in unique(c2p$sample)) {
  for (o in unique(c2p$od[c2p$sample == s])) {
    c2p$avg_pr[c2p$od == o & c2p$sample == s] <- mean(c2p$pr[c2p$od == o & c2p$sample == s])
    c2p$avg_sp[c2p$od == o & c2p$sample == s] <- mean(c2p$sp[c2p$od == o & c2p$sample == s])
    c2p$cv_pr[c2p$od == o & c2p$sample == s] <- sd(c2p$pr[c2p$od == o & c2p$sample == s])/mean(c2p$pr[c2p$od == o & c2p$sample == s]) * 100
    c2p$cv_sp[c2p$od == o & c2p$sample == s] <- sd(c2p$sp[c2p$od == o & c2p$sample == s])/mean(c2p$sp[c2p$od == o & c2p$sample == s]) * 100
    c2p$rng_pr[c2p$od == o & c2p$sample == s] <- max(c2p$pr[c2p$od == o & c2p$sample == s]) - min(c2p$pr[c2p$od == o & c2p$sample == s])
    c2p$rng_sp[c2p$od == o & c2p$sample == s] <- max(c2p$sp[c2p$od == o & c2p$sample == s]) - min(c2p$sp[c2p$od == o & c2p$sample == s])
  }
}
  
# ggplot(c2p) +
#   geom_point(aes(x = avg_pr, y = cv_pr), size = 3) +
#   labs(title = 'Variability in Reading',
#        subtitle = 'as a function of OD',
#        x = 'Cuvette OD',
#        y = 'Coef of Variance (%) of Cuvette OD between Days') +
#   theme_linedraw() +
#   facet_grid(~sample)
# ggsave(sprintf('%sf_od2.png',path.out),
#        height = 10, width = 20,
#        dpi = 300)

##### f(day) = variability
ggplot() +
  geom_abline() +
  geom_point(data = c2p[c2p$date == '918',],
             aes(x = endpoint_1, y = c2p$endpoint_1[c2p$date == '919'], col = '18-19', shape = 'rep1'),
             size = 3) +
  geom_point(data = c2p[c2p$date == '918',],
             aes(x = endpoint_1, y = c2p$endpoint_1[c2p$date == '920'], col = '18-20', shape = 'rep1'),
             size = 3) +
  geom_point(data = c2p[c2p$date == '919',],
             aes(x = endpoint_1, y = c2p$endpoint_1[c2p$date == '920'], col = '19-20', shape = 'rep1'),
             size = 3) +
  geom_point(data = c2p[c2p$date == '918',],
             aes(x = endpoint_2, y = c2p$endpoint_2[c2p$date == '919'], col = '18-19', shape = 'rep2'),
             size = 3) +
  geom_point(data = c2p[c2p$date == '918',],
             aes(x = endpoint_2, y = c2p$endpoint_2[c2p$date == '920'], col = '18-20', shape = 'rep2'),
             size = 3) +
  geom_point(data = c2p[c2p$date == '919',],
             aes(x = endpoint_2, y = c2p$endpoint_2[c2p$date == '920'], col = '19-20', shape = 'rep2'),
             size = 3) +
  geom_point(data = c2p[c2p$date == '918',],
             aes(x = endpoint_3, y = c2p$endpoint_3[c2p$date == '919'], col = '18-19', shape = 'rep3'),
             size = 3) +
  geom_point(data = c2p[c2p$date == '918',],
             aes(x = endpoint_3, y = c2p$endpoint_3[c2p$date == '920'], col = '18-20', shape = 'rep3'),
             size = 3) +
  geom_point(data = c2p[c2p$date == '919',],
             aes(x = endpoint_3, y = c2p$endpoint_3[c2p$date == '920'], col = '19-20', shape = 'rep3'),
             size = 3) +
  geom_point(data = c2p[c2p$date == '918',],
             aes(x = endpoint_4, y = c2p$endpoint_4[c2p$date == '919'], col = '18-19', shape = 'rep4'),
             size = 3) +
  geom_point(data = c2p[c2p$date == '918',],
             aes(x = endpoint_4, y = c2p$endpoint_4[c2p$date == '920'], col = '18-20', shape = 'rep4'),
             size = 3) +
  geom_point(data = c2p[c2p$date == '919',],
             aes(x = endpoint_4, y = c2p$endpoint_4[c2p$date == '920'], col = '19-20', shape = 'rep4'),
             size = 3) +
  theme_linedraw() +
  labs(title = 'Variability in Reading',
       subtitle = 'as a function of Day',
       x = 'Plate OD',
       y = 'Plate OD') +
  scale_color_discrete(name = 'Dates') +
  scale_shape_discrete(name = 'REP.') +
  coord_cartesian(xlim = c(0.05,0.55),
                  ylim = c(0.05,0.55)) +
  facet_grid(~sample)
ggsave(sprintf('%sf_day.png',path.out),
       height = 10, width = 20,
       dpi = 300)

##### f(method) = variability
ggplot(c2p) +
  geom_point(aes(x = pr, y = endpoint_1, col = date, shape = 'rep1'),
             size = 3) +
  geom_point(aes(x = pr, y = endpoint_2, col = date, shape = 'rep2'),
             size = 3) +
  geom_point(aes(x = pr, y = endpoint_3, col = date, shape = 'rep3'),
             size = 3) +
  geom_point(aes(x = pr, y = endpoint_4, col = date, shape = 'rep4'),
             size = 3) +
  theme_linedraw() +
  labs(title = 'Variability in Reading',
       subtitle = 'as a function of Method',
       x = 'Cuvette OD',
       y = 'Plate OD') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  scale_shape_discrete(name = 'REP.') +
  coord_cartesian(xlim = c(0,1.2),
                  ylim = c(0.05,0.55)) +
  facet_grid(~sample)
ggsave(sprintf('%sf_meth1.png',path.out),
       height = 10, width = 20,
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
             aes(x = endpoint_4, y = kinetic_4, shape = 'rep4', col = date),
             size = 3) +
  labs(title = 'Comparing Methods on Plate',
       x = 'Plate Endpoint OD',
       y = 'Plate Kinetic_0 OD') +
  scale_shape_discrete(name = 'REP.') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  facet_wrap(~sample, drop = T) +
  theme_linedraw() +
  coord_cartesian(xlim = c(0.05, 0.55),
                  ylim = c(0.05, 0.55))
ggsave(sprintf('%sf_meth2.png',path.out),
       height = 10, width = 20,
       dpi = 300)

##### f(machine) = variability
ggplot(c2p) +
  geom_point(aes(x = pr, y = sp, col = date),
             size = 3) +
  theme_linedraw() +
  labs(title = 'Variability in Reading',
       subtitle = 'as a function of Machine',
       x = 'SpectroMax',
       y = 'Spectrophotometer') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  coord_cartesian(xlim = c(0,1.2),
                  ylim = c(0,1)) +
  facet_grid(~sample)
ggsave(sprintf('%sf_mach.png',path.out),
       height = 10, width = 20,
       dpi = 300)

##### LINEAR MODELS
library(lme4)

fit_lm <- lm(cv_ep ~ sample + pr + date, c2p)
summary(fit_lm)

fit.sample <- lmer(cv_ep ~ sample + pr + (1|date),
                   data = c2p)
summary(fit.sample)
fit.null <- lmer(cv_ep ~ pr + (1|date),
                 data = c2p)
summary(fit.null)

anova(fit.null, fit.sample)

fit12 <- lm(endpoint_1 ~ endpoint_2, data = c2p[c2p$sample == 'BSA',])
summary(fit12)

ggplot(data = c2p, aes(x = pr, y = sp, col = date)) +
  geom_point(size = 3) +
  # geom_smooth(method = 'lm') +
  theme_linedraw() +
  facet_wrap(~sample, drop = T)



sum(abs(c2p$endpoint_1[c2p$sample == 'CELL'] - c2p$endpoint_2[c2p$sample == 'CELL']))/length(c2p$endpoint_2[c2p$sample == 'CELL'])
sum(abs(c2p$endpoint_1[c2p$sample == 'BSA'] - c2p$endpoint_2[c2p$sample == 'BSA']))/length(c2p$endpoint_2[c2p$sample == 'BSA'])

##### KINETIC

