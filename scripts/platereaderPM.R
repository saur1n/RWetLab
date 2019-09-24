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
c2p <- c2p[1:48,1:13]

#####
ggplot(data = c2p, aes(x = pr, y = sp, col = date)) +
  geom_point(size = 3) +
  # geom_smooth(method = 'lm') +
  theme_linedraw() +
  facet_wrap(~sample, drop = T)

ggplot() +
  geom_point(data = c2p, aes(x = pr, y = endpoint_1, col = 'rep1', shape = date), size = 3) +
  geom_point(data = c2p, aes(x = pr, y = endpoint_2, col = 'rep2', shape = date), size = 3) +
  geom_point(data = c2p, aes(x = pr, y = endpoint_3, col = 'rep3', shape = date), size = 3) +
  geom_point(data = c2p, aes(x = pr, y = endpoint_4, col = 'rep4', shape = date), size = 3) +
  # stat_cor(method = "pearson") +
  facet_wrap(~sample, drop = T) +
  theme_linedraw()

ggplot() +
  geom_abline() +
  geom_point(data = c2p,
             aes(x = endpoint_1, y = endpoint_2, shape = 'rep1-2', col = date),
             size = 3) +
  geom_point(data = c2p,
             aes(x = endpoint_1, y = endpoint_3, shape = 'rep1-3', col = date),
             size = 3) +
  geom_point(data = c2p,
             aes(x = endpoint_1, y = endpoint_4, shape = 'rep1-4', col = date),
             size = 3) +
  geom_point(data = c2p,
             aes(x = endpoint_2, y = endpoint_3, shape = 'rep2-3', col = date),
             size = 3) +
  geom_point(data = c2p,
             aes(x = endpoint_2, y = endpoint_4, shape = 'rep2-4', col = date),
             size = 3) +
  geom_point(data = c2p,
             aes(x = endpoint_3, y = endpoint_4, shape = 'rep3-4', col = date),
             size = 3) +
  labs(title = 'Comparing Replicates on Plate',
       x = 'Plate OD',
       y = 'Plate OD') +
  scale_shape_discrete(name = 'REP.') +
  scale_color_discrete(name = 'Date',
                       breaks = c('918','919','920'),
                       labels = c('9/18','9/19','9/20')) +
  facet_wrap(~sample, drop = T) +
  theme_linedraw() +
  coord_cartesian(xlim = c(0, 0.52),
                  ylim = c(0, 0.52))
ggsave(sprintf('%splate_rep_comp.png',path.out),
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
  coord_cartesian(xlim = c(0, 0.52),
                  ylim = c(0, 0.52))
ggsave(sprintf('%splate_meth_comp.png',path.out),
       height = 10, width = 20,
       dpi = 300)


sum(abs(c2p$endpoint_1[c2p$sample == 'CELL'] - c2p$endpoint_2[c2p$sample == 'CELL']))/length(c2p$endpoint_2[c2p$sample == 'CELL'])
sum(abs(c2p$endpoint_1[c2p$sample == 'BSA'] - c2p$endpoint_2[c2p$sample == 'BSA']))/length(c2p$endpoint_2[c2p$sample == 'BSA'])

#####
?melt
c2p_2 <- melt(c2p, id.vars = c('date','sample','od'))
ggplot(c2p_2) +
  geom_point(aes(x = od, y = value, shape = date, col = variable)) +
  facet_grid(~sample)
