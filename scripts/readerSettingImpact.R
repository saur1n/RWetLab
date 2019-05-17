##### READER SETTING AND PLC
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 05/17/2019
# Experiment done after the suspicion that the END POINT and KINETIC readouts from the platereader
# is assigning different ODs to the same media - causing the decripancies between PLC relationship
# and final experimental usage

##### SINGLE POINT SHAKE MODEL IS THE BEST!

##### INITIALIZATION
library(readxl)
library(ggplot2)
load('plc_models/plc_fy125sps.rda')

##### LOAD DATA & PLC
d <- read_excel("rawData/svsk.xlsx",col_types = "numeric")
df <- d
for (i in 1:2) {
  temp <- data.frame(ul125 = df[[i]])
  df[[i]] <- predict(fit, temp)
}
df$EPp <- d$EP
df$KINp <- d$KIN

##### PLOTING RELATIONSHIPS
ggplot(df) +
  geom_abline(linetype = 2, col = 'red', lwd = 1.2) +
  geom_point(aes(x = KIN, y = EP, col = 'YES'), size = 3) +
  geom_point(aes(x = KINp, y = EPp, col = 'NO'), size = 3) +
  labs(title = "Readout Setting Relationship",
       subtitle = "PLC Model = Single Point + Shake",
       x = "Kinetic",
       y = "End Point") +
  scale_x_continuous(breaks = seq(0,2,0.2),
                     minor_breaks = seq(0,2,0.1)) +
  scale_y_continuous(breaks = seq(0,2,0.2),
                     minor_breaks = seq(0,2,0.1)) +
  scale_color_manual(name = 'PLC',
                     breaks = c("YES", "NO"),
                     values = alpha(c("YES" = "darkred",
                                      "NO" = "darkgreen"),0.7)) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size=10),
        axis.title.x = element_text(size=15),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=15),
        legend.position = c(0.8,0.2),
        legend.background = element_rect(fill="gray90",
                                         size=.5,
                                         linetype="dotted"),
        legend.text = element_text(size=10),
        plot.title = element_text(size=20,hjust = 0.5),
        plot.subtitle = element_text(size=13,hjust = 0.5)) +
  coord_cartesian(xlim = c(0,1.3),
                  ylim = c(0,1.3))
ggsave('outData/plots/kinVSep_sps.png',
       width = 10,height = 10)

ggplot(df) +
  geom_abline(linetype = 2, col = 'red', lwd = 1.2) +
  geom_point(aes(x = C, y = KIN, col = 'Kinetic'),
             shape = 19, size = 3) +
  geom_point(aes(x = C, y = EP, col = 'EndPoint'),
             shape = 15, size = 3) +
  labs(title = "Effect of Plate Reader Setting",
       subtitle = "PLC Model = Single Point + Shake",
       x = "Cuvette OD",
       y = "Path Length Corrected Plate OD") +
  scale_x_continuous(breaks = seq(0,2,0.2),
                     minor_breaks = seq(0,2,0.1)) +
  scale_y_continuous(breaks = seq(0,2,0.2),
                     minor_breaks = seq(0,2,0.1)) +
  scale_color_manual(name = 'Readout Setting',
                     values = alpha(c("Kinetic" = "darkgreen",
                                "EndPoint" = "darkblue"),0.7)) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size=10),
        axis.title.x = element_text(size=15),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=15),
        legend.position = c(0.8,0.2),
        legend.background = element_rect(fill="gray90",
                                         size=.5,
                                         linetype="dotted"),
        legend.text = element_text(size=10),
        legend.title =  element_text(size=15),
        plot.title = element_text(size=20,hjust = 0.5),
        plot.subtitle = element_text(size=13,hjust = 0.5)) +
  coord_cartesian(xlim = c(0,1.3),
                  ylim = c(0,1.3))
ggsave('outData/plots/cuvetteVSall_sps.png',
       width = 10,height = 10)









