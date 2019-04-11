##### GROWTHRATE QC

fit <- fit_easylinear(df$Time, df[[7]], h=14, quota = 1);

plot(fit, log = 'y',
     main=sprintf('%s\n%s\nDoubling Time = %0.2f mins',
                  'Expt10',
                  colnames(df[7]),
                  log(2)/coef(fit)[[3]]),
     ylim = c(0.003,6))

gev = c(154.39,155.79)
ybr = c(135.93,136.87)
ylr = c(132.72,128.36)

t.test(gev,ybr,alternative = 'g')$p.value
t.test(gev,ylr,alternative = 'g')$p.value
