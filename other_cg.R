##### OTHER CELL GROWTH MODEL EXAMPLES
# They do not seem to work with our data

guessCellGrowthParams(df$Time, log2(df$A3), relative.height.at.lag = 0.1)
y = baranyi(df$Time, mu=0.0005341324, l=18900, z0=-5.798806, zmax=1.891853)

df1 <- data.frame(x = df$Time, y = log2(df$A3))
df2 <- data.frame(x = df$Time, y = y)

ggplot(df1, aes(x, y)) +
  geom_line() +
  geom_line(data = df2, color = 'red')
