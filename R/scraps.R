library(tidyverse)
df <- data.frame(x = 0:100,
y = 0:100)

plot(formula = x*x ~ y, data = df)
curve(x^2, from=0, to=100, , xlab="x", ylab="y")
curve(x*1, from=0, to=100, , xlab="x", ylab="y")

b <- 10:0
a <- 0:10

w1 <- 100
w2 <- 75
lst <- list()
df <- data.frame()
for (i in 1:length(a)) {
  twd <- sum(w1*a[i] + w2*b[i])
  df[i,1] <- twd
}

df <- df[1]
df <- rename(df, total_weighted_distance = V1)
df <- df %>% mutate(Xa = 0:10)
plot(x = df$Xa, y = df$total_weighted_distance)

ggplot(df, aes(x = Xa, y = total_weighted_distance)) +
  geom_point(size = 3) +
  geom_line()





