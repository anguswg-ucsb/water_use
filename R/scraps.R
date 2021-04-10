
df <- data.frame(x = 0:100,
y = 0:100)

plot(formula = x*x ~ y, data = df)
curve(x^2, from=0, to=100, , xlab="x", ylab="y")
curve(x*1, from=0, to=100, , xlab="x", ylab="y")
