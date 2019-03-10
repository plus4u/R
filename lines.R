## abline, lines compare

x <- seq(from = 0, to = 2*pi, by = 0.1)
y <- sin(x)

plot(x, y, type = "n")
lines(x, y, lty = 3)

# abline  

plot(x, y)
abline(v = 3, lty = 2)  # vertical
abline(h = 0, lty = 3)  # horizontal

abline(a = -1, b = 1, col = "red")  # y = -1 + x