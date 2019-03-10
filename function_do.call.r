
data <- list()

N <- 10

for (n in 1:N) {
    data[[n]] = data.frame(index = n, char = sample(letters, 1), z = runif(1))
}

data[[5]] 

str(data)

rbind(data)

head(data)

head(do.call(rbind, data))

