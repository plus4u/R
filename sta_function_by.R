
# by ()

# Create a data frame with petal length and petal width for 3 species

dat <- data.frame(species=c(rep(c(1,2,3), each=5)), petal.length=c(rnorm(5, 4.5, 1), rnorm(5, 4.5, 1), rnorm(5, 5.5, 1)), petal.width=c(rnorm(5, 2.5, 1), rnorm(5, 2.5, 1), rnorm(5, 4, 1)))

dat$species <- factor(dat$species) # make species a factor


by(dat, dat$species, function(x){
    # caculate the mean petal length for each species
    mean.pl <- mean(x$petal.length)
})