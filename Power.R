# Design and Analysis of Experiments with R textbook package
library(daewr)
# One-way ANOVA analysis
rmin <- 2 # smallest number of replicates considered
rmax <- 6 # largest number of replicates considered
alpha <- rep(0.05, rmax - rmin +1) # level of future test
sigma <- 2 # estimate of SD
nlev <- 3 # number of levels of treatment
nreps <- rmin:rmax # create vector of possible number of reps
Delta <- 1 # Minimum detectable difference
power <- Fpower1(alpha,nlev,nreps,Delta,sigma)
power

# Two-way ANOVA analysis
rmin <- 2 #smallest number of replicates considered
rmax <- 6 # largest number of replicates considered
alpha <- rep(0.05, rmax - rmin +1)
sigma <- 2
Delta <- 1
nlev <- c(3,20) # c(levels of a, levels of b)
nreps <- c(rmin:rmax)
result <- Fpower2(alpha, nlev, nreps, Delta, sigma)
result