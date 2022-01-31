library(stat1201)
library(lattice)

# Let X be the number of people infected by a diseased person
ex = (0 * 0.25) + (1 * 0.2) + (2 * 0.3) + (3 * 0.25)
ex

varx = (0.25 * (0 - ex) ^ 2) + (0.2 * (1 - ex) ^ 2) + (0.3 * (2 - ex) ^ 2) + (0.25 * (3 - ex) ^ 2)
varx
sdx = sqrt(varx)
sdx

mu = 60 * ex
mu

sigma = sqrt(60 * varx)
sigma

z = (104 - mu) / sigma
1 - pnorm(z)
