library(stat1201)
library(lattice)

mean.A = 145
sd.A = 5.08
n.A = 11

mean.B = 142.3
sd.B = 1.97
n.B = 13

t2 = two_sample_t(mean.A, sd.A, n.A, mean.B, sd.B, n.B, 1)

moe = (6.32343 - -0.923427)/2
moe

pooled = pooled_t(mean.A, sd.A, n.A, mean.B, sd.B, n.B, 1)
sqrt(pooled[[2]]$S2p)

ew = (n.B * (n.A+n.B+1))/2
ew

sdw = sqrt( (n.A * n.B * (n.A + n.B + 1))/12 )
sdw

z = (136 - ew)/sdw
z
pnorm(z)

pnorm((136 - ew)/sdw)
