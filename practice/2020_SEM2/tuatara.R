library(stat1201)

mean.A = 617.7
sd.A = 38.67
n.A = 9

mean.B = 585.8
sd.B = 23.39
n.B = 14

mean.A - mean.B

t2 = two_sample_t(mean.A, sd.A, n.A, mean.B, sd.B, n.B, 2)

MOE = qt(0.975, 8) * t2$se
MOE

outliers(597.8, 621.6)

EW = (n.A * (n.A + n.B + 1)) / 2
EW

sdW = sqrt(((n.A * n.B) * (n.A + n.B + 1)) / 12)
sdW

Z = (145 - EW) / sdW
Z
2 * (1 - pnorm(Z))