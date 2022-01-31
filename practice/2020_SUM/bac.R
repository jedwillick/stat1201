library(stat1201)

alcohol = read.csv("Alcohol.csv")
aggregate(BAC ~ Sex, alcohol, mean)

aggregate(BAC ~ Group,alcohol, mean)
0.08400000 - 0.05257143

t = summary(aov(BAC ~ Group, alcohol))

SST = 0.005067 + 0.009123 
0.005067/SST

TukeyHSD(aov(BAC ~ Group, alcohol))

cor.test(alcohol$BAC, alcohol$Mass, alternative="less")

fit = lm(BAC ~ Mass + Drinks, data=alcohol)
fit
summary(fit)

predict(fit, newdata=data.frame(Mass = 70, Drinks =5.2), interval="prediction")

0.0004782 * t_crit(0.95, 18)

plot(fit)
