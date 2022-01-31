library(stat1201)
library(lattice)

river = read.csv("River.csv", as.is=F)
median(river$Elevation)

hist(river$Elevation)
bwplot(river$Elevation)

hist(river$Cadmium)

aggregate(LogCadmium ~ LandUse, data = river, "mean")

fit = lm(LogCadmium ~ LandUse, data = river)
t = anova(fit)

SSTotal = 4.688 + 16.896

LR2 = (4.688/SSTotal)
LR2

river$Flooding = as.factor(river$Flooding)
fit = aov(LogCadmium ~ Flooding, data = river)
summary(fit)

SSTotal = 13.099 + 8.485

FR2 = (13.099/SSTotal)
FR2

fit = lm(LogCadmium ~ Distance, data = river)
fit
summary(fit)

qt(0.975, 68) * 0.0002054  

predict(fit, newdata=data.frame(Distance = 270), interval="prediction")
10^0.2557189


fit = lm(LogCadmium ~ Distance + Elevation, data = river)
summary(fit)

predict(fit, newdata=data.frame(Distance = 270, Elevation=6.2), interval="prediction")
0.462 - 0.7510874 

river$Fitted = 10^predict(fit)
xyplot(Fitted ~ Distance, data = river)
xyplot(Fitted ~ Elevation, data = river)