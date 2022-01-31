library(stat1201)
library(lattice)

tomato = read.csv("Tomatoes.csv")
View(tomato)

prop.table(table(tomato$Insects))

ct = chisq.test(table(tomato$Insects, tomato$Irrigation))
ct$expected
ct

t.test(Mass ~ Insects, data=tomato)

table(tomato$Biochar, tomato$Irrigation)

aggregate(Mass ~ Biochar, tomato, mean)
1362.056 - 1181.667

summary(aov(VitC ~ Biochar*Irrigation, data=tomato))

SSG = 404.0 + 87.9 + 220.6
SSG/(SSG + 436.0)

aggregate(VitC ~ Biochar*Irrigation, tomato, mean)
