library(stat1201)

migraine = read.csv("Migraine.csv", as.is = F)

mean(migraine$Days0)

t.test(migraine$Days0)

t.test(Age ~ Group, migraine)

prop.table(table(migraine$Education))

addmargins(table(migraine$Education, migraine$Group))

ct = chisq.test(table(migraine$Education, migraine$Group))
ct$expected

migraine$Change = migraine$Days4 - migraine$Days0
mean(migraine$Change)

t.test(Change ~ Group, migraine, alternative = "less")