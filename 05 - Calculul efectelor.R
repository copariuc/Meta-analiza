# Incarcarea pachetelor necesare
if (!require(esc)) {install.packages("esc")}
library(esc)

# Demonstrarea marimii efectului
femei <- round(rnorm(n=19, mean=22, sd=3))
barbati <- round(rnorm(n=19, mean=20, sd=3))
mean(femei); mean(barbati)
sd(femei); sd(barbati)
t.test(femei, barbati, var.equal = T)

esc_mean_sd(grp1m = mean(femei), grp1sd = sd(femei), grp1n = length(femei),
            grp2m = mean(barbati), grp2sd = sd(barbati), grp2n = length(barbati),
            es.type = "d")


