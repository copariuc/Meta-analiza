# Incarcarea pachetelor necesare
if (!require(dplyr)) install.packages("dplyr")
if (!require(MAd)) install.packages("MAd")
if (!require(meta)) install.packages("meta")

library(MAd); library(dplyr); library(meta)

# Selectia si conversia studiilor
efecte$es <- as.numeric(efecte$es); efecte$weight <- as.numeric(efecte$weight)
efecte$sample.size <- as.numeric(efecte$sample.size); efecte$se <- as.numeric(efecte$se)
efecte$var <- as.numeric(efecte$var); efecte$ci.lo <- as.numeric(efecte$ci.hi)
efecte$es <- as.numeric(efecte$es); efecte$weight <- as.numeric(efecte$weight)
efecte$year <- as.numeric(efecte$year); efecte$alpha.1 <- as.numeric(efecte$alpha.1)
efecte$alpha.2 <- as.numeric(efecte$alpha.2)

ds.global <- efecte %>%
  dplyr::filter(moderator.2 != "Age") %>%
  dplyr::filter(moderator.2 != "Gender")
# Agregarea efectiva a studiilor si constuirea noii baze de date
ds.global <- agg(id = study, es = es, var = var,  cor = .5, 
            method="BHHR", mod = NULL, data = ds.global); ds.global
# Calculul efectului global
g.global <- metagen(TE = es, seTE = var, data = ds.global,
                    method.tau = "SJ", hakn = T,
                    prediction = T, sm = "SMD",
                    studlab = paste(study))
g.forest <- meta::forest(g.global, sortvar=TE,  xlim = c(-2,2),
                 rightlabs = c("g","95% CI","weight"), leftlabs = c("Author", "d","Standard Error"),
                 lab.e = "Intervention", pooled.totals = FALSE,
                 text.random = "Overall effect", print.tau2 = TRUE, 
                 col.diamond = "blue", col.diamond.lines = "black", 
                 col.predict = "black", print.I2.ci = TRUE, digits.sd = 2)
