# Incarcarea pachetelor necesare
if (!require(meta)) install.packages("meta")
library(dplyr); library(meta); library(MAd)
# Incarcarea setulului de date
load("Finala.RData")

# Agregarea studiilor si construirea noii baze de date ####
efecte.agg <- agg(data = ds.global, id = study, method = "BHHR",
                 es = es, var = var,  cor = .5)
n <- ds.global %>%
  group_by(study) %>%
  summarise(n = round(mean(sample.size)))
efecte.agg <- efecte.agg %>%
  arrange(id) %>%
  mutate(n = n$n) %>%
  mutate(se = sqrt(var) / sqrt(n)); efecte.agg; rm(n)
  
# Analiza unui model cu efecte fixe ####
fixe <- metagen(data = efecte.agg, TE = es, seTE = se, studlab = id,
                    fixed = TRUE, random = FALSE); summary(fixe)

# Analiza unui mode cu efecte aleatorii ####
aleatorii <- metagen(data = efecte.agg, TE = es, seTE = se, studlab = id, 
                fixed = F, random = T, w.random = T, w.fixed = F, hakn = T,
                prediction = T, level.predict = .95,
                method.tau = "SJ", sm = "BHHR"); summary(aleatorii)

# Desenarea graficului de tip forest plot ####
forest(aleatorii, layout = "meta", xlim = c(-1.5, 1.5), sortvar = TE,
       rightlabs = c("g","95% CI","weight"),
       leftlabs = c("Author(s) and Year", "g","Standard Error"),
       text.fixed = "Common effect size",
       text.random = "Random effect size",
       text.predict = "Prediction interval",
       col.fixed = "red", col.random = "green", 
       col.diamond = "dark orange", col.predict = "yellow",
       print.tau2 = T, print.tau2.ci = T,
       print.tau = F, print.tau.ci = F,
       print.I2 = T, print.I2.ci = F,
       print.Q = F, print.pval.Q = F) 
