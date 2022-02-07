# Incarcarea pachetelor necesare
if (!require(meta)) install.packages("meta")

library(dplyr); library(meta); library(MAd)

# Incarcarea setulului de date si a sablonului PRISMA
load("Efecte.RData"); load("PRISMA.Rdata")

# Selectia si conversia studiilor ####
efecte$es <- as.numeric(efecte$es); efecte$sample.size <- as.numeric(efecte$sample.size)
efecte$var <- as.numeric(efecte$var); efecte$se <- as.numeric(efecte$se)
efecte$ci.lo <- as.numeric(efecte$ci.lo); efecte$ci.hi <- as.numeric(efecte$ci.hi)
efecte$year <- as.numeric(efecte$year); efecte$weight <- as.numeric(efecte$weight)
efecte$alpha.1 <- as.numeric(efecte$alpha.1); efecte$alpha.2 <- as.numeric(efecte$alpha.2)
efecte$moderator.1 <- factor(efecte$moderator.1); efecte$moderator.2 <- factor(efecte$moderator.2)

range(efecte$es)
ds.global <- efecte %>%
  dplyr::filter(!is.infinite(es)) %>%
  dplyr::filter(es < 1000)
range(ds.global$es); shapiro.test(ds.global$es)

# Agregarea studiilor si construirea noii baze de date ####
efecte.agg <- agg(data = ds.global, id = study, method="BHHR",
                 es = es, var = var,  cor = .5)
efecte.agg <- agg(data = ds.global, id = study, mod = moderator.2,
                 es = es, var = var,  cor = .5, method = "BHHR") 
n <- efecte %>%
  group_by(study) %>%
  summarise(n = round(mean(sample.size)))
efecte.agg <- efecte.agg %>%
  arrange(id) %>%
  mutate(n = n$n) %>%
  mutate(se = sqrt(var) / sqrt(n)); efecte.agg
  
# Analiza unui model cu efecte fixe ####
fixe <- metagen(data = efecte.agg, TE = es, seTE = se, studlab = id,
                    fixed = TRUE, random = FALSE); summary(fixe)

# Analiza unui mode cu efecte aleatorii ####
aleatorii <- metagen(data = efecte.agg, TE = es, seTE = se, studlab = id, 
                fixed = F, random = T, w.random = T, w.fixed = F, hakn = T,
                prediction = T, level.predict = .95,
                method.tau = "SJ", sm = "BHHR"); summary(aleatorii)

# Desenarea graficului de tip forest plot ####
forest(aleatorii, layout = "meta", xlim = c(-3, 3), sortvar = TE,
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
