# Incarcarea librariilor si a setulului de date
devtools::install_github("MathiasHarrer/dmetar")
library(dplyr); library(meta); library(MAd)
library(dmetar); library(metafor); load("Finala.RData")

# Corectii de artefact ####
ds.global <- atten(data = ds.global, g = es, xx = Rxx, yy = Ryy)
options(scipen = 999); ds.global$g.corrected
ds.global <- transform(ds.global, 
                       es.cor = ifelse(is.na(g.corrected), es, g.corrected))
ds.global$g.corrected <- NULL

# Agregarea efectelor ####
ds.global <- ds.global %>%
  group_by(study, Y) %>%
  summarise(es = round(mean(es.cor), 4),
            se = round(mean(se), 4), 
            year = round(mean(year), 4),
            n = round(mean(sample.size), 4))

# Analiza unui model cu moderatori categoriali ####
moderare <- metagen(data = ds.global, TE = es, seTE = se, studlab = study,
                    fixed = F, random = T, method.tau = "SJ", hakn = T,
                    prediction = T, level.predict = .95,
                    subgroup = Y, subgroup.name = "Dark Factors")
summary(moderare)
subgroup.analysis.mixed.effects(x = moderare, subgroups = moderare$subgroup)

# Desenarea graficului de tip forest plot ####
pdf(file = 'Efect.pdf', height = 20, width = 10)
png(file = 'Efect.png', height = 1300, width = 700)
forest(moderare, layout = "meta", xlim = c(-2, 2), sortvar = TE,
       rightlabs = c("g","95% CI","weight"),
       leftlabs = c("Author(s) and Year", "g","Standard Error"),
       text.fixed = "Common effect size",
       text.random = "Random effect size",
       text.predict = "Prediction interval",
       subgroup.name = "Darl Triad factor",
       col.fixed = "red", col.random = "green", 
       col.diamond = "dark orange", col.predict = "yellow",
       print.tau2 = T, print.tau2.ci = T,
       print.tau = F, print.tau.ci = F,
       print.I2 = T, print.I2.ci = F,
       print.Q = F, print.pval.Q = F); dev.off()

# Analiza studiilor cu efecte extreme ####
no.out <- find.outliers(moderare); summary(no.out)
pdf(file = 'Efect.pdf', height = 20, width = 10)
forest(no.out, layout = "meta", xlim = c(-1.5, 1.5),
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
       print.Q = F, print.pval.Q = F); dev.off()

# Analiza studiilor influentiale ####
influence <- InfluenceAnalysis(moderare, random = T)
options(ggrepel.max.overlaps = Inf)
pdf(file = 'Efect.pdf', height = 5, width = 10)
plot(influence, "baujat"); dev.off()
pdf(file = 'Efect.pdf', height = 7, width = 12)
plot(influence, "influence"); dev.off()
pdf(file = 'Efect.pdf', height = 9, width = 9)
plot(influence, "es"); dev.off()
pdf(file = 'Efect.pdf', height = 9, width = 9)
plot(influence, "I2"); dev.off()

# Analiza GOSH ####
date.gosh <- rma(yi = aleatorii$TE, sei = aleatorii$seTE,
                 method = aleatorii$method.tau, test = "knha")
parallel::detectCores() # Detectarea nucleelor procesorului
rez.gosh <- gosh(date.gosh, parallel = "snow", 
                 ncpus = 7, progbar = T); save(rez.gosh, file = "GOSH.Rdata")
plot(rez.gosh, alpha = .05)
diag.gosh <- gosh.diagnostics(rez.gosh, km = T, verbose = T)
diag.gosh; plot(diag.gosh)
# Actualizarea meta-analizei inițiale după excluderea unor studii
actualizare <- update.meta(aleatorii, exclude = c(2, 19)); actualizare
actualizare <- update.meta(moderare, 
                           exclude = c(5, 5, 62, 63)); actualizare
