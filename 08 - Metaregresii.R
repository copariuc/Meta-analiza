# Incarcarea librariilor si a setulului de date
if (!require(metafor)) install.packages("metafor") 
library(dplyr); library(meta); library(dmetar)
library(metafor); load("Finala.RData")

# Corectii de artefact ####
ds.global <- atten(data = ds.global, g = es, xx = Rxx, yy = Ryy)
ds.global <- transform(ds.global, 
                       es.cor = ifelse(is.na(g.corrected), es, g.corrected))
ds.global$g.corrected <- NULL

# Agregarea efectelor ####
ds.global <- ds.global %>%
  group_by(study, Y) %>%
  summarise(es = round(mean(es.cor), 4),
            se = round(mean(se), 4), 
            year = round(mean(year), 4),
            cases = round(mean(sample.size), 4))

# Analiza unui model cu moderatori categoriali ####
moderare <- metagen(data = ds.global, TE = es, seTE = se, studlab = study,
                    fixed = F, random = T, method.tau = "SJ", hakn = T,
                    prediction = T, level.predict = .95,
                    subgroup = Y, subgroup.name = "Dark Factors")
# Actualizarea unui model de regresie existent
aleatorii <- update(moderare, method.tau = "REML", subgroup = NULL)

# Efectuarea metaregresiei ####
regresie <- metareg(x = aleatorii, formula = year + cases + Y,
                    method.tau = "REML", control=list(verbose = T)); summary(regresie)

# Reprezentarea grafica a meta-regresiei
bubble(regresie, xlab = "Publishing year", 
       col.line = "green", studlab = T, regline = T)
# Desenarea graficului de tip forest plot ####
pdf(file = 'Efect.pdf', height = 20, width = 30)
png(file = 'Efect.png', height = 800, width = 700)
forest(regresie); dev.off()

# Metaregresia cu model aditiv si efecte de interactiune ####
round(cor(ds.global[,c(3, 5:6)]), 3)

# Model de meta-regresie ierarhica bazat pe relatii aditive
model.1 <- rma(yi = es, sei = se, data = ds.global, 
               method = "SJ", test = "knha",
               mods = ~ cases); model.1
model.2 <- rma(yi = es, sei = se, data = ds.global, 
               method = "SJ", test = "knha",
               mods = ~ cases + year); model.2; anova(model.2, model.1)
ds.global$c.cases <- mean(ds.global$cases) - ds.global$cases
ds.global$c.year <- mean(ds.global$year) - ds.global$year
model.3 <- rma(yi = es, sei = se, data = ds.global, 
               method = "SJ", test = "knha",
               mods = ~ cases + year + c.cases * c.year); model.3;anova(model.3, model.1)
# Efectuarea testului permutarilor
permutest(model.3, iter = 200, permci = T, digits = 3)

# Analiza modelului inferential multimodel
model.4 <- multimodel.inference(TE = "es", seTE = "se", data = ds.global,
                                method = "SJ", test = "knha", 
                                eval.criterion = "AIC", interaction = T,
                                predictors = c("cases", "year")); model.4
