# Incarcarea pachetelor necesare
if (!require(esc)) install.packages("esc")
if (!require(MAd)) install.packages("MAd")
library(esc); library(MAd)

# Demonstrarea marimii efectului ####
femei <- round(rnorm(n=19, mean=22, sd=3))
barbati <- round(rnorm(n=19, mean=20, sd=3))
mean(femei); mean(barbati)
sd(femei); sd(barbati)
t.test(femei, barbati, var.equal = T)

# Extragerea scorului z sau t situat la limita superioara a distributiei
qnorm(p = .0065, lower.tail = F)
qt(p = .0065, df=36, lower.tail = F)
qchisq(p=0.043, df=1, n=200, lower.tail = F)

# Codarea contrastului polinomial
contr.poly(3, contrasts = T)
contr.poly(4, contrasts = T) * 10
contr.poly(5, contrasts = T) * 10
contr.poly(6, contrasts = T) * 10

# Codarea efectelor folosindu-se pachetul "esc" ####
load("Centralizator.Rdata"); load("PRISMA.Rdata"); eliminate <- 0
## 1. Studiul ref_004 - CODARE ####
## H. M. Baughman, S. Dearing, E. gammarco, P. A. Vernon (2012) - 
## Relationships between bullying behaviours and the Dark Triad: A study with adults
year <- tabel.surse[which(tabel.surse$label == "ref_004"),]$year
r = .35; n = 657; conf.lvl = .95
g.1.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.1.se <- 1/sqrt(n - 3)
g.1.ci <- convert_r2z(r) + c(-1, 1) * 
  g.1.se * qnorm((1 + conf.lvl) / 2)
g.1.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.1.ci)), totaln = n)
g.1.study <- "Baughman et al. (2012)"
g.1.m.1 <- "Bullying Total"; g.1.m.2 <- "Machiavellianism"; g.1.rxx <- .89; g.1.ryy <- .73

r = .22; n = 657; conf.lvl = .95
g.2.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.2.se <- 1/sqrt(n - 3)
g.2.ci <-convert_r2z(r) + c(-1, 1) * 
  g.2.se * qnorm((1 + conf.lvl) / 2)
g.2.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.2.ci)), totaln = n)
g.2.study <- "Baughman et al. (2012)"
g.2.m.1 <- "Bullying Total"; g.2.m.2 <- "Narcissism"; g.2.rxx <- .89; g.2.ryy <- .71

r = .55; n = 657; conf.lvl = .95
g.3.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.3.se <- 1/sqrt(n - 3)
g.3.ci <-convert_r2z(r) + c(-1, 1) * 
  g.3.se * qnorm((1 + conf.lvl) / 2)
g.3.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.3.ci)), totaln = n)
g.3.study <- "Baughman et al. (2012)"
g.3.m.1 <- "Bullying Total"; g.3.m.2 <- "Psychopathy"; g.3.rxx <- .89; g.3.ryy <- .78

r = .33; n = 657; conf.lvl = .95
g.4.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.4.se <- 1/sqrt(n - 3)
g.4.ci <-convert_r2z(r) + c(-1, 1) * 
  g.4.se * qnorm((1 + conf.lvl) / 2)
g.4.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.4.ci)), totaln = n)
g.4.study <- "Baughman et al. (2012)"
g.4.m.1 <- "Bullying Indirect"; g.4.m.2 <- "Machiavellianism"; g.4.rxx <- .76; g.4.ryy <- .73

r = .21; n = 657; conf.lvl = .95
g.5.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.5.se <- 1/sqrt(n - 3)
g.5.ci <-convert_r2z(r) + c(-1, 1) * 
  g.5.se * qnorm((1 + conf.lvl) / 2)
g.5.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.5.ci)), totaln = n)
g.5.study <- "Baughman et al. (2012)"
g.5.m.1 <- "Bullying Indirect"; g.5.m.2 <- "Narcissism"; g.5.rxx <- .76; g.5.ryy <- .71

r = .49; n = 657; conf.lvl = .95
g.6.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.6.se <- 1/sqrt(n - 3)
g.6.ci <-convert_r2z(r) + c(-1, 1) * 
  g.6.se * qnorm((1 + conf.lvl) / 2)
g.6.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.6.ci)), totaln = n)
g.6.study <- "Baughman et al. (2012)"
g.6.m.1 <- "Bullying Indirect"; g.6.m.2 <- "Psychopathy"; g.6.rxx <- .76; g.6.ryy <- .78

r = .34; n = 657; conf.lvl = .95
g.7.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.7.se <- 1/sqrt(n - 3)
g.7.ci <-convert_r2z(r) + c(-1, 1) * 
  g.7.se * qnorm((1 + conf.lvl) / 2)
g.7.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.7.ci)), totaln = n)
g.7.study <- "Baughman et al. (2012)"
g.7.m.1 <- "Bullying Direct"; g.7.m.2 <- "Machiavellianism"; g.7.rxx <- .86; g.7.ryy <- .73

r = .20; n = 657; conf.lvl = .95
g.8.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.8.se <- 1/sqrt(n - 3)
g.8.ci <-convert_r2z(r) + c(-1, 1) * 
  g.8.se * qnorm((1 + conf.lvl) / 2)
g.8.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.8.ci)), totaln = n)
g.8.study <- "Baughman et al. (2012)"
g.8.m.1 <- "Bullying Direct"; g.8.m.2 <- "Narcissism"; g.8.rxx <- .86; g.8.ryy <- .71

r = .53; n = 657; conf.lvl = .95
g.9.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.9.se <- 1/sqrt(n - 3)
g.9.ci <-convert_r2z(r) + c(-1, 1) * 
  g.9.se * qnorm((1 + conf.lvl) / 2)
g.9.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.9.ci)), totaln = n)
g.9.study <- "Baughman et al. (2012)"
g.9.m.1 <- "Bullying Direct"; g.9.m.2 <- "Psychopathy"; g.9.rxx <- .86; g.9.ryy <- .78

r = .35; n = 657; conf.lvl = .95
g.10.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.10.se <- 1/sqrt(n - 3)
g.10.ci <-convert_r2z(r) + c(-1, 1) * 
  g.10.se * qnorm((1 + conf.lvl) / 2)
g.10.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.10.ci)), totaln = n)
g.10.study <- "Baughman et al. (2012)"
g.10.m.1 <- "Bullying Verbal Direct"; g.10.m.2 <- "Machiavellianism"; g.10.rxx <- .83; g.10.ryy <- .73

r = .20; n = 657; conf.lvl = .95
g.11.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.11.se <- 1/sqrt(n - 3)
g.11.ci <-convert_r2z(r) + c(-1, 1) * 
  g.11.se * qnorm((1 + conf.lvl) / 2)
g.11.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.11.ci)), totaln = n)
g.11.study <- "Baughman et al. (2012)"
g.11.m.1 <- "Bullying Verbal Direct"; g.11.m.2 <- "Narcissism"; g.11.rxx <- .83; g.11.ryy <- .71

r = .51; n = 657; conf.lvl = .95
g.12.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.12.se <- 1/sqrt(n - 3)
g.12.ci <-convert_r2z(r) + c(-1, 1) * 
  g.12.se * qnorm((1 + conf.lvl) / 2)
g.12.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.12.ci)), totaln = n)
g.12.study <- "Baughman et al. (2012)"
g.12.m.1 <- "Bullying Verbal Direct"; g.12.m.2 <- "Psychopathy"; g.12.rxx <- .83; g.12.ryy <- .78

r = .22; n = 657; conf.lvl = .95
g.13.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.13.se <- 1/sqrt(n - 3)
g.13.ci <-convert_r2z(r) + c(-1, 1) * 
  g.13.se * qnorm((1 + conf.lvl) / 2)
g.13.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.13.ci)), totaln = n)
g.13.study <- "Baughman et al. (2012)"
g.13.m.1 <- "Bullying Physical Direct"; g.13.m.2 <- "Machiavellianism"; g.13.rxx <- .69; g.13.ryy <- .73

r = .11; n = 657; conf.lvl = .95
g.14.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.14.se <- 1/sqrt(n - 3)
g.14.ci <-convert_r2z(r) + c(-1, 1) * 
  g.14.se * qnorm((1 + conf.lvl) / 2)
g.14.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.14.ci)), totaln = n)
g.14.study <- "Baughman et al. (2012)"
g.14.m.1 <- "Bullying Physical Direct"; g.14.m.2 <- "Narcissism"; g.14.rxx <- .69; g.14.ryy <- .71

r = .41; n = 657; conf.lvl = .95
g.15.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.15.se <- 1/sqrt(n - 3)
g.15.ci <-convert_r2z(r) + c(-1, 1) * 
  g.15.se * qnorm((1 + conf.lvl) / 2)
g.15.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.15.ci)), totaln = n)
g.15.study <- "Baughman et al. (2012)"
g.15.m.1 <- "Bullying Physical Direct"; g.15.m.2 <- "Psychopathy"; g.15.rxx <- .69; g.15.ryy <- .78

### Combinare efecte si construirea bazei de date a efectelor ####
efecte <- data.frame(
              c(g.1.study, g.1.es, 1/g.1.se ^ 2, 657, g.1.se, g.1.se ^ 2, g.1.ci[1], g.1.ci[2], 
                "g", year, g.1.m.1, g.1.m.2, g.1.rxx, g.1.ryy),
              c(g.2.study, g.2.es, 1/g.2.se ^ 2, 657, g.2.se, g.2.se ^ 2, g.2.ci[1], g.2.ci[2], 
                "g", year, g.2.m.1, g.2.m.2, g.2.rxx, g.2.ryy),
              c(g.3.study, g.3.es, 1/g.3.se ^ 2, 657, g.3.se, g.3.se ^ 2, g.3.ci[1], g.3.ci[2], 
                "g", year, g.3.m.1, g.3.m.2, g.3.rxx, g.3.ryy),
              c(g.4.study, g.4.es, 1/g.4.se ^ 2, 657, g.4.se, g.4.se ^ 2, g.4.ci[1], g.4.ci[2], 
                "g", year, g.4.m.1, g.4.m.2, g.4.rxx, g.4.ryy),
              c(g.5.study, g.5.es, 1/g.5.se ^ 2, 657, g.5.se, g.5.se ^ 2, g.5.ci[1], g.5.ci[2], 
                "g", year, g.5.m.1, g.5.m.2, g.5.rxx, g.5.ryy),
              c(g.6.study, g.6.es, 1/g.6.se ^ 2, 657, g.6.se, g.6.se ^ 2, g.6.ci[1], g.6.ci[2], 
                "g", year, g.6.m.1, g.6.m.2, g.6.rxx, g.6.ryy),
              c(g.7.study, g.7.es, 1/g.7.se ^ 2, 657, g.7.se, g.7.se ^ 2, g.7.ci[1], g.7.ci[2],
                "g", year, g.7.m.1, g.7.m.2, g.7.rxx, g.7.ryy),
              c(g.8.study, g.8.es, 1/g.8.se ^ 2, 657, g.8.se, g.8.se ^ 2, g.8.ci[1], g.8.ci[2], 
                "g", year, g.8.m.1, g.8.m.2, g.8.rxx, g.8.ryy),
              c(g.9.study, g.9.es, 1/g.9.se ^ 2, 657, g.9.se, g.9.se ^ 2, g.9.ci[1], g.9.ci[2], 
                "g", year, g.9.m.1, g.9.m.2, g.9.rxx, g.9.ryy),
              c(g.10.study, g.10.es, 1/g.10.se ^ 2, 657, g.10.se, g.10.se ^ 2, g.10.ci[1], g.10.ci[2], 
                "g", year, g.10.m.1, g.10.m.2, g.10.rxx, g.10.ryy),
              c(g.11.study, g.11.es, 1/g.11.se ^ 2, 657, g.11.se, g.11.se ^ 2, g.11.ci[1], g.11.ci[2], 
                "g", year, g.11.m.1, g.11.m.2, g.11.rxx, g.11.ryy),
              c(g.12.study, g.12.es, 1/g.12.se ^ 2, 657, g.12.se, g.12.se ^ 2, g.12.ci[1], g.12.ci[2], 
                "g", year, g.12.m.1, g.12.m.2, g.12.rxx, g.12.ryy),
              c(g.13.study, g.13.es, 1/g.13.se ^ 2, 657, g.13.se, g.13.se ^ 2, g.13.ci[1], g.13.ci[2], 
                "g", year, g.13.m.1, g.13.m.2, g.13.rxx, g.13.ryy),
              c(g.14.study, g.14.es, 1/g.14.se ^ 2, 657, g.14.se, g.14.se ^ 2, g.14.ci[1], g.14.ci[2], 
                "g", year, g.14.m.1, g.14.m.2, g.14.rxx, g.14.ryy),
              c(g.15.study, g.15.es, 1/g.15.se ^ 2, 657, g.15.se, g.15.se ^ 2, g.15.ci[1], g.15.ci[2], 
                "g", year, g.15.m.1, g.15.m.2, g.15.rxx, g.15.ryy))
efecte <- as.data.frame(t(efecte)); rownames(efecte) <- NULL
colnames(efecte) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                      "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), n, conf.lvl, r, year)

## 2. Studiul ref_006 - CODARE ####
## O. Bogolyubova, P. Panicheva, R. Tikhonov, V. Ivanov, Y. Ledovaya (2018) 
## Dark personalities on Facebook: Harmful online behaviors and language
year <- tabel.surse[which(tabel.surse$label == "ref_006"),]$year
g.1 <- esc_beta(beta = 1.00, sdy = 0.06 * sqrt(1487 + 5237), grp1n = 1487, grp2n = 5237, 
                  es.type = "g", study = "Bogolyubova et al. (2018)")
g.1.m.1 <- "Harmfull"; g.1.m.2 <- "Psychopathy"; g.1.rxx <- NA; g.1.ryy <- .72

g.2 <- esc_beta(beta = 0.01, sdy = 0.05 * sqrt(1487 + 5237), grp1n = 1487, grp2n = 5237, 
                  es.type = "g", study = "Bogolyubova et al. (2018)")
g.2.m.1 <- "Harmfull"; g.2.m.2 <- "Machiavellianism"; g.2.rxx <- NA; g.2.ryy <- .72

g.3 <- esc_beta(beta = 0.01, sdy = 0.05 * sqrt(1487 + 5237), grp1n = 1487, grp2n = 5237,
                  es.type = "g", study = "Bogolyubova et al. (2018)")
g.3.m.1 <- "Harmfull"; g.3.m.2 <- "Narcissism"; g.3.rxx <- NA; g.3.ryy <- .72

### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(g.1, g.2, g.3)
temp <- cbind(temp,
              c(rep(year, 3)),
              c(g.1.m.1, g.2.m.1, g.3.m.1),
              c(g.1.m.2, g.2.m.2, g.3.m.2),
              c(g.1.rxx, g.2.rxx, g.3.rxx),
              c(g.1.ryy, g.2.ryy, g.3.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), year, temp)

## 3. Studiul ref_012 - CODARE ####
## N. Craker, E. March (2016)
## The dark side of Facebook: The Dark Tetrad, negative social potency, and trolling behaviours
year <- tabel.surse[which(tabel.surse$label == "ref_012"),]$year
g.1 <- esc_beta(beta = 0.07, sdy = 0.21 * sqrt(94 + 296), grp1n = 94, grp2n = 296,
                   es.type = "g", study = "Craker et al. (2016)")
g.1.m.1 <- "Trolling"; g.1.m.2 <- "Sadism"; g.1.rxx <- .70; g.1.ryy <- .58

g.2 <- esc_beta(beta = -0.00, sdy = 0.05 * sqrt(94 + 296), grp1n = 94, grp2n = 296,
                   es.type = "g", study = "Craker et al. (2016)")
g.2.m.1 <- "Trolling"; g.2.m.2 <- "Machiavellianism"; g.2.rxx <- .70; g.2.ryy <- .80

g.3 <- esc_beta(beta = 0.06, sdy = 0.05 * sqrt(94 + 296), grp1n = 94, grp2n = 296,
                   es.type = "g", study = "Craker et al. (2016)")
g.3.m.1 <- "Trolling"; g.3.m.2 <- "Psychopathy"; g.3.rxx <- .70; g.3.ryy <- .75

g.4 <- esc_beta(beta = -0.01, sdy = 0.03 * sqrt(94 + 296), grp1n = 94, grp2n = 296,
                    es.type = "g", study = "Craker et al. (2016)")
g.4.m.1 <- "Trolling"; g.4.m.2 <- "Narcissism"; g.4.rxx <- .70; g.4.ryy <- .82
### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(g.1, g.2, g.3, g.4)
temp <- cbind(temp,
              c(rep(year, 4)),
              c(g.1.m.1, g.2.m.1, g.3.m.1, g.4.m.1),
              c(g.1.m.2, g.2.m.2, g.3.m.2, g.4.m.2),
              c(g.1.rxx, g.2.rxx, g.3.rxx, g.4.rxx),
              c(g.1.ryy, g.2.ryy, g.3.ryy, g.4.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
# Eliminare obiecte redundante
rm(list = ls(pattern = "^g"), year, temp)

## 4. Studiul ref_020 - CODARE ####
## A. K. Goodboy, M. M. Martin (2015) 
## The personality profile of a cyberbully: Examining the Dark Triad
year <- tabel.surse[which(tabel.surse$label == "ref_020"),]$year
g.1 <- esc_beta(beta = 0.09, sdy = 0.10 * sqrt(104 + 112), grp1n = 104, grp2n = 112,
                  es.type = "g", study = "Goodboy et al. (2015)")
g.1.m.1 <- "Visual Cyberbullying"; g.1.m.2 <- "Machiavellianism"; g.1.rxx <- .84; g.1.ryy <- .79

g.2 <- esc_beta(beta = 0.27, sdy = 0.10 * sqrt(104 + 112), grp1n = 104, grp2n = 112,
                  es.type = "g", study = "Goodboy et al. (2015)")
g.2.m.1 <- "Visual Cyberbullying"; g.2.m.2 <- "Psychopathy"; g.2.rxx <- .84; g.2.ryy <- .80

g.3 <- esc_beta(beta = 0.05, sdy = 0.09 * sqrt(104 + 112), grp1n = 104, grp2n = 112,
                  es.type = "g", study = "Goodboy et al. (2015)")
g.3.m.1 <- "Visual Cyberbullying"; g.3.m.2 <- "Narcissism"; g.3.rxx <- .84; g.3.ryy <- .82

g.4 <- esc_beta(beta = 0.07, sdy = 0.12 * sqrt(104 + 112), grp1n = 104, grp2n = 112,
                  es.type = "g", study = "Goodboy et al. (2015)")
g.4.m.1 <- "Text Cyberbullying"; g.4.m.2 <- "Machiavellianism"; g.4.rxx <- .87; g.4.ryy <- .79

g.5 <- esc_beta(beta = 0.30, sdy = 0.11 * sqrt(104 + 112), grp1n = 104, grp2n = 112,
                  es.type = "g", study = "Goodboy et al. (2015)")
g.5.m.1 <- "Text Cyberbullying"; g.5.m.2 <- "Psychopathy"; g.5.rxx <- .87; g.5.ryy <- .80

g.6 <- esc_beta(beta = 0.12, sdy = 0.10 * sqrt(104 + 112), grp1n = 104, grp2n = 112,
                  es.type = "g", study = "Goodboy et al. (2015)")
g.6.m.1 <- "Text Cyberbullying"; g.6.m.2 <- "Narcissism"; g.6.rxx <- .87; g.6.ryy <- .82

### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(g.1, g.2, g.3, g.4, g.5, g.6)
temp <- cbind(temp,
              c(rep(year, 6)),
              c(g.1.m.1, g.2.m.1, g.3.m.1, g.4.m.1, g.5.m.1, g.6.m.1),
              c(g.1.m.2, g.2.m.2, g.3.m.2, g.4.m.2, g.5.m.2, g.6.m.2),
              c(g.1.rxx, g.2.rxx, g.3.rxx, g.4.rxx, g.5.rxx, g.6.rxx),
              c(g.1.ryy, g.2.ryy, g.3.ryy, g.4.ryy, g.5.ryy, g.6.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
# Eliminare obiecte redundante
rm(list = ls(pattern = "^g"), year, temp)

## 5. Studiul ref_022 - CODARE ####
## C. J. Hand, Graham G. Scott b, Zara P. Brodie b, Xilei Ye c, Sara C. Sereno (2021) 
## Tweet valence, volume of abuse, and observers’ dark tetrad personality factors influence victim-blaming and the perceived severity of twitter cyberabuse
year <- tabel.surse[which(tabel.surse$label == "ref_022"),]$year
r = .211; n = 125; conf.lvl = .95
g.1.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.1.se <- 1/sqrt(n - 3)
g.1.ci <-convert_r2z(r) + c(-1, 1) * 
  g.1.se * qnorm((1 + conf.lvl) / 2)
g.1.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.1.ci)), totaln = n)
g.1.study <- "Hand et al. (2021)"
g.1.m.1 <- "Victim Blame"; g.1.m.2 <- "Psychopathy"; g.1.rxx <- .90; g.1.ryy <- .71

r = .164; n = 125; conf.lvl = .95
g.2.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.2.se <- 1/sqrt(n - 3)
g.2.ci <-convert_r2z(r) + c(-1, 1) * 
  g.2.se * qnorm((1 + conf.lvl) / 2)
g.2.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.2.ci)), totaln = n)
g.2.study <- "Hand et al. (2021)"
g.2.m.1 <- "Victim Blame"; g.2.m.2 <- "Narcissism"; g.2.rxx <- .90; g.2.ryy <- .73

r = .252; n = 125; conf.lvl = .95
g.3.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.3.se <- 1/sqrt(n - 3)
g.3.ci <-convert_r2z(r) + c(-1, 1) * 
  g.3.se * qnorm((1 + conf.lvl) / 2)
g.3.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.3.ci)), totaln = n)
g.3.study <- "Hand et al. (2021)"
g.3.m.1 <- "Victim Blame"; g.3.m.2 <- "Machiavellianism"; g.3.rxx <- .90; g.3.ryy <- .70

r = .265; n = 125; conf.lvl = .95
g.4.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.4.se <- 1/sqrt(n - 3)
g.4.ci <-convert_r2z(r) + c(-1, 1) * 
  g.4.se * qnorm((1 + conf.lvl) / 2)
g.4.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.4.ci)), totaln = n)
g.4.study <- "Hand et al. (2021)"
g.4.m.1 <- "Victim Blame"; g.4.m.2 <- "Sadism"; g.4.rxx <- .90; g.4.ryy <- .85

### Combinare efecte si construirea bazei de date a efectelor ####
efecte <- rbind(efecte,
                c(g.1.study, g.1.es, 1/g.1.se ^ 2, 125, g.1.se, g.1.se ^ 2, g.1.ci[1], g.1.ci[2], 
                  "g", year, g.1.m.1, g.1.m.2, g.1.rxx, g.1.ryy),
                c(g.2.study, g.2.es, 1/g.2.se ^ 2, 125, g.2.se, g.2.se ^ 2, g.2.ci[1], g.2.ci[2], 
                  "g", year, g.2.m.1, g.2.m.2, g.2.rxx, g.2.ryy),
                c(g.3.study, g.3.es, 1/g.3.se ^ 2, 125, g.3.se, g.3.se ^ 2, g.3.ci[1], g.3.ci[2], 
                  "g", year, g.3.m.1, g.3.m.2, g.3.rxx, g.3.ryy),
                c(g.4.study, g.4.es, 1/g.4.se ^ 2, 125, g.4.se, g.4.se ^ 2, g.4.ci[1], g.4.ci[2], 
                  "g", year, g.4.m.1, g.4.m.2, g.4.rxx, g.4.ryy))
# Eliminare obiecte redundante
rm(list = ls(pattern = "^g"), year, conf.lvl, n, r)

## 6. Studiul ref_029 - CODARE ####
## Kurek, A., Jose, P. E., & Stuart, J. (2019). 
## I did it for the LULZ: How the dark personality predicts online disinhibition and aggressive online behavior in adolescence.
year <- tabel.surse[which(tabel.surse$label == "ref_029"),]$year
r = .17; n = 718; conf.lvl = .95
g.1.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.1.se <- 1/sqrt(n - 3)
g.1.ci <-convert_r2z(r) + c(-1, 1) * 
  g.1.se * qnorm((1 + conf.lvl) / 2)
g.1.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.1.ci)), totaln = n)
g.1.study <- "Kurek et al. (2019)"
g.1.m.1 <- "Cyber Aggression"; g.1.m.2 <- "Narcissism"; g.1.rxx <- .92; g.1.ryy <- .93

r = .26; n = 718; conf.lvl = .95
g.2.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.2.se <- 1/sqrt(n - 3)
g.2.ci <-convert_r2z(r) + c(-1, 1) * 
  g.2.se * qnorm((1 + conf.lvl) / 2)
g.2.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.2.ci)), totaln = n)
g.2.study <- "Kurek et al. (2019)"
g.2.m.1 <- "Cyber Aggression"; g.2.m.2 <- "Sadism"; g.2.rxx <- .92; g.2.ryy <- .79

r = .05; n = 718; conf.lvl = .95
g.3.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.3.se <- 1/sqrt(n - 3)
g.3.ci <-convert_r2z(r) + c(-1, 1) * 
  g.3.se * qnorm((1 + conf.lvl) / 2)
g.3.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.3.ci)), totaln = n)
g.3.study <- "Kurek et al. (2019)"
g.3.m.1 <- "Cyber Aggression"; g.3.m.2 <- "Psychopathy"; g.3.rxx <- .92; g.3.ryy <- .71
### Combinare efecte si construirea bazei de date a efectelor ####
efecte <- rbind(efecte,
                c(g.1.study, g.1.es, 1/g.1.se ^ 2, 718, g.1.se, g.1.se ^ 2, g.1.ci[1], g.1.ci[2], 
                  "g", year, g.1.m.1, g.1.m.2, g.1.rxx, g.1.ryy),
                c(g.2.study, g.2.es, 1/g.2.se ^ 2, 718, g.2.se, g.2.se ^ 2, g.2.ci[1], g.2.ci[2], 
                  "g", year, g.2.m.1, g.2.m.2, g.2.rxx, g.2.ryy),
                c(g.3.study, g.3.es, 1/g.3.se ^ 2, 718, g.3.se, g.3.se ^ 2, g.3.ci[1], g.3.ci[2], 
                  "g", year, g.3.m.1, g.3.m.2, g.3.rxx, g.3.ryy))
# Eliminare obiecte redundante
rm(list = ls(pattern = "^g"), year, conf.lvl, n, r)

## 7. Studiul ref_045 - CODARE ####
## Pabian, S., De Backer, C. J. S., & Vandebosch, H. (2015) 
## Dark Triad personality traits and adolescent cyber-aggression
year <- tabel.surse[which(tabel.surse$label == "ref_045"),]$year
t.val <- qt(0.05 / 2, df = 277); beta <- 0.60
g.1 <- esc_beta(beta = beta, sdy = beta / abs(t.val), grp1n = 0.37 * 277, grp2n = 0.63 * 277,
                  es.type = "g", study = "Pabian et al. (2015)")
g.1.m.1 <- "Cyber Aggression"; g.1.m.2 <- "Psychopathy"; g.1.rxx <- .84; g.1.ryy <- .77

t.val <- qt(0.199 / 2, df = 277); beta <- -0.36
g.2 <- esc_beta(beta = beta, sdy = beta / abs(t.val), grp1n = 0.37 * 277, grp2n = 0.63 * 277,
                   es.type = "g", study = "Pabian et al. (2015)")
g.2.m.1 <- "Cyber Aggression"; g.2.m.2 <- "Machiavellianism"; g.2.rxx <- .84; g.2.ryy <- .74

t.val <- qt(0.055 / 2, df = 277); beta <- 0.23
g.3 <- esc_beta(beta = beta, sdy = beta / abs(t.val), grp1n = 0.37 * 277, grp2n = 0.63 * 277,
                   es.type = "g", study = "Pabian et al. (2015)")
g.3.m.1 <- "Cyber Aggression"; g.3.m.2 <- "Narcissism"; g.3.rxx <- .84; g.3.ryy <- .61
### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(g.1, g.2, g.3)
temp <- cbind(temp,
              c(rep(year, 3)),
              c(g.1.m.1, g.2.m.1, g.3.m.1),
              c(g.1.m.2, g.2.m.2, g.3.m.2),
              c(g.1.rxx, g.2.rxx, g.3.rxx),
              c(g.1.ryy, g.2.ryy, g.3.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), temp, year, t.val, beta)

## 8. Studiul ref_053 - CODARE ####
## Stiff C. (2019)
## The Dark Triad and Facebook surveillance: How Machiavellianism, psychopathy, but not narcissism predict using Facebook to spy on others
year <- tabel.surse[which(tabel.surse$label == "ref_053"),]$year
g.1 <- esc_beta(beta = 0.16, sdy = 0.10 * sqrt(108 + 150), grp1n = 108, grp2n = 150,
                   es.type = "g", study = "Stiff et al. (2019)")
g.1.m.1 <- "Facebook tracking"; g.1.m.2 <- "Machiavellianism"; g.1.rxx <- .7; g.1.ryy <- .7

g.2 <- esc_beta(beta = 0.04, sdy = 0.09 * sqrt(108 + 150), grp1n = 108, grp2n = 150,
                    es.type = "g", study = "Stiff et al. (2019)")
g.2.m.1 <- "Facebook investigating"; g.2.m.2 <- "Machiavellianism"; g.2.rxx <- .7; g.2.ryy <- .7

g.3 <- esc_beta(beta = 0.27, sdy = 0.10 * sqrt(108 + 150), grp1n = 108, grp2n = 150,
                    es.type = "g", study = "Stiff et al. (2019)")
g.3.m.1 <- "Facebook tracking"; g.3.m.2 <- "Psychopathy"; g.3.rxx <- .7; g.3.ryy <- .7

g.4 <- esc_beta(beta = 0.10, sdy = 0.09 * sqrt(108 + 150), grp1n = 108, grp2n = 150,
                    es.type = "g", study = "Stiff et al. (2019)")
g.4.m.1 <- "Facebook investigating"; g.4.m.2 <- "Psychopathy"; g.4.rxx <- .7; g.4.ryy <- .7

### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(g.1, g.2, g.3, g.4)
temp <- cbind(temp,
              c(rep(year, 4)),
              c(g.1.m.1, g.2.m.1, g.3.m.1, g.4.m.1),
              c(g.1.m.2, g.2.m.2, g.3.m.2, g.4.m.2),
              c(g.1.rxx, g.2.rxx, g.3.rxx, g.4.rxx),
              c(g.1.ryy, g.2.ryy, g.3.ryy, g.4.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), temp)

## 9.a. Studiul ref_059.a - CODARE ####
## Buckels E.E. Trapnell P. D., Andjelovic T, Paulhus D.L. (2019)
## Internet Trolling and Everyday Sadism: Parallel Effects on Pain Perception and Moral Judgment
year <- tabel.surse[which(tabel.surse$label == "ref_059"),]$year
r = .71; n = 304; conf.lvl = .95
g.1.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.1.se <- 1/sqrt(n - 3)
g.1.ci <- convert_r2z(r) + c(-1, 1) * 
  g.1.se * qnorm((1 + conf.lvl) / 2)
g.1.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.1.ci)), totaln = n)
g.1.study <- "Buckels et al. (2019)"
g.1.m.1 <- "Trolling"; g.1.m.2 <- "Sadism"; g.1.rxx <- .85; g.1.ryy <- .89

r = .32; n = 304; conf.lvl = .95
g.2.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.2.se <- 1/sqrt(n - 3)
g.2.ci <- convert_r2z(r) + c(-1, 1) * 
  g.2.se * qnorm((1 + conf.lvl) / 2)
g.2.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.2.ci)), totaln = n)
g.2.study <- "Buckels et al. (2019)"
g.2.m.1 <- "Trolling"; g.2.m.2 <- "Machiavellianism"; g.2.rxx <- .85; g.2.ryy <- .82

r = .26; n = 304; conf.lvl = .95
g.3.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.3.se <- 1/sqrt(n - 3)
g.3.ci <- convert_r2z(r) + c(-1, 1) * 
  g.3.se * qnorm((1 + conf.lvl) / 2)
g.3.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.3.ci)), totaln = n)
g.3.study <- "Buckels et al. (2019)"
g.3.m.1 <- "Trolling"; g.3.m.2 <- "Narcissism"; g.3.rxx <- .85; g.3.ryy <- .76

r = .63; n = 304; conf.lvl = .95
g.4.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.4.se <- 1/sqrt(n - 3)
g.4.ci <- convert_r2z(r) + c(-1, 1) * 
  g.4.se * qnorm((1 + conf.lvl) / 2)
g.4.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.4.ci)), totaln = n)
g.4.study <- "Buckels et al. (2019)"
g.4.m.1 <- "Trolling"; g.4.m.2 <- "Psychopathy"; g.4.rxx <- .85; g.4.ryy <- .82

r = .59; n = 304; conf.lvl = .95
g.5.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.5.se <- 1/sqrt(n - 3)
g.5.ci <- convert_r2z(r) + c(-1, 1) * 
  g.5.se * qnorm((1 + conf.lvl) / 2)
g.5.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.5.ci)), totaln = n)
g.5.study <- "Buckels et al. (2019)"
g.5.m.1 <- "Pain pleasure"; g.5.m.2 <- "Sadism"; g.5.rxx <- .68; g.5.ryy <- .89

r = .31; n = 304; conf.lvl = .95
g.6.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.6.se <- 1/sqrt(n - 3)
g.6.ci <- convert_r2z(r) + c(-1, 1) * 
  g.6.se * qnorm((1 + conf.lvl) / 2)
g.6.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.6.ci)), totaln = n)
g.6.study <- "Buckels et al. (2019)"
g.6.m.1 <- "Pain pleasure"; g.6.m.2 <- "Machiavellianism"; g.6.rxx <- .68; g.6.ryy <- .82

r = .18; n = 304; conf.lvl = .95
g.7.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.7.se <- 1/sqrt(n - 3)
g.7.ci <- convert_r2z(r) + c(-1, 1) * 
  g.7.se * qnorm((1 + conf.lvl) / 2)
g.7.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.7.ci)), totaln = n)
g.7.study <- "Buckels et al. (2019)"
g.7.m.1 <- "Pain pleasure"; g.7.m.2 <- "Narcissism"; g.7.rxx <- .68; g.7.ryy <- .76

r = .56; n = 304; conf.lvl = .95
g.8.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.8.se <- 1/sqrt(n - 3)
g.8.ci <- convert_r2z(r) + c(-1, 1) * 
  g.8.se * qnorm((1 + conf.lvl) / 2)
g.8.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.8.ci)), totaln = n)
g.8.study <- "Buckels et al. (2019)"
g.8.m.1 <- "Pain pleasure"; g.8.m.2 <- "Psychopathy"; g.8.rxx <- .68; g.8.ryy <- .82

### Combinare efecte si construirea bazei de date a efectelor ####
efecte <- rbind(efecte,
                c(g.1.study, g.1.es, 1/g.1.se ^ 2, 304, g.1.se, g.1.se ^ 2, g.1.ci[1], g.1.ci[2], 
                  "g", year, g.1.m.1, g.1.m.2, g.1.rxx, g.1.ryy),
                c(g.2.study, g.2.es, 1/g.2.se ^ 2, 304, g.2.se, g.2.se ^ 2, g.2.ci[1], g.2.ci[2], 
                  "g", year, g.2.m.1, g.2.m.2, g.2.rxx, g.2.ryy),
                c(g.3.study, g.3.es, 1/g.3.se ^ 2, 304, g.3.se, g.3.se ^ 2, g.3.ci[1], g.3.ci[2], 
                  "g", year, g.3.m.1, g.3.m.2, g.3.rxx, g.3.ryy),
                c(g.4.study, g.4.es, 1/g.4.se ^ 2, 304, g.4.se, g.4.se ^ 2, g.4.ci[1], g.4.ci[2], 
                  "g", year, g.4.m.1, g.4.m.2, g.4.rxx, g.4.ryy),
                c(g.5.study, g.5.es, 1/g.5.se ^ 2, 304, g.5.se, g.5.se ^ 2, g.5.ci[1], g.5.ci[2], 
                  "g", year, g.5.m.1, g.5.m.2, g.5.rxx, g.5.ryy),
                c(g.6.study, g.6.es, 1/g.6.se ^ 2, 304, g.6.se, g.6.se ^ 2, g.6.ci[1], g.6.ci[2], 
                  "g", year, g.6.m.1, g.6.m.2, g.6.rxx, g.6.ryy),
                c(g.7.study, g.7.es, 1/g.7.se ^ 2, 304, g.7.se, g.7.se ^ 2, g.7.ci[1], g.7.ci[2], 
                  "g", year, g.7.m.1, g.7.m.2, g.7.rxx, g.7.ryy),
                c(g.8.study, g.8.es, 1/g.8.se ^ 2, 304, g.8.se, g.8.se ^ 2, g.8.ci[1], g.8.ci[2], 
                  "g", year, g.8.m.1, g.8.m.2, g.8.rxx, g.8.ryy))
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), n, conf.lvl, r, year)

## 9.b. Studiul ref_059.b - CODARE ####
## Buckels E.E. Trapnell P. D., Andjelovic T, Paulhus D.L. (2018)
## Internet Trolling and Everyday Sadism: Parallel Effects on Pain Perception and Moral Judgment
year <- tabel.surse[which(tabel.surse$label == "ref_059"),]$year
r = .44; n = 223; conf.lvl = .95
g.1.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.1.se <- 1/sqrt(n - 3)
g.1.ci <- convert_r2z(r) + c(-1, 1) * 
  g.1.se * qnorm((1 + conf.lvl) / 2)
g.1.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.1.ci)), totaln = n)
g.1.study <- "Buckels et al. (2019)"
g.1.m.1 <- "Trolling"; g.1.m.2 <- "Sadism"; g.1.rxx <- .91; g.1.ryy <- .89

r = .43; n = 223; conf.lvl = .95
g.2.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.2.se <- 1/sqrt(n - 3)
g.2.ci <- convert_r2z(r) + c(-1, 1) * 
  g.2.se * qnorm((1 + conf.lvl) / 2)
g.2.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.2.ci)), totaln = n)
g.2.study <- "Buckels et al. (2019)"
g.2.m.1 <- "Trolling"; g.2.m.2 <- "Psychopathy"; g.2.rxx <- .91; g.2.ryy <- .82
### Combinare efecte si construirea bazei de date a efectelor ####
efecte <- rbind(efecte,
                c(g.1.study, g.1.es, 1/g.1.se ^ 2, 223, g.1.se, g.1.se ^ 2, g.1.ci[1], g.1.ci[2], 
                  "g", year, g.1.m.1, g.1.m.2, g.1.rxx, g.1.ryy),
                c(g.2.study, g.2.es, 1/g.2.se ^ 2, 223, g.2.se, g.2.se ^ 2, g.2.ci[1], g.2.ci[2], 
                  "g", year, g.2.m.1, g.2.m.2, g.2.rxx, g.2.ryy))
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), n, conf.lvl, r, year)

## 10. Studiul ref_067 - CODARE ####
## Brown W.M., Hazraty S., Palasinski M. (2019) 
## Examining the Dark Tetrad and Its Links to Cyberbullying
year <- tabel.surse[which(tabel.surse$label == "ref_067"),]$year
g.1 <- esc_f(f = 24.40, totaln = 790, es.type = "g", study = "Brown et al. (2019)")
g.1.m.1 <- "Cyberbullying"; g.1.m.2 <- "Narcissism"; g.1.rxx <- .93; g.1.ryy <- .88 # Male

g.2 <- esc_f(f = 42.67, totaln = 1310, es.type = "g", study = "Brown et al. (2019)")
g.2.m.1 <- "Cyberbullying"; g.2.m.2 <- "Narcissism"; g.2.rxx <- .93; g.2.ryy <- .88 # Female

g.3 <- esc_f(f = 52.93, totaln = 790, es.type = "g", study = "Brown et al. (2019)")
g.3.m.1 <- "Cyberbullying"; g.3.m.2 <- "Machiavellianism"; g.3.rxx <- .93; g.3.ryy <- .88 # Male

g.4 <- esc_f(f = 129.7, totaln = 1310, es.type = "g", study = "Brown et al. (2019)")
g.4.m.1 <- "Cyberbullying"; g.4.m.2 <- "Machiavellianism"; g.4.rxx <- .93; g.4.ryy <- .88 # Female

g.5 <- esc_f(f = 73.00, totaln = 790, es.type = "g", study = "Brown et al. (2019)")
g.5.m.1 <- "Cyberbullying"; g.5.m.2 <- "Psychopathy"; g.5.rxx <- .93; g.5.ryy <- .88 # Male

g.6 <- esc_f(f = 118.10, totaln = 1310, es.type = "g", study = "Brown et al. (2019)")
g.6.m.1 <- "Cyberbullying"; g.6.m.2 <- "Psychopathy"; g.6.rxx <- .93; g.6.ryy <- .88 # Female

g.7 <- esc_f(f = 25.63, totaln = 790, es.type = "g", study = "Brown et al. (2019)")
g.7.m.1 <- "Cyberbullying"; g.7.m.2 <- "Sadism"; g.7.rxx <- .93; g.7.ryy <- .88 # Male

g.8 <- esc_f(f = 9.54, totaln = 1310, es.type = "g", study = "Brown et al. (2019)")
g.8.m.1 <- "Cyberbullying"; g.8.m.2 <- "Sadism"; g.8.rxx <- .93; g.8.ryy <- .88 # Female

### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(g.1, g.2, g.3, g.4, g.5, g.6, g.7, g.8)
temp <- cbind(temp,
              c(rep(year, 8)),
              c(g.1.m.1, g.2.m.1, g.3.m.1, g.4.m.1, g.5.m.1, g.6.m.1, 
                g.7.m.1, g.8.m.1),
              c(g.1.m.2, g.2.m.2, g.3.m.2, g.4.m.2, g.5.m.2, g.6.m.2,
                g.7.m.2, g.8.m.2),
              c(g.1.rxx, g.2.rxx, g.3.rxx, g.4.rxx, g.5.rxx, g.6.rxx, 
                g.7.rxx, g.8.rxx),
              c(g.1.ryy, g.2.ryy, g.3.ryy, g.4.ryy, g.5.ryy, g.6.ryy, 
                g.7.ryy, g.8.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), temp, year)

## 11. Studiul ref_068 - CODARE ####
## Kircaburuna K., Jonasonb P.K., Griffithsc M.D. (2018)
## The Dark Tetrad traits and problematic social media use: The mediating role of cyberbullying and cyberstalking
year <- tabel.surse[which(tabel.surse$label == "ref_068"),]$year
r = .46; n = 761; conf.lvl = .95
g.1.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.1.se <- 1/sqrt(n - 3)
g.1.ci <- convert_r2z(r) + c(-1, 1) * 
  g.1.se * qnorm((1 + conf.lvl) / 2)
g.1.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.1.ci)), totaln = n)
g.1.study <- "Kircaburuna et al. (2018)"
g.1.m.1 <- "Cyberbullying"; g.1.m.2 <- "Machiavellianism"; g.1.rxx <- .79; g.1.ryy <- .82

r = .41; n = 761; conf.lvl = .95
g.2.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.2.se <- 1/sqrt(n - 3)
g.2.ci <- convert_r2z(r) + c(-1, 1) * 
  g.2.se * qnorm((1 + conf.lvl) / 2)
g.2.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.2.ci)), totaln = n)
g.2.study <- "Kircaburuna et al. (2018)"
g.2.m.1 <- "Cyberbullying"; g.2.m.2 <- "Psychopathy"; g.2.rxx <- .79; g.2.ryy <- .66

r = .30; n = 761; conf.lvl = .95
g.3.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.3.se <- 1/sqrt(n - 3)
g.3.ci <- convert_r2z(r) + c(-1, 1) * 
  g.3.se * qnorm((1 + conf.lvl) / 2)
g.3.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.3.ci)), totaln = n)
g.3.study <- "Kircaburuna et al. (2018)"
g.3.m.1 <- "Cyberbullying"; g.3.m.2 <- "Narcissism"; g.3.rxx <- .79; g.3.ryy <- .88

r = .47; n = 761; conf.lvl = .95
g.4.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.4.se <- 1/sqrt(n - 3)
g.4.ci <- convert_r2z(r) + c(-1, 1) * 
  g.4.se * qnorm((1 + conf.lvl) / 2)
g.4.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.4.ci)), totaln = n)
g.4.study <- "Kircaburuna et al. (2018)"
g.4.m.1 <- "Cyberbullying"; g.4.m.2 <- "Sadism"; g.4.rxx <- .79; g.4.ryy <- .74

r = .42; n = 761; conf.lvl = .95
g.5.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.5.se <- 1/sqrt(n - 3)
g.5.ci <- convert_r2z(r) + c(-1, 1) * 
  g.5.se * qnorm((1 + conf.lvl) / 2)
g.5.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.5.ci)), totaln = n)
g.5.study <- "Kircaburuna et al. (2018)"
g.5.m.1 <- "Cybertrolling"; g.5.m.2 <- "Machiavellianism"; g.5.rxx <- .83; g.5.ryy <- .82

r = .38; n = 761; conf.lvl = .95
g.6.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.6.se <- 1/sqrt(n - 3)
g.6.ci <- convert_r2z(r) + c(-1, 1) * 
  g.6.se * qnorm((1 + conf.lvl) / 2)
g.6.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.6.ci)), totaln = n)
g.6.study <- "Kircaburuna et al. (2018)"
g.6.m.1 <- "Cybertrolling"; g.6.m.2 <- "Psychopathy"; g.6.rxx <- .83; g.6.ryy <- .66

r = .28; n = 761; conf.lvl = .95
g.7.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.7.se <- 1/sqrt(n - 3)
g.7.ci <- convert_r2z(r) + c(-1, 1) * 
  g.7.se * qnorm((1 + conf.lvl) / 2)
g.7.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.7.ci)), totaln = n)
g.7.study <- "Kircaburuna et al. (2018)"
g.7.m.1 <- "Cybertrolling"; g.7.m.2 <- "Narcissism"; g.7.rxx <- .83; g.7.ryy <- .88

r = .39; n = 761; conf.lvl = .95
g.8.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.8.se <- 1/sqrt(n - 3)
g.8.ci <- convert_r2z(r) + c(-1, 1) * 
  g.8.se * qnorm((1 + conf.lvl) / 2)
g.8.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.8.ci)), totaln = n)
g.8.study <- "Kircaburuna et al. (2018)"
g.8.m.1 <- "Cybertrolling"; g.8.m.2 <- "Sadism"; g.8.rxx <- .83; g.8.ryy <- .74

### Combinare efecte si construirea bazei de date a efectelor ####
efecte <- rbind(efecte,
                c(g.1.study, g.1.es, 1/g.1.se ^ 2, 761, g.1.se, g.1.se ^ 2, g.1.ci[1], g.1.ci[2], 
                  "g", year, g.1.m.1, g.1.m.2, g.1.rxx, g.1.ryy),
                c(g.2.study, g.2.es, 1/g.2.se ^ 2, 761, g.2.se, g.1.se ^ 2, g.2.ci[1], g.2.ci[2], 
                  "g", year, g.2.m.1, g.2.m.2, g.2.rxx, g.2.ryy),
                c(g.3.study, g.3.es, 1/g.3.se ^ 2, 761, g.3.se, g.3.se ^ 2, g.3.ci[1], g.3.ci[2], 
                  "g", year, g.3.m.1, g.3.m.2, g.3.rxx, g.3.ryy),
                c(g.4.study, g.4.es, 1/g.4.se ^ 2, 761, g.4.se, g.4.se ^ 2, g.4.ci[1], g.4.ci[2], 
                  "g", year, g.4.m.1, g.4.m.2, g.4.rxx, g.4.ryy),
                c(g.5.study, g.5.es, 1/g.5.se ^ 2, 761, g.5.se, g.5.se ^ 2, g.5.ci[1], g.5.ci[2], 
                  "g", year, g.5.m.1, g.5.m.2, g.5.rxx, g.5.ryy),
                c(g.6.study, g.6.es, 1/g.6.se ^ 2, 761, g.6.se, g.6.se ^ 2, g.6.ci[1], g.6.ci[2], 
                  "g", year, g.6.m.1, g.6.m.2, g.6.rxx, g.6.ryy),
                c(g.7.study, g.7.es, 1/g.7.se ^ 2, 761, g.7.se, g.7.se ^ 2, g.7.ci[1], g.7.ci[2], 
                  "g", year, g.7.m.1, g.7.m.2, g.7.rxx, g.7.ryy),
                c(g.8.study, g.8.es, 1/g.8.se ^ 2, 761, g.8.se, g.8.se ^ 2, g.8.ci[1], g.8.ci[2], 
                  "g", year, g.8.m.1, g.8.m.2, g.8.rxx, g.8.ryy))
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), n, conf.lvl, r, year)

## 12. Studiul ref_069 - CODARE ####
## Gylfason H.F., Sveinsdottir A.H., Vésteinsdóttir V., Sigurvinsdottir R. (2021) 
## Haters Gonna Hate, Trolls Gonna Troll: The Personality Profile of a Facebook Troll
year <- tabel.surse[which(tabel.surse$label == "ref_069"),]$year
r = .449; n = 139; conf.lvl = .95
g.1.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.1.se <- 1/sqrt(n - 3)
g.1.ci <- convert_r2z(r) + c(-1, 1) * 
  g.1.se * qnorm((1 + conf.lvl) / 2)
g.1.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.1.ci)), totaln = n)
g.1.study <- "Gylfason et al. (2021)"
g.1.m.1 <- "Trolling"; g.1.m.2 <- "Sadism"; g.1.rxx <- .67; g.1.ryy <- .63

r = .367; n = 139; conf.lvl = .95
g.2.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.2.se <- 1/sqrt(n - 3)
g.2.ci <- convert_r2z(r) + c(-1, 1) * 
  g.2.se * qnorm((1 + conf.lvl) / 2)
g.2.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.2.ci)), totaln = n)
g.2.study <- "Gylfason et al. (2021)"
g.2.m.1 <- "Trolling"; g.2.m.2 <- "Machiavellianism"; g.2.rxx <- .67; g.2.ryy <- .80

r = .285; n = 139; conf.lvl = .95
g.3.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.3.se <- 1/sqrt(n - 3)
g.3.ci <- convert_r2z(r) + c(-1, 1) * 
  g.3.se * qnorm((1 + conf.lvl) / 2)
g.3.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.3.ci)), totaln = n)
g.3.study <- "Gylfason et al. (2021)"
g.3.m.1 <- "Trolling"; g.3.m.2 <- "Psychopathy"; g.3.rxx <- .67; g.3.ryy <- .74

r = .105; n = 139; conf.lvl = .95
g.4.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.4.se <- 1/sqrt(n - 3)
g.4.ci <- convert_r2z(r) + c(-1, 1) * 
  g.4.se * qnorm((1 + conf.lvl) / 2)
g.4.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.3.ci)), totaln = n)
g.4.study <- "Gylfason et al. (2021)"
g.4.m.1 <- "Trolling"; g.4.m.2 <- "Narcissism"; g.4.rxx <- .67; g.4.ryy <- .86

### Combinare efecte si construirea bazei de date a efectelor ####
efecte <- rbind(efecte,
                c(g.1.study, g.1.es, 1/g.1.se ^ 2, 139, g.1.se, g.1.se ^ 2, g.1.ci[1], g.1.ci[2], 
                  "g", year, g.1.m.1, g.1.m.2, g.1.rxx, g.1.ryy),
                c(g.2.study, g.2.es, 1/g.2.se ^ 2, 139, g.2.se, g.2.se ^ 2, g.2.ci[1], g.2.ci[2], 
                  "g", year, g.2.m.1, g.2.m.2, g.2.rxx, g.2.ryy),
                c(g.3.study, g.3.es, 1/g.3.se ^ 2, 139, g.3.se, g.3.se ^ 2, g.3.ci[1], g.3.ci[2], 
                  "g", year, g.3.m.1, g.3.m.2, g.3.rxx, g.3.ryy),
                c(g.4.study, g.4.es, 1/g.4.se ^ 2, 139, g.4.se, g.4.se ^ 2, g.4.ci[1], g.4.ci[2], 
                  "g", year, g.4.m.1, g.4.m.2, g.4.rxx, g.4.ryy))
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), n, conf.lvl, r)

g.5 <- esc_beta(beta = .356, sdy = 0.35 * sqrt(17 + 119), grp1n = 17, grp2n = 119, 
                    es.type = "g", study = "Gylfason et al. (2021)")
g.5.m.1 <- "Trolling"; g.5.m.2 <- "Narcissism"; g.5.rxx <- .67; g.5.ryy <- .63

g.6 <- esc_beta(beta = .242, sdy = 0.36 * sqrt(17 + 119), grp1n = 17, grp2n = 119, 
                    es.type = "g", study = "Gylfason et al. (2021)")
g.6.m.1 <- "Trolling"; g.6.m.2 <- "Machiavellianism"; g.6.rxx <- .67; g.6.ryy <- .80

g.7 <- esc_beta(beta = .063, sdy = 0.33 * sqrt(17 + 119), grp1n = 17, grp2n = 119, 
                    es.type = "g", study = "Gylfason et al. (2021)")
g.7.m.1 <- "Trolling"; g.7.m.2 <- "Psychopathy"; g.7.rxx <- .67; g.7.ryy <- .74

g.8 <- esc_beta(beta = -.110, sdy = 0.25 * sqrt(17 + 119), grp1n = 17, grp2n = 119, 
                    es.type = "g", study = "Gylfason et al. (2021)")
g.8.m.1 <- "Trolling"; g.8.m.2 <- "Narcissism"; g.8.rxx <- .67; g.8.ryy <- .86

### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(g.5, g.6, g.7, g.8)
temp <- cbind(temp,
              c(rep(year, 4)),
              c(g.5.m.1, g.6.m.1, g.7.m.1, g.8.m.1),
              c(g.5.m.2, g.6.m.2, g.7.m.2, g.8.m.2),
              c(g.5.rxx, g.6.rxx, g.7.rxx, g.8.rxx),
              c(g.5.ryy, g.6.ryy, g.7.ryy, g.8.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
rm(list = ls(pattern = "^g"), temp, year)

## 13. Studiul ref_150 - CODARE ####
## Duncan Z., March E. (2019) 
## Examining the Dark Tetrad and Its Links to Cyberbullying
year <- tabel.surse[which(tabel.surse$label == "ref_150"),]$year
g.1 <- esc_beta(beta = .08, sdy = 0.03 * sqrt(587), grp1n = 0.21*587, grp2n = 0.79*587, 
                es.type = "g", study = "Duncan et al. (2019)")
g.1.m.1 <- "Antisocial-general"; g.1.m.2 <- "Narcissism"; g.1.rxx <- .85; g.1.ryy <- .69

g.2 <- esc_beta(beta = .09, sdy = 0.03 * sqrt(587), grp1n = 0.21*587, grp2n = 0.79*587, 
                es.type = "g", study = "Duncan et al. (2019)")
g.2.m.1 <- "Antisocial-general"; g.2.m.2 <- "Machiavellianism"; g.2.rxx <- .85; g.2.ryy <- .74

g.3 <- esc_beta(beta = .21, sdy = 0.03 * sqrt(587), grp1n = 0.21*587, grp2n = 0.79*587, 
                es.type = "g", study = "Duncan et al. (2019)")
g.3.m.1 <- "Antisocial-general"; g.3.m.2 <- "Psychopathy"; g.3.rxx <- .85; g.3.ryy <- .73

g.4 <- esc_beta(beta = .26, sdy = 0.03 * sqrt(587), grp1n = 0.21*587, grp2n = 0.79*587, 
                es.type = "g", study = "Duncan et al. (2019)")
g.4.m.1 <- "Antisocial-general"; g.4.m.2 <- "Sadism"; g.4.rxx <- .85; g.4.ryy <- .86

g.5 <- esc_beta(beta = .21, sdy = 0.04 * sqrt(587), grp1n = 0.21*587, grp2n = 0.79*587, 
                es.type = "g", study = "Duncan et al. (2019)")
g.5.m.1 <- "Antisocial-esteem"; g.5.m.2 <- "Machiavellianism"; g.5.rxx <- .89; g.5.ryy <- .74

g.6 <- esc_beta(beta = .06, sdy = 0.05 * sqrt(587), grp1n = 0.21*587, grp2n = 0.79*587, 
                es.type = "g", study = "Duncan et al. (2019)")
g.6.m.1 <- "Antisocial-esteem"; g.6.m.2 <- "Psychopathy"; g.6.rxx <- .89; g.6.ryy <- .73

g.7 <- esc_beta(beta = .03, sdy = 0.04 * sqrt(587), grp1n = 0.21*587, grp2n = 0.79*587, 
                es.type = "g", study = "Duncan et al. (2019)")
g.7.m.1 <- "Antisocial-esteem"; g.7.m.2 <- "Sadism"; g.7.rxx <- .89; g.7.ryy <- .86

g.8 <- esc_beta(beta = .03, sdy = 0.02 * sqrt(587), grp1n = 0.21*587, grp2n = 0.79*587, 
                es.type = "g", study = "Duncan et al. (2019)")
g.8.m.1 <- "Antisocial-sexual"; g.8.m.2 <- "Narcissism"; g.8.rxx <- .67; g.8.ryy <- .69

g.9 <- esc_beta(beta = .11, sdy = 0.02 * sqrt(587), grp1n = 0.21*587, grp2n = 0.79*587, 
                es.type = "g", study = "Duncan et al. (2019)")
g.9.m.1 <- "Antisocial-sexual"; g.9.m.2 <- "Machiavellianism"; g.9.rxx <- .67; g.9.ryy <- .74

g.10 <- esc_beta(beta = .36, sdy = 0.02 * sqrt(587), grp1n = 0.21*587, grp2n = 0.79*587, 
                es.type = "g", study = "Duncan et al. (2019)")
g.10.m.1 <- "Antisocial-sexual"; g.10.m.2 <- "Psychopathy"; g.10.rxx <- .67; g.10.ryy <- .73

g.11 <- esc_beta(beta = .10, sdy = 0.02 * sqrt(587), grp1n = 0.21*587, grp2n = 0.79*587, 
                 es.type = "g", study = "Duncan et al. (2019)")
g.11.m.1 <- "Antisocial-sexual"; g.11.m.2 <- "Sadism"; g.11.rxx <- .67; g.11.ryy <- .86

### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(g.1, g.2, g.3, g.4, g.5, g.6, g.7, g.8, g.9, g.10, g.11)
temp <- cbind(temp,
              c(rep(year, 11)),
              c(g.1.m.1, g.2.m.1, g.3.m.1, g.4.m.1, g.5.m.1, g.6.m.1, g.7.m.1, g.8.m.1, g.9.m.1, 
                g.10.m.1, g.11.m.1),
              c(g.1.m.2, g.2.m.2, g.3.m.2, g.4.m.2, g.5.m.2, g.6.m.2, g.7.m.2, g.8.m.2, g.9.m.2, 
                g.10.m.2, g.11.m.2),
              c(g.1.rxx, g.2.rxx, g.3.rxx, g.4.rxx, g.5.rxx, g.6.rxx, g.7.rxx, g.8.rxx, g.9.rxx, 
                g.10.rxx, g.11.rxx),
              c(g.1.ryy, g.2.ryy, g.3.ryy, g.4.ryy, g.5.ryy, g.6.ryy, g.7.ryy, g.8.ryy, g.9.ryy, 
                g.10.ryy, g.11.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
rm(list = ls(pattern = "^g"), temp)

### Combinare efecte si construirea bazei de date a efectelor ####
r = .21; n = 587; conf.lvl = .95
g.12.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.12.se <- 1/sqrt(n - 3)
g.12.ci <- convert_r2z(r) + c(-1, 1) * 
  g.12.se * qnorm((1 + conf.lvl) / 2)
g.12.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.12.ci)), totaln = n)
g.12.study <- "Duncan et al. (2019)"
g.12.m.1 <- "Antisocial-general"; g.12.m.2 <- "Narcissism"; g.12.rxx <- .85; g.12.ryy <- .69

r = .33; n = 587; conf.lvl = .95
g.13.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.13.se <- 1/sqrt(n - 3)
g.13.ci <- convert_r2z(r) + c(-1, 1) * 
  g.13.se * qnorm((1 + conf.lvl) / 2)
g.13.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.13.ci)), totaln = n)
g.13.study <- "Duncan et al. (2019)"
g.13.m.1 <- "Antisocial-general"; g.13.m.2 <- "Machiavellianism"; g.13.rxx <- .85; g.13.ryy <- .74

r = .42; n = 587; conf.lvl = .95
g.14.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.14.se <- 1/sqrt(n - 3)
g.14.ci <- convert_r2z(r) + c(-1, 1) * 
  g.14.se * qnorm((1 + conf.lvl) / 2)
g.14.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.14.ci)), totaln = n)
g.14.study <- "Duncan et al. (2019)"
g.14.m.1 <- "Antisocial-general"; g.14.m.2 <- "Psychopathy"; g.14.rxx <- .85; g.14.ryy <- .73

r = .42; n = 587; conf.lvl = .95
g.15.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.15.se <- 1/sqrt(n - 3)
g.15.ci <- convert_r2z(r) + c(-1, 1) * 
  g.15.se * qnorm((1 + conf.lvl) / 2)
g.15.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.15.ci)), totaln = n)
g.15.study <- "Duncan et al. (2019)"
g.15.m.1 <- "Antisocial-general"; g.15.m.2 <- "Sadism"; g.15.rxx <- .85; g.15.ryy <- .86

r = .12; n = 587; conf.lvl = .95
g.16.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.16.se <- 1/sqrt(n - 3)
g.16.ci <- convert_r2z(r) + c(-1, 1) * 
  g.16.se * qnorm((1 + conf.lvl) / 2)
g.16.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.16.ci)), totaln = n)
g.16.study <- "Duncan et al. (2019)"
g.16.m.1 <- "Antisocial-esteem"; g.16.m.2 <- "Narcissism"; g.16.rxx <- .89; g.16.ryy <- .69

r = .23; n = 587; conf.lvl = .95
g.17.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.17.se <- 1/sqrt(n - 3)
g.17.ci <- convert_r2z(r) + c(-1, 1) * 
  g.17.se * qnorm((1 + conf.lvl) / 2)
g.17.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.17.ci)), totaln = n)
g.17.study <- "Duncan et al. (2019)"
g.17.m.1 <- "Antisocial-esteem"; g.17.m.2 <- "Machiavellianism"; g.17.rxx <- .89; g.17.ryy <- .74

r = .17; n = 587; conf.lvl = .95
g.18.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.18.se <- 1/sqrt(n - 3)
g.18.ci <- convert_r2z(r) + c(-1, 1) * 
  g.18.se * qnorm((1 + conf.lvl) / 2)
g.18.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.18.ci)), totaln = n)
g.18.study <- "Duncan et al. (2019)"
g.18.m.1 <- "Antisocial-esteem"; g.18.m.2 <- "Psychopathy"; g.18.rxx <- .89; g.18.ryy <- .73

r = .12; n = 587; conf.lvl = .95
g.19.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.19.se <- 1/sqrt(n - 3)
g.19.ci <- convert_r2z(r) + c(-1, 1) * 
  g.19.se * qnorm((1 + conf.lvl) / 2)
g.19.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.19.ci)), totaln = n)
g.19.study <- "Duncan et al. (2019)"
g.19.m.1 <- "Antisocial-esteem"; g.19.m.2 <- "Sadism"; g.19.rxx <- .89; g.19.ryy <- .86

r = .20; n = 587; conf.lvl = .95
g.20.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.20.se <- 1/sqrt(n - 3)
g.20.ci <- convert_r2z(r) + c(-1, 1) * 
  g.20.se * qnorm((1 + conf.lvl) / 2)
g.20.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.20.ci)), totaln = n)
g.20.study <- "Duncan et al. (2019)"
g.20.m.1 <- "Antisocial-sexual"; g.20.m.2 <- "Narcissism"; g.20.rxx <- .67; g.20.ryy <- .69

r = .35; n = 587; conf.lvl = .95
g.21.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.21.se <- 1/sqrt(n - 3)
g.21.ci <- convert_r2z(r) + c(-1, 1) * 
  g.21.se * qnorm((1 + conf.lvl) / 2)
g.21.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.21.ci)), totaln = n)
g.21.study <- "Duncan et al. (2019)"
g.21.m.1 <- "Antisocial-sexual"; g.21.m.2 <- "Machiavellianism"; g.21.rxx <- .67; g.21.ryy <- .74

r = .49; n = 587; conf.lvl = .95
g.22.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.22.se <- 1/sqrt(n - 3)
g.22.ci <- convert_r2z(r) + c(-1, 1) * 
  g.22.se * qnorm((1 + conf.lvl) / 2)
g.22.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.22.ci)), totaln = n)
g.22.study <- "Duncan et al. (2019)"
g.22.m.1 <- "Antisocial-sexual"; g.22.m.2 <- "Psychopathy"; g.22.rxx <- .67; g.22.ryy <- .73

r = .36; n = 587; conf.lvl = .95
g.23.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.23.se <- 1/sqrt(n - 3)
g.23.ci <- convert_r2z(r) + c(-1, 1) * 
  g.23.se * qnorm((1 + conf.lvl) / 2)
g.23.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.23.ci)), totaln = n)
g.23.study <- "Duncan et al. (2019)"
g.23.m.1 <- "Antisocial-sexual"; g.23.m.2 <- "Sadism"; g.23.rxx <- .67; g.23.ryy <- .86

efecte <- rbind(efecte,
                c(g.12.study, g.12.es, 1/g.12.se ^ 2, 587, g.12.se, g.12.se ^ 2, g.12.ci[1], g.12.ci[2], 
                  "g", year, g.12.m.1, g.12.m.2, g.12.rxx, g.12.ryy),
                c(g.13.study, g.13.es, 1/g.13.se ^ 2, 587, g.13.se, g.13.se ^ 2, g.13.ci[1], g.13.ci[2], 
                  "g", year, g.13.m.1, g.13.m.2, g.13.rxx, g.13.ryy),
                c(g.14.study, g.14.es, 1/g.14.se ^ 2, 587, g.14.se, g.14.se ^ 2, g.14.ci[1], g.14.ci[2], 
                  "g", year, g.14.m.1, g.14.m.2, g.14.rxx, g.14.ryy),
                c(g.15.study, g.15.es, 1/g.15.se ^ 2, 587, g.15.se, g.15.se ^ 2, g.15.ci[1], g.15.ci[2], 
                  "g", year, g.15.m.1, g.15.m.2, g.15.rxx, g.15.ryy),
                c(g.16.study, g.16.es, 1/g.16.se ^ 2, 587, g.16.se, g.16.se ^ 2, g.16.ci[1], g.16.ci[2], 
                  "g", year, g.16.m.1, g.16.m.2, g.16.rxx, g.16.ryy),
                c(g.17.study, g.17.es, 1/g.17.se ^ 2, 587, g.17.se, g.17.se ^ 2, g.17.ci[1], g.17.ci[2], 
                  "g", year, g.17.m.1, g.17.m.2, g.17.rxx, g.17.ryy),
                c(g.18.study, g.18.es, 1/g.18.se ^ 2, 587, g.18.se, g.18.se ^ 2, g.18.ci[1], g.18.ci[2], 
                  "g", year, g.18.m.1, g.18.m.2, g.18.rxx, g.18.ryy),
                c(g.19.study, g.19.es, 1/g.19.se ^ 2, 587, g.19.se, g.19.se ^ 2, g.19.ci[1], g.19.ci[2], 
                  "g", year, g.19.m.1, g.19.m.2, g.19.rxx, g.19.ryy),
                c(g.20.study, g.20.es, 1/g.20.se ^ 2, 587, g.20.se, g.20.se ^ 2, g.20.ci[1], g.20.ci[2], 
                  "g", year, g.20.m.1, g.20.m.2, g.20.rxx, g.20.ryy),
                c(g.21.study, g.21.es, 1/g.21.se ^ 2, 587, g.21.se, g.21.se ^ 2, g.21.ci[1], g.21.ci[2], 
                  "g", year, g.21.m.1, g.21.m.2, g.21.rxx, g.21.ryy),
                c(g.22.study, g.22.es, 1/g.22.se ^ 2, 587, g.22.se, g.22.se ^ 2, g.22.ci[1], g.22.ci[2], 
                  "g", year, g.22.m.1, g.22.m.2, g.22.rxx, g.22.ryy),
                c(g.23.study, g.23.es, 1/g.23.se ^ 2, 587, g.23.se, g.23.se ^ 2, g.23.ci[1], g.23.ci[2], 
                  "g", year, g.23.m.1, g.23.m.2, g.23.rxx, g.23.ryy))
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), n, conf.lvl, r, year)

## *** Studiul ref_159 - CODARE ####
## Ferenczi N., Marshall T.C., Bejanyan K. (2017) 
## Are sex differences in antisocial and prosocial Facebook use explained by narcissism and relational self-construal?
eliminate <- eliminate + 1
# Eliminat deoarece nu contine masuri ale cyberbullying-ului
## *** Studiul ref_195 - CODARE ####
## Hussain Z., Wegmann E., Grifths M.D. (2021) 
## The association between problematic  social networking site use, dark triad traits,and emotion dysregulation
eliminate <- eliminate + 1
# Eliminat deoarece nu contine masuri ale cyberbullying-ului
## *** Studiul ref_216 - CODARE ####
eliminate <- eliminate + 1
# Eliminat deoarece nu contine masuri ale cyberbullying-ului
## 14. Studiul ref_221 - CODARE ####
## Kircaburun K., Jonason P. Griffiths M.D., Aslanargun E., Emirtekin E., Tosuntas S.B., Billieux J. (2019)
## Childhood Emotional Abuse and Cyberbullying Perpetration: The Role of Dark Personality Traits
year <- tabel.surse[which(tabel.surse$label == "ref_221"),]$year
r = .46; n = 587; conf.lvl = .95
g.1.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.1.se <- 1/sqrt(n - 3)
g.1.ci <- convert_r2z(r) + c(-1, 1) * 
  g.1.se * qnorm((1 + conf.lvl) / 2)
g.1.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.1.ci)), totaln = n)
g.1.study <- "Kircaburun et al. (2019)"
g.1.m.1 <- "Cyberbullying"; g.1.m.2 <- "Machiavellianism"; g.1.rxx <- .79; g.1.ryy <- .81

r = .41; n = 587; conf.lvl = .95
g.2.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.2.se <- 1/sqrt(n - 3)
g.2.ci <- convert_r2z(r) + c(-1, 1) * 
  g.2.se * qnorm((1 + conf.lvl) / 2)
g.2.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.2.ci)), totaln = n)
g.2.study <- "Kircaburun et al. (2019)"
g.2.m.1 <- "Cyberbullying"; g.2.m.2 <- "Psychopathy"; g.2.rxx <- .79; g.2.ryy <- .67

r = .30; n = 587; conf.lvl = .95
g.3.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.3.se <- 1/sqrt(n - 3)
g.3.ci <- convert_r2z(r) + c(-1, 1) * 
  g.3.se * qnorm((1 + conf.lvl) / 2)
g.3.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.3.ci)), totaln = n)
g.3.study <- "Kircaburun et al. (2019)"
g.3.m.1 <- "Cyberbullying"; g.3.m.2 <- "Narcissism"; g.3.rxx <- .79; g.3.ryy <- .88

r = .43; n = 587; conf.lvl = .95
g.4.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.4.se <- 1/sqrt(n - 3)
g.4.ci <- convert_r2z(r) + c(-1, 1) * 
  g.4.se * qnorm((1 + conf.lvl) / 2)
g.4.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.4.ci)), totaln = n)
g.4.study <- "Kircaburun et al. (2019)"
g.4.m.1 <- "Cyberbullying"; g.4.m.2 <- "Sadism"; g.4.rxx <- .79; g.4.ryy <- .77

efecte <- rbind(efecte,
                c(g.1.study, g.1.es, 1/g.1.se ^ 2, 139, g.1.se, g.1.se ^ 2, g.1.ci[1], g.1.ci[2], 
                  "g", year, g.1.m.1, g.1.m.2, g.1.rxx, g.1.ryy),
                c(g.2.study, g.2.es, 1/g.2.se ^ 2, 139, g.2.se, g.2.se ^ 2, g.2.ci[1], g.2.ci[2], 
                  "g", year, g.2.m.1, g.2.m.2, g.2.rxx, g.2.ryy),
                c(g.3.study, g.3.es, 1/g.3.se ^ 2, 139, g.3.se, g.3.se ^ 2, g.3.ci[1], g.3.ci[2], 
                  "g", year, g.3.m.1, g.3.m.2, g.3.rxx, g.3.ryy),
                c(g.4.study, g.4.es, 1/g.4.se ^ 2, 139, g.4.se, g.4.se ^ 2, g.4.ci[1], g.4.ci[2], 
                  "g", year, g.4.m.1, g.4.m.2, g.4.rxx, g.4.ryy))
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), n, conf.lvl, r, year)

## *** Studiul ref_224 - CODARE ####
## Kircaburun K., Griffiths M.D. (2018)
## The dark side of internet: Preliminary evidence for the associations of dark personality traits with specific online activities and problematic internet use 
eliminate <- eliminate + 1
# Eliminat deoarece nu contine masuri ale cyberbullying-ului


## 15. Studiul ref_244 - CODARE ####
## March E., Grieve R., Marrington J., Jonason P.K. (2017)
## Trolling on Tinder (and other dating apps): Examining the role of the Dark Tetrad and impulsivity
year <- tabel.surse[which(tabel.surse$label == "ref_244"),]$year
r = .11; n = 357; conf.lvl = .95
g.1.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.1.se <- 1/sqrt(n - 3)
g.1.ci <- convert_r2z(r) + c(-1, 1) * 
  g.1.se * qnorm((1 + conf.lvl) / 2)
g.1.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.1.ci)), totaln = n)
g.1.study <- "March et al. (2017)"
g.1.m.1 <- "Trolling"; g.1.m.2 <- "Narcissism"; g.1.rxx <- .74; g.1.ryy <- .80

r = .20; n = 357; conf.lvl = .95
g.2.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.2.se <- 1/sqrt(n - 3)
g.2.ci <- convert_r2z(r) + c(-1, 1) * 
  g.2.se * qnorm((1 + conf.lvl) / 2)
g.2.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.2.ci)), totaln = n)
g.2.study <- "March et al. (2017)"
g.2.m.1 <- "Trolling"; g.2.m.2 <- "Machiavellianism"; g.2.rxx <- .74; g.2.ryy <- .77

r = .32; n = 357; conf.lvl = .95
g.3.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.3.se <- 1/sqrt(n - 3)
g.3.ci <- convert_r2z(r) + c(-1, 1) * 
  g.3.se * qnorm((1 + conf.lvl) / 2)
g.3.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.3.ci)), totaln = n)
g.3.study <- "March et al. (2017)"
g.3.m.1 <- "Trolling"; g.3.m.2 <- "Psychopathy"; g.3.rxx <- .74; g.3.ryy <- .73

r = .25; n = 357; conf.lvl = .95
g.4.es <- hedges_g(d = cohens_d(r = r), totaln = n)
g.4.se <- 1/sqrt(n - 3)
g.4.ci <- convert_r2z(r) + c(-1, 1) * 
  g.4.se * qnorm((1 + conf.lvl) / 2)
g.4.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  g.4.ci)), totaln = n)
g.4.study <- "March et al. (2017)"
g.4.m.1 <- "Trolling"; g.4.m.2 <- "Sadism"; g.4.rxx <- .74; g.4.ryy <- .84
### Combinare efecte si construirea bazei de date a efectelor ####
efecte <- rbind(efecte,
                c(g.1.study, g.1.es, 1/g.1.se ^ 2, 357, g.1.se, g.1.se ^ 2, g.1.ci[1], g.1.ci[2], 
                  "g", year, g.1.m.1, g.1.m.2, g.1.rxx, g.1.ryy),
                c(g.2.study, g.2.es, 1/g.2.se ^ 2, 357, g.2.se, g.2.se ^ 2, g.2.ci[1], g.2.ci[2], 
                  "g", year, g.2.m.1, g.2.m.2, g.2.rxx, g.2.ryy),
                c(g.3.study, g.3.es, 1/g.3.se ^ 2, 357, g.3.se, g.3.se ^ 2, g.3.ci[1], g.3.ci[2], 
                  "g", year, g.3.m.1, g.3.m.2, g.3.rxx, g.3.ryy),
                c(g.4.study, g.4.es, 1/g.4.se ^ 2, 357, g.4.se, g.4.se ^ 2, g.4.ci[1], g.4.ci[2], 
                  "g", year, g.4.m.1, g.4.m.2, g.4.rxx, g.4.ryy))
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), n, conf.lvl, r)

t.val <- qt(0.011 / 2, df = 351); beta <- 0.17
g.5 <- esc_beta(beta = beta, sdy = beta / abs(t.val), grp1n = 0.29 * 357, grp2n = 0.71 * 357,
                es.type = "g", study = "March et al. (2017)")
g.5.m.1 <- "Trolling"; g.5.m.2 <- "Psychopathy"; g.5.rxx <- .74; g.5.ryy <- .73

t.val <- qt(0.015 / 2, df = 351); beta <- 0.16
g.6 <- esc_beta(beta = beta, sdy = beta / abs(t.val), grp1n = 0.29 * 357, grp2n = 0.71 * 357,
                es.type = "g", study = "March et al. (2017)")
g.6.m.1 <- "Trolling"; g.6.m.2 <- "Sadism"; g.6.rxx <- .74; g.6.ryy <- .84

t.val <- qt(0.280 / 2, df = 351); beta <- 0.06
g.7 <- esc_beta(beta = beta, sdy = beta / abs(t.val), grp1n = 0.29 * 357, grp2n = 0.71 * 357,
                es.type = "g", study = "March et al. (2017)")
g.7.m.1 <- "Trolling"; g.7.m.2 <- "Narcissism"; g.7.rxx <- .74; g.7.ryy <- .80

t.val <- qt(0.284 / 2, df = 351); beta <- 0.06
g.8 <- esc_beta(beta = beta, sdy = beta / abs(t.val), grp1n = 0.29 * 357, grp2n = 0.71 * 357,
                es.type = "g", study = "March et al. (2017)")
g.8.m.1 <- "Trolling"; g.8.m.2 <- "Machiavellianism"; g.8.rxx <- .74; g.8.ryy <- .77
### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(g.5, g.6, g.7, g.8)
temp <- cbind(temp,
              c(rep(year, 4)),
              c(g.5.m.1, g.6.m.1, g.7.m.1, g.8.m.1),
              c(g.5.m.2, g.6.m.2, g.7.m.2, g.8.m.2),
              c(g.5.rxx, g.6.rxx, g.7.rxx, g.8.rxx),
              c(g.5.ryy, g.6.ryy, g.7.ryy, g.8.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
rm(list = ls(pattern = "^g"), temp, year, beta, t.val)


## 21. Studiul ref_245 - CODARE ####
year <- tabel.surse[which(tabel.surse$label == "ref_245"),]$year

## 22. Studiul ref_249 - CODARE ####
year <- tabel.surse[which(tabel.surse$label == "ref_249"),]$year

## 23. Studiul ref_254 - CODARE ####
year <- tabel.surse[which(tabel.surse$label == "ref_254"),]$year

## 24. Studiul ref_289 - CODARE ####
year <- tabel.surse[which(tabel.surse$label == "ref_289"),]$year

## 25. Studiul ref_290 - CODARE ####
year <- tabel.surse[which(tabel.surse$label == "ref_290"),]$year

## 26. Studiul ref_311 - CODARE ####
year <- tabel.surse[which(tabel.surse$label == "ref_311"),]$year

## 27. Studiul ref_323 - CODARE ####
year <- tabel.surse[which(tabel.surse$label == "ref_323"),]$year

## 28. Studiul ref_360 - CODARE ####
year <- tabel.surse[which(tabel.surse$label == "ref_360"),]$year

## 29. Studiul ref_366 - CODARE ####
year <- tabel.surse[which(tabel.surse$label == "ref_366"),]$year

## Salvarea finala a datelor si actualizarea PRISMA
save(efecte, file = "Efecte.RData")

