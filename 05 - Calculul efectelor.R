# Incarcarea pachetelor necesare
if (!require(esc)) install.packages("esc")
library(esc)

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
# M - Machiavelism; N - Narcisism; P - Psihopatie
# 1 - Masculin; 2 (0) - Feminin
load("Centralizator.Rdata")
## 1. Studiul ref_004 - CODARE ####
## H. M. Baughman, S. Dearing, E. Giammarco, P. A. Vernon (2012) - 
## Relationships between bullying behaviours and the Dark Triad: A study with adults
year <- tabel.surse[which(tabel.surse$label == "ref_004"),]$year
gI.1 <- esc_mean_sd(grp1m = 3.42, grp1sd = 0.60, grp1n = 203, 
                    grp2m = 3.14, grp2sd = 0.54, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012)")
gI.1.m.1 <- "Machiavellianism"; gI.1.m.2 <- "Gender"; gI.1.rxx <- .73; gI.1.ryy <- NA

gI.2 <- esc_mean_sd(grp1m = 3.07, grp1sd = 0.58, grp1n = 203, 
                    grp2m = 2.86, grp2sd = 0.51, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012)")
gI.2.m.1 <- "Narcissism"; gI.2.m.2 <- "Gender"; gI.2.rxx <- .71; gI.2.ryy <- NA

gI.3 <- esc_mean_sd(grp1m = 2.37, grp1sd = 0.69, grp1n = 203, 
                    grp2m = 2.04, grp2sd = 0.53, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012)")
gI.3.m.1 <- "Psychopathy"; gI.3.m.2 <- "Gender"; gI.3.rxx <- .78; gI.3.ryy <- NA

gI.4 <- esc_mean_sd(grp1m = 0.46, grp1sd = 0.52, grp1n = 203, 
                    grp2m = 0.34, grp2sd = 0.43, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012)")
gI.4.m.1 <- "Bullying Physical Direct"; gI.4.m.2 <- "Gender"; gI.4.rxx <- .69; gI.4.ryy <- NA

gI.5 <- esc_mean_sd(grp1m = 0.85, grp1sd = 0.68, grp1n = 203, 
                    grp2m = 0.69, grp2sd = 0.52, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012)")
gI.5.m.1 <- "Bullying Verbal Direct"; gI.5.m.2 <- "Gender"; gI.5.rxx <- .83; gI.5.ryy <- NA

gI.6 <- esc_mean_sd(grp1m = 0.72, grp1sd = 0.61, grp1n = 203, 
                    grp2m = 0.56, grp2sd = 0.45, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012)")
gI.6.m.1 <- "Bullying Direct"; gI.6.m.2 <- "Gender"; gI.6.rxx <- .86; gI.6.ryy <- NA

gI.7 <- esc_mean_sd(grp1m = 0.54, grp1sd = 0.55, grp1n = 203, 
                    grp2m = 0.46, grp2sd = 0.43, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012)")
gI.7.m.1 <- "Bullying Indirect"; gI.7.m.2 <- "Gender"; gI.7.rxx <- .76; gI.7.ryy <- NA

gI.8 <- esc_mean_sd(grp1m = 0.66, grp1sd = 0.56, grp1n = 203, 
                    grp2m = 0.52, grp2sd = 0.41, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012)")
gI.8.m.1 <- "Bullying Total"; gI.8.m.2 <- "Gender"; gI.8.rxx <- .89; gI.8.ryy <- NA

### Combinare efecte si construirea bazei de date a efectelor ####
efecte <- combine_esc(gI.1, gI.2, gI.3, gI.4, gI.5, gI.6, gI.7, gI.8)
efecte <- cbind(efecte,
              c(rep(year, 8)),
              c(gI.1.m.1, gI.2.m.1, gI.3.m.1, gI.4.m.1, gI.5.m.1, gI.6.m.1, gI.7.m.1, gI.8.m.1),
              c(gI.1.m.2, gI.2.m.2, gI.3.m.2, gI.4.m.2, gI.5.m.2, gI.6.m.2, gI.7.m.2, gI.8.m.2),
              c(gI.1.rxx, gI.2.rxx, gI.3.rxx, gI.4.rxx, gI.5.rxx, gI.6.rxx, gI.7.rxx, gI.8.rxx),
              c(gI.1.ryy, gI.2.ryy, gI.3.ryy, gI.4.ryy, gI.5.ryy, gI.6.ryy, gI.7.ryy, gI.8.ryy))
colnames(efecte) <- c(names(efecte[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"))

r = .35; n = 657; conf.lvl = .95
gI.9.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.9.se <- 1/sqrt(n - 3)
gI.9.ci <- convert_r2z(r) + c(-1, 1) * 
  gI.9.se * qnorm((1 + conf.lvl) / 2)
gI.9.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.9.ci)), totaln = n)
gI.9.study <- "Baughman et al. (2012)"
gI.9.m.1 <- "Bullying Total"; gI.9.m.2 <- "Machiavellianism"; gI.9.rxx <- .89; gI.9.ryy <- .73

r = .22; n = 657; conf.lvl = .95
gI.10.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.10.se <- 1/sqrt(n - 3)
gI.10.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.10.se * qnorm((1 + conf.lvl) / 2)
gI.10.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.10.ci)), totaln = n)
gI.10.study <- "Baughman et al. (2012)"
gI.10.m.1 <- "Bullying Total"; gI.10.m.2 <- "Narcissism"; gI.10.rxx <- .89; gI.10.ryy <- .71

r = .55; n = 657; conf.lvl = .95
gI.11.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.11.se <- 1/sqrt(n - 3)
gI.11.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.11.se * qnorm((1 + conf.lvl) / 2)
gI.11.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.11.ci)), totaln = n)
gI.11.study <- "Baughman et al. (2012)"
gI.11.m.1 <- "Bullying Total"; gI.11.m.2 <- "Psychopathy"; gI.11.rxx <- .89; gI.11.ryy <- .78

r = .33; n = 657; conf.lvl = .95
gI.12.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.12.se <- 1/sqrt(n - 3)
gI.12.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.12.se * qnorm((1 + conf.lvl) / 2)
gI.12.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.12.ci)), totaln = n)
gI.12.study <- "Baughman et al. (2012)"
gI.12.m.1 <- "Bullying Indirect"; gI.12.m.2 <- "Machiavellianism"; gI.12.rxx <- .76; gI.12.ryy <- .73

r = .21; n = 657; conf.lvl = .95
gI.13.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.13.se <- 1/sqrt(n - 3)
gI.13.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.13.se * qnorm((1 + conf.lvl) / 2)
gI.13.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.13.ci)), totaln = n)
gI.13.study <- "Baughman et al. (2012)"
gI.13.m.1 <- "Bullying Indirect"; gI.13.m.2 <- "Narcissism"; gI.13.rxx <- .76; gI.13.ryy <- .71

r = .49; n = 657; conf.lvl = .95
gI.14.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.14.se <- 1/sqrt(n - 3)
gI.14.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.14.se * qnorm((1 + conf.lvl) / 2)
gI.14.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.14.ci)), totaln = n)
gI.14.study <- "Baughman et al. (2012)"
gI.14.m.1 <- "Bullying Indirect"; gI.14.m.2 <- "Psychopathy"; gI.14.rxx <- .76; gI.14.ryy <- .78

r = .34; n = 657; conf.lvl = .95
gI.15.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.15.se <- 1/sqrt(n - 3)
gI.15.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.15.se * qnorm((1 + conf.lvl) / 2)
gI.15.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.15.ci)), totaln = n)
gI.15.study <- "Baughman et al. (2012)"
gI.15.m.1 <- "Bullying Direct"; gI.15.m.2 <- "Machiavellianism"; gI.15.rxx <- .86; gI.15.ryy <- .73

r = .20; n = 657; conf.lvl = .95
gI.16.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.16.se <- 1/sqrt(n - 3)
gI.16.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.16.se * qnorm((1 + conf.lvl) / 2)
gI.16.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.16.ci)), totaln = n)
gI.16.study <- "Baughman et al. (2012)"
gI.16.m.1 <- "Bullying Direct"; gI.16.m.2 <- "Narcissism"; gI.16.rxx <- .86; gI.16.ryy <- .71

r = .53; n = 657; conf.lvl = .95
gI.17.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.17.se <- 1/sqrt(n - 3)
gI.17.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.17.se * qnorm((1 + conf.lvl) / 2)
gI.17.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.17.ci)), totaln = n)
gI.17.study <- "Baughman et al. (2012)"
gI.17.m.1 <- "Bullying Direct"; gI.17.m.2 <- "Psychopathy"; gI.17.rxx <- .86; gI.17.ryy <- .78

r = .35; n = 657; conf.lvl = .95
gI.18.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.18.se <- 1/sqrt(n - 3)
gI.18.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.18.se * qnorm((1 + conf.lvl) / 2)
gI.18.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.18.ci)), totaln = n)
gI.18.study <- "Baughman et al. (2012)"
gI.18.m.1 <- "Bullying Verbal Direct"; gI.18.m.2 <- "Machiavellianism"; gI.18.rxx <- .83; gI.18.ryy <- .73

r = .20; n = 657; conf.lvl = .95
gI.19.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.19.se <- 1/sqrt(n - 3)
gI.19.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.19.se * qnorm((1 + conf.lvl) / 2)
gI.19.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.19.ci)), totaln = n)
gI.19.study <- "Baughman et al. (2012)"
gI.19.m.1 <- "Bullying Verbal Direct"; gI.19.m.2 <- "Narcissism"; gI.19.rxx <- .83; gI.19.ryy <- .71

r = .51; n = 657; conf.lvl = .95
gI.20.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.20.se <- 1/sqrt(n - 3)
gI.20.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.20.se * qnorm((1 + conf.lvl) / 2)
gI.20.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.20.ci)), totaln = n)
gI.20.study <- "Baughman et al. (2012)"
gI.20.m.1 <- "Bullying Verbal Direct"; gI.20.m.2 <- "Psychopathy"; gI.20.rxx <- .83; gI.20.ryy <- .78

r = .22; n = 657; conf.lvl = .95
gI.21.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.21.se <- 1/sqrt(n - 3)
gI.21.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.21.se * qnorm((1 + conf.lvl) / 2)
gI.21.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.21.ci)), totaln = n)
gI.21.study <- "Baughman et al. (2012)"
gI.21.m.1 <- "Bullying Physical Direct"; gI.21.m.2 <- "Machiavellianism"; gI.21.rxx <- .69; gI.21.ryy <- .73

r = .11; n = 657; conf.lvl = .95
gI.22.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.22.se <- 1/sqrt(n - 3)
gI.22.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.22.se * qnorm((1 + conf.lvl) / 2)
gI.22.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.22.ci)), totaln = n)
gI.22.study <- "Baughman et al. (2012)"
gI.22.m.1 <- "Bullying Physical Direct"; gI.22.m.2 <- "Narcissism"; gI.22.rxx <- .69; gI.22.ryy <- .71

r = .41; n = 657; conf.lvl = .95
gI.23.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.23.se <- 1/sqrt(n - 3)
gI.23.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.23.se * qnorm((1 + conf.lvl) / 2)
gI.23.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.23.ci)), totaln = n)
gI.23.study <- "Baughman et al. (2012)"
gI.23.m.1 <- "Bullying Physical Direct"; gI.23.m.2 <- "Psychopathy"; gI.23.rxx <- .69; gI.23.ryy <- .78

### Combinare efecte si construirea bazei de date a efectelor ####
efecte <- rbind(efecte,
              c(gI.9.study, gI.9.es, 1/gI.9.se ^ 2, 657, gI.9.se, gI.9.se ^ 2, gI.9.ci[1], gI.9.ci[2], 
                "g", year, gI.9.m.1, gI.9.m.2, gI.9.rxx, gI.9.ryy),
              c(gI.10.study, gI.10.es, 1/gI.10.se ^ 2, 657, gI.10.se, gI.10.se ^ 2, gI.10.ci[1], gI.10.ci[2], 
                "g", year, gI.10.m.1, gI.10.m.2, gI.10.rxx, gI.10.ryy),
              c(gI.11.study, gI.11.es, 1/gI.11.se ^ 2, 657, gI.11.se, gI.11.se ^ 2, gI.11.ci[1], gI.11.ci[2], 
                "g", year, gI.11.m.1, gI.11.m.2, gI.11.rxx, gI.11.ryy),
              c(gI.12.study, gI.12.es, 1/gI.12.se ^ 2, 657, gI.12.se, gI.12.se ^ 2, gI.12.ci[1], gI.12.ci[2], 
                "g", year, gI.12.m.1, gI.12.m.2, gI.12.rxx, gI.12.ryy),
              c(gI.13.study, gI.13.es, 1/gI.13.se ^ 2, 657, gI.13.se, gI.13.se ^ 2, gI.13.ci[1], gI.13.ci[2], 
                "g", year, gI.13.m.1, gI.13.m.2, gI.13.rxx, gI.13.ryy),
              c(gI.14.study, gI.14.es, 1/gI.14.se ^ 2, 657, gI.14.se, gI.14.se ^ 2, gI.14.ci[1], gI.14.ci[2], 
                "g", year, gI.14.m.1, gI.14.m.2, gI.14.rxx, gI.14.ryy),
              c(gI.15.study, gI.15.es, 1/gI.15.se ^ 2, 657, gI.15.se, gI.15.se ^ 2, gI.15.ci[1], gI.15.ci[2],
                "g", year, gI.15.m.1, gI.15.m.2, gI.15.rxx, gI.15.ryy),
              c(gI.16.study, gI.16.es, 1/gI.16.se ^ 2, 657, gI.16.se, gI.16.se ^ 2, gI.16.ci[1], gI.16.ci[2], 
                "g", year, gI.16.m.1, gI.16.m.2, gI.16.rxx, gI.16.ryy),
              c(gI.17.study, gI.17.es, 1/gI.17.se ^ 2, 657, gI.17.se, gI.17.se ^ 2, gI.17.ci[1], gI.17.ci[2], 
                "g", year, gI.17.m.1, gI.17.m.2, gI.17.rxx, gI.17.ryy),
              c(gI.18.study, gI.18.es, 1/gI.18.se ^ 2, 657, gI.18.se, gI.18.se ^ 2, gI.18.ci[1], gI.18.ci[2], 
                "g", year, gI.18.m.1, gI.18.m.2, gI.18.rxx, gI.18.ryy),
              c(gI.19.study, gI.19.es, 1/gI.19.se ^ 2, 657, gI.19.se, gI.19.se ^ 2, gI.19.ci[1], gI.19.ci[2], 
                "g", year, gI.19.m.1, gI.19.m.2, gI.19.rxx, gI.19.ryy),
              c(gI.20.study, gI.20.es, 1/gI.20.se ^ 2, 657, gI.20.se, gI.20.se ^ 2, gI.20.ci[1], gI.20.ci[2], 
                "g", year, gI.20.m.1, gI.20.m.2, gI.20.rxx, gI.20.ryy),
              c(gI.21.study, gI.21.es, 1/gI.21.se ^ 2, 657, gI.21.se, gI.21.se ^ 2, gI.21.ci[1], gI.21.ci[2], 
                "g", year, gI.21.m.1, gI.21.m.2, gI.21.rxx, gI.21.ryy),
              c(gI.22.study, gI.22.es, 1/gI.22.se ^ 2, 657, gI.22.se, gI.22.se ^ 2, gI.22.ci[1], gI.22.ci[2], 
                "g", year, gI.22.m.1, gI.22.m.2, gI.22.rxx, gI.22.ryy),
              c(gI.23.study, gI.23.es, 1/gI.23.se ^ 2, 657, gI.23.se, gI.23.se ^ 2, gI.23.ci[1], gI.23.ci[2], 
                "g", year, gI.23.m.1, gI.23.m.2, gI.23.rxx, gI.23.ryy))
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), n, conf.lvl, r, year)

## 2. Studiul ref_006 - CODARE ####
## O. Bogolyubova, P. Panicheva, R. Tikhonov, V. Ivanov, Y. Ledovaya (2018) 
## Dark personalities on Facebook: Harmful online behaviors and language
year <- tabel.surse[which(tabel.surse$label == "ref_006"),]$year
gII.1 <- esc_mean_sd(grp1m = 3.43, grp1sd = 0.63, grp1n = 1487, 
                     grp2m = 3.16, grp2sd = 0.64, grp2n = 5237, 
                     es.type = "g", study = "Bogolyubova et al. (2018)")
gII.1.m.1 <- "Machiavellianism"; gII.1.m.2 <- "Gender"; gII.1.rxx <- .72; gII.1.ryy <- NA

gII.2 <- esc_mean_sd(grp1m = 3.00, grp1sd = 0.63, grp1n = 1487, 
                     grp2m = 3.04, grp2sd = 0.53, grp2n = 5237, 
                     es.type = "g", study = "Bogolyubova et al. (2018)")
gII.2.m.1 <- "Narcissism"; gII.2.m.2 <- "Gender"; gII.2.rxx <- .72; gII.2.ryy <- NA

gII.3 <- esc_mean_sd(grp1m = 2.30, grp1sd = 0.59, grp1n = 1487, 
                     grp2m = 1.89, grp2sd = 0.53, grp2n = 5237, 
                     es.type = "g", study = "Bogolyubova et al. (2018)")
gII.3.m.1 <- "Psychopathy"; gII.3.m.2 <- "Gender"; gII.3.rxx <- .72; gII.3.ryy <- NA

gII.4 <- esc_beta(beta = 0.62, sdy = 0.07 * sqrt(6724), grp1n = 1487, grp2n = 5237, 
                  es.type = "g", study = "Bogolyubova et al. (2018)")
gII.4.m.1 <- "Harmfull"; gII.4.m.2 <- "Gender"; gII.4.rxx <- NA; gII.4.ryy <- NA

gII.5 <- esc_beta(beta = 1.00, sdy = 0.06 * sqrt(6724), grp1n = 6724/2, grp2n = 6724/2, 
                  es.type = "g", study = "Bogolyubova et al. (2018)")
gII.5.m.1 <- "Harmfull"; gII.5.m.2 <- "Psychopathy"; gII.5.rxx <- NA; gII.5.ryy <- .72

gII.6 <- esc_beta(beta = 0.01, sdy = 0.05 * sqrt(6724), grp1n = 6724/2, grp2n = 6724/2, 
                  es.type = "g", study = "Bogolyubova et al. (2018)")
gII.6.m.1 <- "Harmfull"; gII.6.m.2 <- "Machiavellianism"; gII.6.rxx <- NA; gII.6.ryy <- .72

gII.7 <- esc_beta(beta = 0.01, sdy = 0.05 * sqrt(6724), grp1n = 6724/2, grp2n = 6724/2, 
                  es.type = "g", study = "Bogolyubova et al. (2018)")
gII.7.m.1 <- "Harmfull"; gII.7.m.2 <- "Narcissism"; gII.7.rxx <- NA; gII.7.ryy <- .72

### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(gII.1, gII.2, gII.3, gII.4, gII.5, gII.6, gII.7)
temp <- cbind(temp,
              c(rep(year, 7)),
              c(gII.1.m.1, gII.2.m.1, gII.3.m.1, gII.4.m.1, gII.5.m.1, gII.6.m.1, gII.7.m.1),
              c(gII.1.m.2, gII.2.m.2, gII.3.m.2, gII.4.m.2, gII.5.m.2, gII.6.m.2, gII.7.m.2),
              c(gII.1.rxx, gII.2.rxx, gII.3.rxx, gII.4.rxx, gII.5.rxx, gII.6.rxx, gII.7.rxx),
              c(gII.1.ryy, gII.2.ryy, gII.3.ryy, gII.4.ryy, gII.5.ryy, gII.6.ryy, gII.7.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), year, temp)

## 3. Studiul ref_012 - CODARE ####
## N. Craker, E. March (2016)
## The dark side of Facebook: The Dark Tetrad, negative social potency, and trolling behaviours
year <- tabel.surse[which(tabel.surse$label == "ref_012"),]$year
gIII.1 <- esc_mean_sd(grp1m = 15.86, grp1sd = 4.58, grp1n = 87, 
                      grp2m = 12.52, grp2sd = 4.00, grp2n = 292, 
                      es.type = "g", study = "Craker et al. (2016)")
gIII.1.m.1 <- "Trolling"; gIII.1.m.2 <- "Gender"; gIII.1.rxx <- .70; gIII.1.ryy <- NA

gIII.2 <- esc_mean_sd(grp1m = 1.32, grp1sd = 1.58, grp1n = 77, 
                      grp2m = 0.68, grp2sd = 0.99, grp2n = 266, 
                      es.type = "g", study = "Craker et al. (2016)")
gIII.2.m.1 <- "Sadism"; gIII.2.m.2 <- "Gender"; gIII.2.rxx <- .58; gIII.2.ryy <- NA

gIII.3 <- esc_mean_sd(grp1m = 16.44, grp1sd = 6.89, grp1n = 78, 
                      grp2m = 13.09, grp2sd = 6.16, grp2n = 269, 
                      es.type = "g", study = "Craker et al. (2016)")
gIII.3.m.1 <- "Machiavellianism"; gIII.3.m.2 <- "Gender"; gIII.3.rxx <- .80; gIII.3.ryy <- NA

gIII.4 <- esc_mean_sd(grp1m = 13.62, grp1sd = 6.82, grp1n = 78, 
                      grp2m = 9.76, grp2sd = 4.97, grp2n = 269, 
                      es.type = "g", study = "Craker et al. (2016)")
gIII.4.m.1 <- "Psychopathy"; gIII.4.m.2 <- "Gender"; gIII.4.rxx <- .75; gIII.4.ryy <- NA

gIII.5 <- esc_mean_sd(grp1m = 19.73, grp1sd = 6.88, grp1n = 78, 
                      grp2m = 17.48, grp2sd = 7.01, grp2n = 269, 
                      es.type = "g", study = "Craker et al. (2016)")
gIII.5.m.1 <- "Narcissism"; gIII.5.m.2 <- "Gender"; gIII.5.rxx <- .82; gIII.5.ryy <- NA

gIII.6 <- esc_beta(beta = -0.06, sdy = 0.02 * sqrt(396), grp1n = 396/2, grp2n = 396/2,
                  es.type = "g", study = "Craker et al. (2016)")
gIII.6.m.1 <- "Trolling"; gIII.6.m.2 <- "Age"; gIII.6.rxx <- .70; gIII.6.ryy <- NA

gIII.7 <- esc_beta(beta = -0.19, sdy = 0.51 * sqrt(396), grp1n = 94, grp2n = 296,
                   es.type = "g", study = "Craker et al. (2016)")
gIII.7.m.1 <- "Trolling"; gIII.7.m.2 <- "Gender"; gIII.7.rxx <- .70; gIII.7.ryy <- NA

gIII.8 <- esc_beta(beta = 0.07, sdy = 0.21 * sqrt(396), grp1n = 396/2, grp2n = 396/2,
                   es.type = "g", study = "Craker et al. (2016)")
gIII.8.m.1 <- "Trolling"; gIII.8.m.2 <- "Sadism"; gIII.8.rxx <- .70; gIII.8.ryy <- .58

gIII.9 <- esc_beta(beta = -0.00, sdy = 0.05 * sqrt(396), grp1n = 396/2, grp2n = 396/2,
                   es.type = "g", study = "Craker et al. (2016)")
gIII.9.m.1 <- "Trolling"; gIII.9.m.2 <- "Machiavellianism"; gIII.9.rxx <- .70; gIII.9.ryy <- .80

gIII.10 <- esc_beta(beta = 0.06, sdy = 0.05 * sqrt(396), grp1n = 396/2, grp2n = 396/2,
                   es.type = "g", study = "Craker et al. (2016)")
gIII.10.m.1 <- "Trolling"; gIII.10.m.2 <- "Psychopathy"; gIII.10.rxx <- .70; gIII.10.ryy <- .75

gIII.11 <- esc_beta(beta = -0.01, sdy = 0.03 * sqrt(396), grp1n = 396/2, grp2n = 396/2,
                    es.type = "g", study = "Craker et al. (2016)")
gIII.11.m.1 <- "Trolling"; gIII.11.m.2 <- "Narcissism"; gIII.11.rxx <- .70; gIII.11.ryy <- .82
### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(gIII.1, gIII.2, gIII.3, gIII.4, gIII.5, gIII.6, gIII.7, gIII.8, gIII.9,
                    gIII.10,gIII.11)
temp <- cbind(temp,
              c(rep(year, 11)),
              c(gIII.1.m.1, gIII.2.m.1, gIII.3.m.1, gIII.4.m.1, gIII.5.m.1, gIII.6.m.1,
                gIII.7.m.1, gIII.8.m.1, gIII.9.m.1, gIII.10.m.1, gIII.11.m.1),
              c(gIII.1.m.2, gIII.2.m.2, gIII.3.m.2, gIII.4.m.2, gIII.5.m.2, gIII.6.m.2,
                gIII.7.m.2, gIII.8.m.2, gIII.9.m.2, gIII.10.m.2, gIII.11.m.2),
              c(gIII.1.rxx, gIII.2.rxx, gIII.3.rxx, gIII.4.rxx, gIII.5.rxx, gIII.6.rxx, 
                gIII.7.rxx, gIII.8.rxx, gIII.9.rxx, gIII.10.rxx, gIII.11.rxx),
              c(gIII.1.ryy, gIII.2.ryy, gIII.3.ryy, gIII.4.ryy, gIII.5.ryy, gIII.6.ryy,
                gIII.7.ryy, gIII.8.ryy, gIII.9.ryy, gIII.10.ryy, gIII.11.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
# Eliminare obiecte redundante
rm(list = ls(pattern = "^g"), year, temp)

## 4. Studiul ref_020 - CODARE ####
## A. K. Goodboy, M. M. Martin (2015) 
## The personality profile of a cyberbully: Examining the Dark Triad
year <- tabel.surse[which(tabel.surse$label == "ref_020"),]$year
gIV.1 <- esc_beta(beta = 0.09, sdy = 0.10 * sqrt(227), grp1n = 227/2, grp2n = 227/2,
                  es.type = "g", study = "Goodboy et al. (2015)")
gIV.1.m.1 <- "Visual Cyberbullying"; gIV.1.m.2 <- "Machiavellianism"; gIV.1.rxx <- .84; gIV.1.ryy <- .79

gIV.2 <- esc_beta(beta = 0.27, sdy = 0.10 * sqrt(227), grp1n = 227/2, grp2n = 227/2,
                  es.type = "g", study = "Goodboy et al. (2015)")
gIV.2.m.1 <- "Visual Cyberbullying"; gIV.2.m.2 <- "Psychopathy"; gIV.2.rxx <- .84; gIV.2.ryy <- .80

gIV.3 <- esc_beta(beta = 0.05, sdy = 0.09 * sqrt(227), grp1n = 227/2, grp2n = 227/2,
                  es.type = "g", study = "Goodboy et al. (2015)")
gIV.3.m.1 <- "Visual Cyberbullying"; gIV.3.m.2 <- "Narcissism"; gIV.3.rxx <- .84; gIV.3.ryy <- .82

gIV.4 <- esc_beta(beta = 0.07, sdy = 0.12 * sqrt(227), grp1n = 227/2, grp2n = 227/2,
                  es.type = "g", study = "Goodboy et al. (2015)")
gIV.4.m.1 <- "Text Cyberbullying"; gIV.4.m.2 <- "Machiavellianism"; gIV.4.rxx <- .87; gIV.4.ryy <- .79

gIV.5 <- esc_beta(beta = 0.30, sdy = 0.11 * sqrt(227), grp1n = 227/2, grp2n = 227/2,
                  es.type = "g", study = "Goodboy et al. (2015)")
gIV.5.m.1 <- "Text Cyberbullying"; gIV.5.m.2 <- "Psychopathy"; gIV.5.rxx <- .87; gIV.5.ryy <- .80

gIV.6 <- esc_beta(beta = 0.12, sdy = 0.10 * sqrt(227), grp1n = 227/2, grp2n = 227/2,
                  es.type = "g", study = "Goodboy et al. (2015)")
gIV.6.m.1 <- "Text Cyberbullying"; gIV.6.m.2 <- "Narcissism"; gIV.6.rxx <- .87; gIV.6.ryy <- .82

### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(gIV.1, gIV.2, gIV.3, gIV.4, gIV.5, gIV.6)
temp <- cbind(temp,
              c(rep(year, 6)),
              c(gIV.1.m.1, gIV.2.m.1, gIV.3.m.1, gIV.4.m.1, gIV.5.m.1, gIV.6.m.1),
              c(gIV.1.m.2, gIV.2.m.2, gIV.3.m.2, gIV.4.m.2, gIV.5.m.2, gIV.6.m.2),
              c(gIV.1.rxx, gIV.2.rxx, gIV.3.rxx, gIV.4.rxx, gIV.5.rxx, gIV.6.rxx),
              c(gIV.1.ryy, gIV.2.ryy, gIV.3.ryy, gIV.4.ryy, gIV.5.ryy, gIV.6.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
# Eliminare obiecte redundante
rm(list = ls(pattern = "^g"), year, temp)

## 5. Studiul ref_022 - CODARE ####
## C. J. Hand, Graham G. Scott b, Zara P. Brodie b, Xilei Ye c, Sara C. Sereno (2021) 
## Tweet valence, volume of abuse, and observers’ dark tetrad personality factors influence victim-blaming and the perceived severity of twitter cyberabuse
year <- tabel.surse[which(tabel.surse$label == "ref_022"),]$year
r = .211; n = 125; conf.lvl = .95
gV.1.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.1.se <- 1/sqrt(n - 3)
gV.1.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.1.se * qnorm((1 + conf.lvl) / 2)
gV.1.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.1.ci)), totaln = n)
gV.1.study <- "Hand et al. (2021)"
gV.1.m.1 <- "Victim Blame"; gV.1.m.2 <- "Psychopathy"; gV.1.rxx <- .90; gV.1.ryy <- .71

r = .164; n = 125; conf.lvl = .95
gV.2.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.2.se <- 1/sqrt(n - 3)
gV.2.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.2.se * qnorm((1 + conf.lvl) / 2)
gV.2.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.2.ci)), totaln = n)
gV.2.study <- "Hand et al. (2021)"
gV.2.m.1 <- "Victim Blame"; gV.2.m.2 <- "Narcissism"; gV.2.rxx <- .90; gV.2.ryy <- .73

r = .252; n = 125; conf.lvl = .95
gV.3.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.3.se <- 1/sqrt(n - 3)
gV.3.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.3.se * qnorm((1 + conf.lvl) / 2)
gV.3.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.3.ci)), totaln = n)
gV.3.study <- "Hand et al. (2021)"
gV.3.m.1 <- "Victim Blame"; gV.3.m.2 <- "Machiavellianism"; gV.3.rxx <- .90; gV.3.ryy <- .70

r = .265; n = 125; conf.lvl = .95
gV.4.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.4.se <- 1/sqrt(n - 3)
gV.4.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.4.se * qnorm((1 + conf.lvl) / 2)
gV.4.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.4.ci)), totaln = n)
gV.4.study <- "Hand et al. (2021)"
gV.4.m.1 <- "Victim Blame"; gV.4.m.2 <- "Sadism"; gV.4.rxx <- .90; gV.4.ryy <- .85

r = -.276; n = 125; conf.lvl = .95
gV.5.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.5.se <- 1/sqrt(n - 3)
gV.5.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.5.se * qnorm((1 + conf.lvl) / 2)
gV.5.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.5.ci)), totaln = n)
gV.5.study <- "Hand et al. (2021)"
gV.5.m.1 <- "Perceived Severity"; gV.5.m.2 <- "Psychopathy"; gV.5.rxx <- .65; gV.5.ryy <- .71

r = -.108; n = 125; conf.lvl = .95
gV.6.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.6.se <- 1/sqrt(n - 3)
gV.6.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.6.se * qnorm((1 + conf.lvl) / 2)
gV.6.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.6.ci)), totaln = n)
gV.6.study <- "Hand et al. (2021)"
gV.6.m.1 <- "Perceived Severity"; gV.6.m.2 <- "Narcissism"; gV.6.rxx <- .65; gV.6.ryy <- .73

r = -.227; n = 125; conf.lvl = .95
gV.7.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.7.se <- 1/sqrt(n - 3)
gV.7.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.7.se * qnorm((1 + conf.lvl) / 2)
gV.7.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.7.ci)), totaln = n)
gV.7.study <- "Hand et al. (2021)"
gV.7.m.1 <- "Perceived Severity"; gV.7.m.2 <- "Machiavellianism"; gV.7.rxx <- .65; gV.7.ryy <- .70

r = -.199; n = 125; conf.lvl = .95
gV.8.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.8.se <- 1/sqrt(n - 3)
gV.8.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.8.se * qnorm((1 + conf.lvl) / 2)
gV.8.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.8.ci)), totaln = n)
gV.8.study <- "Hand et al. (2021)"
gV.8.m.1 <- "Perceived Severity"; gV.8.m.2 <- "Sadism"; gV.8.rxx <- .65; gV.8.ryy <- .85
### Combinare efecte si construirea bazei de date a efectelor ####
efecte <- rbind(efecte,
                c(gV.1.study, gV.1.es, 1/gV.1.se ^ 2, 125, gV.1.se, gV.1.se ^ 2, gV.1.ci[1], gV.1.ci[2], 
                  "g", year, gV.1.m.1, gV.1.m.2, gV.1.rxx, gV.1.ryy),
                c(gV.2.study, gV.2.es, 1/gV.2.se ^ 2, 125, gV.2.se, gV.2.se ^ 2, gV.2.ci[1], gV.2.ci[2], 
                  "g", year, gV.2.m.1, gV.2.m.2, gV.2.rxx, gV.2.ryy),
                c(gV.3.study, gV.3.es, 1/gV.3.se ^ 2, 125, gV.3.se, gV.3.se ^ 2, gV.3.ci[1], gV.3.ci[2], 
                  "g", year, gV.3.m.1, gV.3.m.2, gV.3.rxx, gV.3.ryy),
                c(gV.4.study, gV.4.es, 1/gV.4.se ^ 2, 125, gV.4.se, gV.4.se ^ 2, gV.4.ci[1], gV.4.ci[2], 
                  "g", year, gV.4.m.1, gV.4.m.2, gV.4.rxx, gV.4.ryy),
                c(gV.5.study, gV.5.es, 1/gV.5.se ^ 2, 125, gV.5.se, gV.5.se ^ 2, gV.5.ci[1], gV.5.ci[2], 
                  "g", year, gV.5.m.1, gV.5.m.2, gV.5.rxx, gV.5.ryy),
                c(gV.6.study, gV.6.es, 1/gV.6.se ^ 2, 125, gV.6.se, gV.6.se ^ 2, gV.6.ci[1], gV.6.ci[2], 
                  "g", year, gV.6.m.1, gV.6.m.2, gV.6.rxx, gV.6.ryy),
                c(gV.7.study, gV.7.es, 1/gV.7.se ^ 2, 125, gV.7.se, gV.7.se ^ 2, gV.7.ci[1], gV.7.ci[2], 
                  "g", year, gV.7.m.1, gV.7.m.2, gV.7.rxx, gV.7.ryy),
                c(gV.8.study, gV.8.es, 1/gV.8.se ^ 2, 125, gV.8.se, gV.8.se ^ 2, gV.8.ci[1], gV.8.ci[2], 
                  "g", year, gV.8.m.1, gV.8.m.2, gV.8.rxx, gV.8.ryy))
# Eliminare obiecte redundante
rm(list = ls(pattern = "^g"), year, conf.lvl, n, r)

## 6. Studiul ref_029 - CODARE ####
## Kurek, A., Jose, P. E., & Stuart, J. (2019). 
## I did it for the LULZ: How the dark personality predicts online disinhibition and aggressive online behavior in adolescence.
# 1 - Younger, 2 - Older
year <- tabel.surse[which(tabel.surse$label == "ref_029"),]$year
gVI.1 <- esc_mean_se(grp1m = 3.38, grp1se = 0.04, grp1n = 354, 
                     grp2m = 3.49, grp2se = 0.04, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019)")
gVI.1.m.1 <- "Narcissism"; gVI.1.m.2 <- "Gender"; gVI.1.rxx <- .93; gVI.1.ryy <- NA

gVI.2 <- esc_mean_se(grp1m = 2.95, grp1se = 0.03, grp1n = 354, 
                     grp2m = 2.54, grp2se = 0.03, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019)")
gVI.2.m.1 <- "Sadism"; gVI.2.m.2 <- "Gender"; gVI.2.rxx <- .79; gVI.2.ryy <- NA

gVI.3 <- esc_mean_se(grp1m = 1.89, grp1se = 0.01, grp1n = 354, 
                     grp2m = 1.93, grp2se = 0.01, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019)")
gVI.3.m.1 <- "Psychopathy"; gVI.3.m.2 <- "Gender"; gVI.3.rxx <- .71; gVI.3.ryy <- NA

gVI.4 <- esc_mean_se(grp1m = 1.38, grp1se = 0.03, grp1n = 354, 
                     grp2m = 1.28, grp2se = 0.03, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019)")
gVI.4.m.1 <- "Cyber Aggression"; gVI.4.m.2 <- "Gender"; gVI.4.rxx <- .92; gVI.4.ryy <- NA

gVI.5 <- esc_mean_se(grp1m = 3.36, grp1se = 0.04, grp1n = 354, 
                     grp2m = 3.52, grp2se = 0.04, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019)")
gVI.5.m.1 <- "Narcissism"; gVI.5.m.2 <- "Age"; gVI.5.rxx <- .93; gVI.5.ryy <- NA

gVI.6 <- esc_mean_se(grp1m = 2.65, grp1se = 0.03, grp1n = 354, 
                     grp2m = 2.60, grp2se = 0.03, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019)")
gVI.6.m.1 <- "Sadism"; gVI.6.m.2 <- "Age"; gVI.6.rxx <- .79; gVI.6.ryy <- NA

gVI.7 <- esc_mean_se(grp1m = 1.90, grp1se = 0.01, grp1n = 354, 
                     grp2m = 1.92, grp2se = 0.01, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019)")
gVI.7.m.1 <- "Psychopathy"; gVI.7.m.2 <- "Age"; gVI.7.rxx <- .71; gVI.7.ryy <- NA

gVI.8 <- esc_mean_se(grp1m = 1.36, grp1se = 0.03, grp1n = 354, 
                     grp2m = 1.30, grp2se = 0.03, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019)")
gVI.8.m.1 <- "Cyber Aggression"; gVI.8.m.2 <- "Age"; gVI.8.rxx <- .92; gVI.8.ryy <- NA
### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(gVI.1, gVI.2, gVI.3, gVI.4, gVI.5, gVI.6, gVI.7, gVI.8)
temp <- cbind(temp,
              c(rep(year, 8)),
              c(gVI.1.m.1, gVI.2.m.1, gVI.3.m.1, gVI.4.m.1, gVI.5.m.1, gVI.6.m.1,
                gVI.7.m.1, gVI.8.m.1),
              c(gVI.1.m.2, gVI.2.m.2, gVI.3.m.2, gVI.4.m.2, gVI.5.m.2, gVI.6.m.2, 
                gVI.7.m.2, gVI.8.m.2),
              c(gVI.1.rxx, gVI.2.rxx, gVI.3.rxx, gVI.4.rxx, gVI.5.rxx, gVI.6.rxx, 
                gVI.7.rxx, gVI.8.rxx),
              c(gVI.1.ryy, gVI.2.ryy, gVI.3.ryy, gVI.4.ryy, gVI.5.ryy, gVI.6.ryy, 
                gVI.7.ryy, gVI.8.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
# Eliminare obiecte redundante
rm(list = ls(pattern = "^g"), temp)

r = -.17; n = 718; conf.lvl = .95
gVI.9.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gVI.9.se <- 1/sqrt(n - 3)
gVI.9.ci <-convert_r2z(r) + c(-1, 1) * 
  gVI.9.se * qnorm((1 + conf.lvl) / 2)
gVI.9.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gVI.9.ci)), totaln = n)
gVI.9.study <- "Kurek et al. (2019)"
gVI.9.m.1 <- "Cyber Aggression"; gVI.9.m.2 <- "Narcissism"; gVI.9.rxx <- .92; gVI.9.ryy <- .93

r = -.26; n = 718; conf.lvl = .95
gVI.10.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gVI.10.se <- 1/sqrt(n - 3)
gVI.10.ci <-convert_r2z(r) + c(-1, 1) * 
  gVI.10.se * qnorm((1 + conf.lvl) / 2)
gVI.10.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gVI.10.ci)), totaln = n)
gVI.10.study <- "Kurek et al. (2019)"
gVI.10.m.1 <- "Cyber Aggression"; gVI.10.m.2 <- "Sadism"; gVI.10.rxx <- .92; gVI.10.ryy <- .79

r = -.05; n = 718; conf.lvl = .95
gVI.11.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gVI.11.se <- 1/sqrt(n - 3)
gVI.11.ci <-convert_r2z(r) + c(-1, 1) * 
  gVI.11.se * qnorm((1 + conf.lvl) / 2)
gVI.11.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gVI.11.ci)), totaln = n)
gVI.11.study <- "Kurek et al. (2019)"
gVI.11.m.1 <- "Cyber Aggression"; gVI.11.m.2 <- "Psychopathy"; gVI.11.rxx <- .92; gVI.11.ryy <- .71
### Combinare efecte si construirea bazei de date a efectelor ####
efecte <- rbind(efecte,
                c(gVI.9.study, gVI.9.es, 1/gVI.9.se ^ 2, 718, gVI.9.se, gVI.9.se ^ 2, gVI.9.ci[1], gVI.9.ci[2], 
                  "g", year, gVI.9.m.1, gVI.9.m.2, gVI.9.rxx, gVI.9.ryy),
                c(gVI.10.study, gVI.10.es, 1/gVI.10.se ^ 2, 718, gVI.10.se, gVI.10.se ^ 2, gVI.10.ci[1], gVI.10.ci[2], 
                  "g", year, gVI.10.m.1, gVI.10.m.2, gVI.10.rxx, gVI.10.ryy),
                c(gVI.11.study, gVI.11.es, 1/gVI.11.se ^ 2, 718, gVI.11.se, gVI.11.se ^ 2, gVI.11.ci[1], gVI.11.ci[2], 
                  "g", year, gVI.11.m.1, gVI.11.m.2, gVI.11.rxx, gVI.11.ryy))
# Eliminare obiecte redundante
rm(list = ls(pattern = "^g"), year, conf.lvl, n, r)

## 7. Studiul ref_045 - CODARE ####
## Pabian, S., De Backer, C. J. S., & Vandebosch, H. (2015) 
## Dark Triad personality traits and adolescent cyber-aggression
year <- tabel.surse[which(tabel.surse$label == "ref_045"),]$year
t.val <- qt(0.05 / 2, df = 277); beta <- 0.60
gVII.1 <- esc_beta(beta = beta, sdy = beta / abs(t.val), grp1n = 277/2, grp2n = 277/2,
                  es.type = "g", study = "Pabian et al. (2015)")
gVII.1.m.1 <- "Cyber Aggression"; gVII.1.m.2 <- "Psychopathy"; gVII.1.rxx <- .84; gVII.1.ryy <- .77

t.val <- qt(0.199 / 2, df = 277); beta <- -0.36
gVII.2 <- esc_beta(beta = beta, sdy = beta / abs(t.val), grp1n = 277/2, grp2n = 277/2,
                   es.type = "g", study = "Pabian et al. (2015)")
gVII.2.m.1 <- "Cyber Aggression"; gVII.2.m.2 <- "Machiavellianism"; gVII.2.rxx <- .84; gVII.2.ryy <- .74

t.val <- qt(0.055 / 2, df = 277); beta <- 0.23
gVII.3 <- esc_beta(beta = beta, sdy = beta / abs(t.val), grp1n = 277/2, grp2n = 277/2,
                   es.type = "g", study = "Pabian et al. (2015)")
gVII.3.m.1 <- "Cyber Aggression"; gVII.3.m.2 <- "Narcissism"; gVII.3.rxx <- .84; gVII.3.ryy <- .61
### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(gVII.1, gVII.2, gVII.3)
temp <- cbind(temp,
              c(rep(year, 3)),
              c(gVII.1.m.1, gVII.2.m.1, gVII.3.m.1),
              c(gVII.1.m.2, gVII.2.m.2, gVII.3.m.2),
              c(gVII.1.rxx, gVII.2.rxx, gVII.3.rxx),
              c(gVII.1.ryy, gVII.2.ryy, gVII.3.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), temp, year, t.val, beta)

## 8. Studiul ref_053 - CODARE ####
## Stiff C. (2019)
## The Dark Triad and Facebook surveillance: How Machiavellianism, psychopathy, but not narcissism predict using Facebook to spy on others
year <- tabel.surse[which(tabel.surse$label == "ref_053"),]$year
gVIII.1 <- esc_beta(beta = 0.16, sdy = 0.10 * sqrt(259), grp1n = 259/2, grp2n = 259/2,
                   es.type = "g", study = "Pabian et al. (2019)")
gVIII.1.m.1 <- "Facebook tracking"; gVIII.1.m.2 <- "Machiavellianism"; gVIII.1.rxx <- .7; gVIII.1.ryy <- .7

gVIII.2 <- esc_beta(beta = 0.04, sdy = 0.09 * sqrt(259), grp1n = 259/2, grp2n = 259/2,
                    es.type = "g", study = "Pabian et al. (2019)")
gVIII.2.m.1 <- "Facebook investigating"; gVIII.2.m.2 <- "Machiavellianism"; gVIII.2.rxx <- .7; gVIII.2.ryy <- .7

gVIII.3 <- esc_beta(beta = 0.27, sdy = 0.10 * sqrt(259), grp1n = 259/2, grp2n = 259/2,
                    es.type = "g", study = "Pabian et al. (2019)")
gVIII.3.m.1 <- "Facebook tracking"; gVIII.3.m.2 <- "Psychopathy"; gVIII.3.rxx <- .7; gVIII.3.ryy <- .7

gVIII.4 <- esc_beta(beta = 0.10, sdy = 0.09 * sqrt(259), grp1n = 259/2, grp2n = 259/2,
                    es.type = "g", study = "Pabian et al. (2019)")
gVIII.4.m.1 <- "Facebook investigating"; gVIII.4.m.2 <- "Psychopathy"; gVIII.4.rxx <- .7; gVIII.4.ryy <- .7

### Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(gVIII.1, gVIII.2, gVIII.3, gVIII.4)
temp <- cbind(temp,
              c(rep(year, 4)),
              c(gVIII.1.m.1, gVIII.2.m.1, gVIII.3.m.1, gVIII.4.m.1),
              c(gVIII.1.m.2, gVIII.2.m.2, gVIII.3.m.2, gVIII.4.m.2),
              c(gVIII.1.rxx, gVIII.2.rxx, gVIII.3.rxx, gVIII.4.rxx),
              c(gVIII.1.ryy, gVIII.2.ryy, gVIII.3.ryy, gVIII.4.ryy))
colnames(temp) <- c(names(temp[1:9]), "year", "moderator.1", "moderator.2", "alpha.1", "alpha.2")
efecte <- rbind(efecte, temp)
# Eliminare obiecte ramase
rm(list = ls(pattern = "^g"), temp)


## 9. Studiul ref_059.a - CODARE ####
## Buckels E.E. Trapnell P. D., Andjelovic T, Paulhus D.L. (2018)
## Internet Trolling and Everyday Sadism: Parallel Effects on Pain Perception and Moral Judgment



## 10. Studiul ref_059.b - CODARE ####
## Buckels E.E. Trapnell P. D., Andjelovic T, Paulhus D.L. (2018)
## Internet Trolling and Everyday Sadism: Parallel Effects on Pain Perception and Moral Judgment

temp <- efecte %>%
  dplyr::mutate(ponderare = as.numeric(weight) * as.numeric(es))
sum(temp$ponderare, na.rm = T) / sum(as.numeric(temp$weight), na.rm = T)
1/sum(as.numeric(temp$weight), na.rm = T)

