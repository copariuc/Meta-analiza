# Incarcarea pachetelor necesare
if (!require(esc)) {install.packages("esc")}
library(esc)

# Demonstrarea marimii efectului
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

# Codarea efectelor folosindu-se pachetul "esc"
# M - Machiavelism; N - Narcisism; P - Psihopatie
# 1 - Masculin; 2 (0) - Feminin

## 1. Studiul ref_004 - CODARE ####
## H. M. Baughman, S. Dearing, E. Giammarco, P. A. Vernon (2012) - 
## Relationships between bullying behaviours and the Dark Triad: A study with adults
gI.1 <- esc_mean_sd(grp1m = 3.42, grp1sd = 0.60, grp1n = 203, 
                    grp2m = 3.14, grp2sd = 0.54, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012) - Machiavellianism: Male - Female")
gI.2 <- esc_mean_sd(grp1m = 3.07, grp1sd = 0.58, grp1n = 203, 
                    grp2m = 2.86, grp2sd = 0.51, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012) - Narcissism: Male - Female")
gI.3 <- esc_mean_sd(grp1m = 2.37, grp1sd = 0.69, grp1n = 203, 
                    grp2m = 2.04, grp2sd = 0.53, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012) - Psychopathy: Male - Female")
gI.4 <- esc_mean_sd(grp1m = 0.46, grp1sd = 0.52, grp1n = 203, 
                    grp2m = 0.34, grp2sd = 0.43, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012) - Bullying Physical Direct: Male - Female")
gI.5 <- esc_mean_sd(grp1m = 0.85, grp1sd = 0.68, grp1n = 203, 
                    grp2m = 0.69, grp2sd = 0.52, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012) - Bullying Verbal Direct: Male - Female")
gI.6 <- esc_mean_sd(grp1m = 0.72, grp1sd = 0.61, grp1n = 203, 
                    grp2m = 0.56, grp2sd = 0.45, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012) - Bullying Direct: Male - Female")
gI.7 <- esc_mean_sd(grp1m = 0.54, grp1sd = 0.55, grp1n = 203, 
                    grp2m = 0.46, grp2sd = 0.43, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012) - Bullying Indirect: Male - Female")
gI.8 <- esc_mean_sd(grp1m = 0.66, grp1sd = 0.56, grp1n = 203, 
                    grp2m = 0.52, grp2sd = 0.41, grp2n = 454, 
                    es.type = "g", study = "Baughman et al. (2012) - Bullying Total: Male - Female")
r = .35; n = 657; conf.lvl = .95
gI.9.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.9.se <- 1/sqrt(n - 3)
gI.9.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.9.se * qnorm((1 + conf.lvl) / 2)
gI.9.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.9.ci)), totaln = n)
gI.9.study <- "Baughman et al. (2012) - Bullying Total & Machiavellianism"
r = .22; n = 657; conf.lvl = .95
gI.10.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.10.se <- 1/sqrt(n - 3)
gI.10.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.10.se * qnorm((1 + conf.lvl) / 2)
gI.10.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.10.ci)), totaln = n)
gI.10.study <- "Baughman et al. (2012) - Bullying Total & Narcissism"
r = .55; n = 657; conf.lvl = .95
gI.11.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.11.se <- 1/sqrt(n - 3)
gI.11.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.11.se * qnorm((1 + conf.lvl) / 2)
gI.11.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.11.ci)), totaln = n)
gI.11.study <- "Baughman et al. (2012) - Bullying Total & Psychopathy"
r = .33; n = 657; conf.lvl = .95
gI.12.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.12.se <- 1/sqrt(n - 3)
gI.12.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.12.se * qnorm((1 + conf.lvl) / 2)
gI.12.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.12.ci)), totaln = n)
gI.12.study <- "Baughman et al. (2012) - Bullying Indirect & Machiavellianism"
r = .21; n = 657; conf.lvl = .95
gI.13.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.13.se <- 1/sqrt(n - 3)
gI.13.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.13.se * qnorm((1 + conf.lvl) / 2)
gI.13.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.13.ci)), totaln = n)
gI.13.study <- "Baughman et al. (2012) - Bullying Indirect & Narcissism"
r = .49; n = 657; conf.lvl = .95
gI.14.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.14.se <- 1/sqrt(n - 3)
gI.14.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.14.se * qnorm((1 + conf.lvl) / 2)
gI.14.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.14.ci)), totaln = n)
gI.14.study <- "Baughman et al. (2012) - Bullying Indirect & Psychopathy"
r = .34; n = 657; conf.lvl = .95
gI.15.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.15.se <- 1/sqrt(n - 3)
gI.15.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.15.se * qnorm((1 + conf.lvl) / 2)
gI.15.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.15.ci)), totaln = n)
gI.15.study <- "Baughman et al. (2012) - Bullying Direct & Machiavellianism"
r = .20; n = 657; conf.lvl = .95
gI.16.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.16.se <- 1/sqrt(n - 3)
gI.16.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.16.se * qnorm((1 + conf.lvl) / 2)
gI.16.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.16.ci)), totaln = n)
gI.16.study <- "Baughman et al. (2012) - Bullying Direct & Narcissism"
r = .53; n = 657; conf.lvl = .95
gI.17.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.17.se <- 1/sqrt(n - 3)
gI.17.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.17.se * qnorm((1 + conf.lvl) / 2)
gI.17.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.17.ci)), totaln = n)
gI.17.study <- "Baughman et al. (2012) - Bullying Direct & Psychopathy"
r = .35; n = 657; conf.lvl = .95
gI.18.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.18.se <- 1/sqrt(n - 3)
gI.18.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.18.se * qnorm((1 + conf.lvl) / 2)
gI.18.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.18.ci)), totaln = n)
gI.18.study <- "Baughman et al. (2012) - Bullying Verbal Direct & Machiavellianism"
r = .20; n = 657; conf.lvl = .95
gI.19.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.19.se <- 1/sqrt(n - 3)
gI.19.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.19.se * qnorm((1 + conf.lvl) / 2)
gI.19.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.19.ci)), totaln = n)
gI.19.study <- "Baughman et al. (2012) - Bullying Verbal Direct & Narcissism"
r = .51; n = 657; conf.lvl = .95
gI.20.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.20.se <- 1/sqrt(n - 3)
gI.20.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.20.se * qnorm((1 + conf.lvl) / 2)
gI.20.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.20.ci)), totaln = n)
gI.20.study <- "Baughman et al. (2012) - Bullying Verbal Direct & Psychopathy"
r = .22; n = 657; conf.lvl = .95
gI.21.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.21.se <- 1/sqrt(n - 3)
gI.21.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.21.se * qnorm((1 + conf.lvl) / 2)
gI.21.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.21.ci)), totaln = n)
gI.21.study <- "Baughman et al. (2012) - Bullying Physical Direct & Machiavellianism"
r = .11; n = 657; conf.lvl = .95
gI.22.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.22.se <- 1/sqrt(n - 3)
gI.22.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.22.se * qnorm((1 + conf.lvl) / 2)
gI.22.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.22.ci)), totaln = n)
gI.22.study <- "Baughman et al. (2012) - Bullying Physical Direct & Narcissism"
r = .41; n = 657; conf.lvl = .95
gI.23.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gI.23.se <- 1/sqrt(n - 3)
gI.23.ci <-convert_r2z(r) + c(-1, 1) * 
  gI.23.se * qnorm((1 + conf.lvl) / 2)
gI.23.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gI.23.ci)), totaln = n)
gI.23.study <- "Baughman et al. (2012) - Bullying Physical Direct & Psychopathy"
## 2. Studiul ref_006 - CODARE ####
## O. Bogolyubova, P. Panicheva, R. Tikhonov, V. Ivanov, Y. Ledovaya (2018) 
## Dark personalities on Facebook: Harmful online behaviors and language
# Caracteristici ale participantilor
gII.1 <- esc_mean_sd(grp1m = 3.43, grp1sd = 0.63, grp1n = 1487, 
                     grp2m = 3.16, grp2sd = 0.64, grp2n = 5237, 
                     es.type = "g", study = "Bogolyubova et al. (2018) - Machiavellianism: Male - Female")
gII.2 <- esc_mean_sd(grp1m = 3.00, grp1sd = 0.63, grp1n = 1487, 
                     grp2m = 3.04, grp2sd = 0.53, grp2n = 5237, 
                     es.type = "g", study = "Bogolyubova et al. (2018) - Narcissism: Male - Female")
gII.3 <- esc_mean_sd(grp1m = 2.30, grp1sd = 0.59, grp1n = 1487, 
                     grp2m = 1.89, grp2sd = 0.53, grp2n = 5237, 
                     es.type = "g", study = "Bogolyubova et al. (2018) - Psychopathy: Male - Female")
gII.4 <- esc_beta(beta = 0.62, sdy = 0.07 * sqrt(6724), grp1n = 1487, grp2n = 5237, 
                  es.type = "g", study = "Bogolyubova et al. (2018) - Harmfull by Gender")
gII.5 <- esc_beta(beta = 1.00, sdy = 0.06 * sqrt(6724), grp1n = 6724/2, grp2n = 6724/2, 
                  es.type = "g", study = "Bogolyubova et al. (2018) - Harmfull by Psychopathy")
gII.6 <- esc_beta(beta = 0.01, sdy = 0.05 * sqrt(6724), grp1n = 6724/2, grp2n = 6724/2, 
                  es.type = "g", study = "Bogolyubova et al. (2018) - Harmfull by Machiavellianism")
gII.7 <- esc_beta(beta = 0.01, sdy = 0.05 * sqrt(6724), grp1n = 6724/2, grp2n = 6724/2, 
                  es.type = "g", study = "Bogolyubova et al. (2018) - Harmfull by Narcissism")
## 3. Studiul ref_012 - CODARE ####
## N. Craker, E. March (2016)
## The dark side of Facebook: The Dark Tetrad, negative social potency, and trolling behaviours
gIII.1 <- esc_mean_sd(grp1m = 15.86, grp1sd = 4.58, grp1n = 87, 
                      grp2m = 12.52, grp2sd = 4.00, grp2n = 292, 
                      es.type = "g", study = "Craker et al. (2016) - Trolling: Male - Female")
gIII.2 <- esc_mean_sd(grp1m = 1.32, grp1sd = 1.58, grp1n = 77, 
                      grp2m = 0.68, grp2sd = 0.99, grp2n = 266, 
                      es.type = "g", study = "Craker et al. (2016) - Sadism: Male - Female")
gIII.3 <- esc_mean_sd(grp1m = 16.44, grp1sd = 6.89, grp1n = 78, 
                      grp2m = 13.09, grp2sd = 6.16, grp2n = 269, 
                      es.type = "g", study = "Craker et al. (2016) - Machiavellianism: Male - Female")
gIII.4 <- esc_mean_sd(grp1m = 13.62, grp1sd = 6.82, grp1n = 78, 
                      grp2m = 9.76, grp2sd = 4.97, grp2n = 269, 
                      es.type = "g", study = "Craker et al. (2016) - Psychopathy: Male - Female")
gIII.5 <- esc_mean_sd(grp1m = 19.73, grp1sd = 6.88, grp1n = 78, 
                      grp2m = 17.48, grp2sd = 7.01, grp2n = 269, 
                      es.type = "g", study = "Craker et al. (2016) - Narcissism: Male - Female")
gIII.6 <- esc_beta(beta = -0.06, sdy = 0.02 * sqrt(396), grp1n = 396/2, grp2n = 396/2,
                  es.type = "g", study = "Craker et al. (2016) - Trolling by Age")
gIII.7 <- esc_beta(beta = -0.19, sdy = 0.51 * sqrt(396), grp1n = 94, grp2n = 296,
                   es.type = "g", study = "Craker et al. (2016) - Trolling by Gender")
gIII.8 <- esc_beta(beta = 0.07, sdy = 0.21 * sqrt(396), grp1n = 396/2, grp2n = 396/2,
                   es.type = "g", study = "Craker et al. (2016) - Trolling by Sadism")
gIII.9 <- esc_beta(beta = -0.00, sdy = 0.05 * sqrt(396), grp1n = 396/2, grp2n = 396/2,
                   es.type = "g", study = "Craker et al. (2016) - Trolling by Machiavellianism")
gIII.10 <- esc_beta(beta = 0.06, sdy = 0.05 * sqrt(396), grp1n = 396/2, grp2n = 396/2,
                   es.type = "g", study = "Craker et al. (2016) - Trolling by Psychopathy")
gIII.11 <- esc_beta(beta = -0.01, sdy = 0.03 * sqrt(396), grp1n = 396/2, grp2n = 396/2,
                    es.type = "g", study = "Craker et al. (2016) - Trolling by Narcissism")
## 4. Studiul ref_020 - CODARE ####
## A. K. Goodboy, M. M. Martin (2015) 
## The personality profile of a cyberbully: Examining the Dark Triad
gIV.1 <- esc_beta(beta = 0.09, sdy = 0.10 * sqrt(227), grp1n = 227/2, grp2n = 227/2,
                  es.type = "g", study = "Goodboy et al. (2015) - Visual Cyberbullying by Machiavellianism")
gIV.2 <- esc_beta(beta = 0.27, sdy = 0.10 * sqrt(227), grp1n = 227/2, grp2n = 227/2,
                  es.type = "g", study = "Goodboy et al. (2015) - Visual Cyberbullying by Psychopathy")
gIV.3 <- esc_beta(beta = 0.05, sdy = 0.09 * sqrt(227), grp1n = 227/2, grp2n = 227/2,
                  es.type = "g", study = "Goodboy et al. (2015) - Visual Cyberbullying by Narcissism")
gIV.4 <- esc_beta(beta = 0.07, sdy = 0.12 * sqrt(227), grp1n = 227/2, grp2n = 227/2,
                  es.type = "g", study = "Goodboy et al. (2015) - Text Cyberbullying by Machiavellianism")
gIV.5 <- esc_beta(beta = 0.30, sdy = 0.11 * sqrt(227), grp1n = 227/2, grp2n = 227/2,
                  es.type = "g", study = "Goodboy et al. (2015) - Text Cyberbullying by Psychopathy")
gIV.6 <- esc_beta(beta = 0.12, sdy = 0.10 * sqrt(227), grp1n = 227/2, grp2n = 227/2,
                  es.type = "g", study = "Goodboy et al. (2015) - Text Cyberbullying by Narcissism")
## 5. Studiul ref_022 - CODARE ####
## C. J. Hand, Graham G. Scott b, Zara P. Brodie b, Xilei Ye c, Sara C. Sereno (2021) 
## Tweet valence, volume of abuse, and observers’ dark tetrad personality factors influence victim-blaming and the perceived severity of twitter cyberabuse
r = .211; n = 125; conf.lvl = .95
gV.1.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.1.se <- 1/sqrt(n - 3)
gV.1.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.1.se * qnorm((1 + conf.lvl) / 2)
gV.1.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.1.ci)), totaln = n)
gV.1.study <- "Hand et al. (2021) - Victim Blame & Psychopathy"
r = .164; n = 125; conf.lvl = .95
gV.2.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.2.se <- 1/sqrt(n - 3)
gV.2.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.2.se * qnorm((1 + conf.lvl) / 2)
gV.2.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.2.ci)), totaln = n)
gV.2.study <- "Hand et al. (2021) - Victim Blame & Narcissism"
r = .252; n = 125; conf.lvl = .95
gV.3.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.3.se <- 1/sqrt(n - 3)
gV.3.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.3.se * qnorm((1 + conf.lvl) / 2)
gV.3.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.3.ci)), totaln = n)
gV.3.study <- "Hand et al. (2021) - Victim Blame & Machiavellianism"
r = .265; n = 125; conf.lvl = .95
gV.4.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.4.se <- 1/sqrt(n - 3)
gV.4.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.4.se * qnorm((1 + conf.lvl) / 2)
gV.4.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.4.ci)), totaln = n)
gV.4.study <- "Hand et al. (2021) - Victim Blame & Sadism"
r = -.276; n = 125; conf.lvl = .95
gV.5.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.5.se <- 1/sqrt(n - 3)
gV.5.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.5.se * qnorm((1 + conf.lvl) / 2)
gV.5.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.5.ci)), totaln = n)
gV.5.study <- "Hand et al. (2021) - Total Perceived Severity & Psychopathy"
r = -.108; n = 125; conf.lvl = .95
gV.6.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.6.se <- 1/sqrt(n - 3)
gV.6.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.6.se * qnorm((1 + conf.lvl) / 2)
gV.6.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.6.ci)), totaln = n)
gV.6.study <- "Hand et al. (2021) - Total Perceived Severity & Narcissism"
r = -.227; n = 125; conf.lvl = .95
gV.7.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.7.se <- 1/sqrt(n - 3)
gV.7.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.7.se * qnorm((1 + conf.lvl) / 2)
gV.7.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.7.ci)), totaln = n)
gV.7.study <- "Hand et al. (2021) - Total Perceived Severity & Machiavellianism"
r = -.199; n = 125; conf.lvl = .95
gV.8.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gV.8.se <- 1/sqrt(n - 3)
gV.8.ci <-convert_r2z(r) + c(-1, 1) * 
  gV.8.se * qnorm((1 + conf.lvl) / 2)
gV.8.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gV.8.ci)), totaln = n)
gV.8.study <- "Hand et al. (2021) - Total Perceived Severity & Sadism"
## 6. Studiul ref_029 - CODARE ####
## Kurek, A., Jose, P. E., & Stuart, J. (2019). 
## I did it for the LULZ: How the dark personality predicts online disinhibition and aggressive online behavior in adolescence.
gVI.1 <- esc_mean_se(grp1m = 3.38, grp1se = 0.04, grp1n = 354, 
                     grp2m = 3.49, grp2se = 0.04, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019) - Narcissism: Male - Female")
gVI.2 <- esc_mean_se(grp1m = 2.95, grp1se = 0.03, grp1n = 354, 
                     grp2m = 2.54, grp2se = 0.03, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019) - Sadism: Male - Female")
gVI.3 <- esc_mean_se(grp1m = 1.89, grp1se = 0.01, grp1n = 354, 
                     grp2m = 1.93, grp2se = 0.01, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019) - Psychopathy: Male - Female")
gVI.4 <- esc_mean_se(grp1m = 1.38, grp1se = 0.03, grp1n = 354, 
                     grp2m = 1.28, grp2se = 0.03, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019) - Cyber Aggression: Male - Female")
gVI.5 <- esc_mean_se(grp1m = 3.36, grp1se = 0.04, grp1n = 354, 
                     grp2m = 3.52, grp2se = 0.04, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019) - Narcissism: Younger - Older")
gVI.6 <- esc_mean_se(grp1m = 2.65, grp1se = 0.03, grp1n = 354, 
                     grp2m = 2.60, grp2se = 0.03, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019) - Sadism: Younger - Older")
gVI.7 <- esc_mean_se(grp1m = 1.90, grp1se = 0.01, grp1n = 354, 
                     grp2m = 1.92, grp2se = 0.01, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019) - Psychopathy: Younger - Older")
gVI.8 <- esc_mean_se(grp1m = 1.36, grp1se = 0.03, grp1n = 354, 
                     grp2m = 1.30, grp2se = 0.03, grp2n = 355, 
                     es.type = "g", study = "Kurek et al. (2019) - Cyber Aggression: Younger - Older")
r = -.17; n = 718; conf.lvl = .95
gVI.9.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gVI.9.se <- 1/sqrt(n - 3)
gVI.9.ci <-convert_r2z(r) + c(-1, 1) * 
  gVI.9.se * qnorm((1 + conf.lvl) / 2)
gVI.9.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gVI.9.ci)), totaln = n)
gVI.9.study <- "Kurek et al. (2019) - Cyber Aggression & Narcissism"
r = -.26; n = 718; conf.lvl = .95
gVI.10.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gVI.10.se <- 1/sqrt(n - 3)
gVI.10.ci <-convert_r2z(r) + c(-1, 1) * 
  gVI.10.se * qnorm((1 + conf.lvl) / 2)
gVI.10.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gVI.10.ci)), totaln = n)
gVI.10.study <- "Kurek et al. (2019) - Cyber Aggression & Sadism"
r = -.05; n = 718; conf.lvl = .95
gVI.11.es <- hedges_g(d = cohens_d(r = r), totaln = n)
gVI.11.se <- 1/sqrt(n - 3)
gVI.11.ci <-convert_r2z(r) + c(-1, 1) * 
  gVI.11.se * qnorm((1 + conf.lvl) / 2)
gVI.11.ci <- hedges_g(d = cohens_d(r = convert_z2r(
  gVI.11.ci)), totaln = n)
gVI.11.study <- "Kurek et al. (2019) - Cyber Aggression & Psychopathy"
## 7. Studiul ref_045 - CODARE ####
## Pabian, S., De Backer, C. J. S., & Vandebosch, H. (2015) 
## Dark Triad personality traits and adolescent cyber-aggression
t.val <- qt(0.05 / 2, df = 277); beta <- 0.60
gVII.1 <- esc_beta(beta = beta, sdy = beta / abs(t.val), grp1n = 277/2, grp2n = 277/2,
                  es.type = "g", study = "Pabian et al. (2015) - Cyber Aggression by Psychopathy")
t.val <- qt(0.199 / 2, df = 277); beta <- -0.36
gVII.2 <- esc_beta(beta = beta, sdy = beta / abs(t.val), grp1n = 277/2, grp2n = 277/2,
                   es.type = "g", study = "Pabian et al. (2015) - Cyber Aggression by Machiavellianism")
t.val <- qt(0.055 / 2, df = 277); beta <- 0.23
gVII.3 <- esc_beta(beta = beta, sdy = beta / abs(t.val), grp1n = 277/2, grp2n = 277/2,
                   es.type = "g", study = "Pabian et al. (2015) - Cyber Aggression by Narcissism")
## 8. Studiul ref_053 - CODARE ####
## Stiff C. (2019)
## The Dark Triad and Facebook surveillance: How Machiavellianism, psychopathy, but not narcissism predict using Facebook to spy on others
gVIII.1 <- esc_beta(beta = 0.16, sdy = 0.10 * sqrt(259), grp1n = 259/2, grp2n = 259/2,
                   es.type = "g", study = "Pabian et al. (2015) - Facebook tracking by Machiavellianism")
gVIII.2 <- esc_beta(beta = 0.04, sdy = 0.09 * sqrt(259), grp1n = 259/2, grp2n = 259/2,
                    es.type = "g", study = "Pabian et al. (2015) - Facebook investigating by Machiavellianism")
gVIII.3 <- esc_beta(beta = 0.27, sdy = 0.10 * sqrt(259), grp1n = 259/2, grp2n = 259/2,
                    es.type = "g", study = "Pabian et al. (2015) - Facebook tracking by Psychopathy")
gVIII.4 <- esc_beta(beta = 0.10, sdy = 0.09 * sqrt(259), grp1n = 259/2, grp2n = 259/2,
                    es.type = "g", study = "Pabian et al. (2015) - Facebook investigating by Psychopathy")
## 9. Studiul ref_059.a - CODARE ####
## Buckels E.E. Trapnell P. D., Andjelovic T, Paulhus D.L. (2018)
## Internet Trolling and Everyday Sadism: Parallel Effects on Pain Perception and Moral Judgment



## 10. Studiul ref_059.b - CODARE ####
## Buckels E.E. Trapnell P. D., Andjelovic T, Paulhus D.L. (2018)
## Internet Trolling and Everyday Sadism: Parallel Effects on Pain Perception and Moral Judgment








# Combinare efecte si construirea bazei de date a efectelor ####
temp <- combine_esc(gI.1, gI.2, gI.3, gI.4, gI.5, gI.6, gI.7, gI.8)
bd.efecte <- rbind(temp,
                   c(gI.9.study, gI.9.es, 1/gI.9.se ^ 2, 657, gI.9.se, gI.9.se ^ 2, gI.9.ci[1], gI.9.ci[2], "g"),
                   c(gI.10.study, gI.10.es, 1/gI.10.se ^ 2, 657, gI.10.se, gI.10.se ^ 2, gI.10.ci[1], gI.10.ci[2], "g"),
                   c(gI.11.study, gI.11.es, 1/gI.11.se ^ 2, 657, gI.11.se, gI.11.se ^ 2, gI.11.ci[1], gI.11.ci[2], "g"),
                   c(gI.12.study, gI.12.es, 1/gI.12.se ^ 2, 657, gI.12.se, gI.12.se ^ 2, gI.12.ci[1], gI.12.ci[2], "g"),
                   c(gI.13.study, gI.13.es, 1/gI.13.se ^ 2, 657, gI.13.se, gI.13.se ^ 2, gI.13.ci[1], gI.13.ci[2], "g"),
                   c(gI.14.study, gI.14.es, 1/gI.14.se ^ 2, 657, gI.14.se, gI.14.se ^ 2, gI.14.ci[1], gI.14.ci[2], "g"),
                   c(gI.15.study, gI.15.es, 1/gI.15.se ^ 2, 657, gI.15.se, gI.15.se ^ 2, gI.15.ci[1], gI.15.ci[2], "g"),
                   c(gI.16.study, gI.16.es, 1/gI.16.se ^ 2, 657, gI.16.se, gI.16.se ^ 2, gI.16.ci[1], gI.16.ci[2], "g"),
                   c(gI.17.study, gI.17.es, 1/gI.17.se ^ 2, 657, gI.17.se, gI.17.se ^ 2, gI.17.ci[1], gI.17.ci[2], "g"),
                   c(gI.18.study, gI.18.es, 1/gI.18.se ^ 2, 657, gI.18.se, gI.18.se ^ 2, gI.18.ci[1], gI.18.ci[2], "g"),
                   c(gI.19.study, gI.19.es, 1/gI.19.se ^ 2, 657, gI.19.se, gI.19.se ^ 2, gI.19.ci[1], gI.19.ci[2], "g"),
                   c(gI.20.study, gI.20.es, 1/gI.20.se ^ 2, 657, gI.20.se, gI.20.se ^ 2, gI.20.ci[1], gI.20.ci[2], "g"),
                   c(gI.21.study, gI.21.es, 1/gI.21.se ^ 2, 657, gI.21.se, gI.21.se ^ 2, gI.21.ci[1], gI.21.ci[2], "g"),
                   c(gI.22.study, gI.22.es, 1/gI.22.se ^ 2, 657, gI.22.se, gI.22.se ^ 2, gI.22.ci[1], gI.22.ci[2], "g"),
                   c(gI.23.study, gI.23.es, 1/gI.23.se ^ 2, 657, gI.23.se, gI.23.se ^ 2, gI.23.ci[1], gI.23.ci[2], "g"))
temp <- combine_esc(gII.1, gII.2, gII.3, gII.4, gII.5, gII.6, gII.7,
                    gIII.1, gIII.2, gIII.3, gIII.4, gIII.5, gIII.6, gIII.7, gIII.8, gIII.9, gIII.10, gIII.11,
                    gIV.1, gIV.2, gIV.3, gIV.4, gIV.5, gIV.6)
bd.efecte <- rbind(bd.efecte, temp,
                   c(gV.1.study, gV.1.es, 1/gV.1.se ^ 2, 125, gV.1.se, gV.1.se ^ 2, gV.1.ci[1], gV.1.ci[2], "g"),
                   c(gV.2.study, gV.2.es, 1/gV.2.se ^ 2, 125, gV.2.se, gV.2.se ^ 2, gV.2.ci[1], gV.2.ci[2], "g"),
                   c(gV.3.study, gV.3.es, 1/gV.3.se ^ 2, 125, gV.3.se, gV.3.se ^ 2, gV.3.ci[1], gV.3.ci[2], "g"),
                   c(gV.4.study, gV.4.es, 1/gV.4.se ^ 2, 125, gV.4.se, gV.4.se ^ 2, gV.4.ci[1], gV.4.ci[2], "g"),
                   c(gV.5.study, gV.5.es, 1/gV.5.se ^ 2, 125, gV.5.se, gV.5.se ^ 2, gV.5.ci[1], gV.5.ci[2], "g"),
                   c(gV.6.study, gV.6.es, 1/gV.6.se ^ 2, 125, gV.6.se, gV.6.se ^ 2, gV.6.ci[1], gV.6.ci[2], "g"),
                   c(gV.7.study, gV.7.es, 1/gV.7.se ^ 2, 125, gV.7.se, gV.7.se ^ 2, gV.7.ci[1], gV.7.ci[2], "g"),
                   c(gV.8.study, gV.8.es, 1/gV.8.se ^ 2, 125, gV.8.se, gV.8.se ^ 2, gV.8.ci[1], gV.8.ci[2], "g"))
temp <- combine_esc(gVI.1, gVI.2, gVI.3, gVI.4, gVI.5, gVI.6, gVI.7, gVI.8)
bd.efecte <- rbind(bd.efecte, temp,
                   c(gVI.9.study, gVI.9.es, 1/gVI.9.se ^ 2, 718, gVI.9.se, gVI.9.se ^ 2, gVI.9.ci[1], gVI.9.ci[2], "g"),
                   c(gVI.10.study, gVI.10.es, 1/gVI.10.se ^ 2, 718, gVI.10.se, gVI.10.se ^ 2, gVI.10.ci[1], gVI.10.ci[2], "g"),
                   c(gVI.11.study, gVI.11.es, 1/gVI.11.se ^ 2, 718, gVI.11.se, gVI.11.se ^ 2, gVI.11.ci[1], gVI.11.ci[2], "g"))
temp <- combine_esc(gVII.1, gVII.2, gVII.3,
                    gVIII.1, gVIII.2, gVIII.3, gVIII.4)
bd.efecte <- rbind(bd.efecte, temp)



# Eliminare obiecte redundante
rm(list = ls(pattern = "^g"), conf.lvl, n, r, temp)
