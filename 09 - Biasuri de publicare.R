# Incarcarea pachetelor necesare
if (!require(metasens)) install.packages("metasens")
library(meta); library(metasens); library(dmetar)

# Crearea graficul de tip funnel plot ####
culori <- c("orange", "green"); praguri <- c("p<.05", "p<.01")
funnel.meta(moderare, studlab = F, xlim = c(-0.5, 2.1),
            col.random = "red", lwd.random = 1.5, col = "red",
            xlab = "Effect size", ylab = "Standard Error")
funnel.meta(moderare, studlab = F, xlim = c(-0.5, 2.1),
            contour = c(.95, .99), col.contour = culori,
            col.random = "red", lwd.random = 1.5, col = "red",
            xlab = "Effect size", ylab = "Standard Error")
legend(x = 1.7, y = 0.00, legend = praguri, fill = culori)
title("Moderation analysis funnel-plot")

# Testul regresiei Egger ####
metabias(aleatorii, plotit = T, method.bias = "Egger")
eggers.test(aleatorii)
metabias(aleatorii, plotit = T, method.bias = "Begg")
metabias(aleatorii, plotit = T, method.bias = "Thompson")

# Metoda Trim Fill ####
aleatorii$I2
all <- trimfill(aleatorii); summary(all)
funnel.meta(all, studlab = F, xlim = c(-1.5, 2.1),
            contour = c(.95, .99), col.contour = culori,
            col.random = "red", lwd.random = 1.5, col = "red",
            xlab = "Effect size", ylab = "Standard Error")
legend(x = 1.7, y = 0.00, legend = praguri, fill = culori)
title("Moderation analysis funnel-plot after trim and fill")
eggers.test(all)
forest(all, layout = "meta", xlim = c(-1.5, 1.5), sortvar = TE,
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

# Metoda Rücker a meta-analizei limitate ####
l.meta <- limitmeta(aleatorii); summary(l.meta)
funnel.limitmeta(l.meta, shrunken = T, xlim = c(-0.1, 1.7),
                 xlab = "Effect size", ylab = "Standard Error",
                 contour = c(.95, .99), col.contour = culori,
                 col.adjust = "red", line = T, col.line = "red",
                 col.shrunken = "blue", col = "black")
title("Moderation analysis funnel-plot after trim and fill")

# Metoda curbei „p” ####
pcurve(aleatorii, effect.estimation = T, N = efecte.agg$n)
