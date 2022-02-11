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
all <- trimfill(aleatorii); summary(all)
all <- trimfill(all); summary(all)
funnel.meta(all)
eggers.test(all)

metafor::rma.uni(es, se^2, mods = ~se, data = ds.global, method = "FE")

l.meta <- limitmeta(aleatorii); summary(l.meta)
funnel.limitmeta(l.meta, shrunken = T, xlim = c(0.0, 1.6))

pcurve(aleatorii)
