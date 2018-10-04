rm(list = ls())
options(digits = 6, width = 50)

setwd("/Users/arng0001/Dropbox/Gwaith/Cyfrifiaduro/DropIn/Exercises/Session3")
incRegres <- read.table("giRegresDF.txt", header = TRUE)
names(incRegres)
range(incRegres$year)
tapply(incRegres$year, incRegres$Plot, min)
tapply(incRegres$year, incRegres$Plot, max)

plotno <- c("plot1", "plot2", "plot3", "plot4", "plot5", "plot6")
surveyYears <- c(1988, 1992, 1997, 2004)

dummy <- incRegres
incRegres <- incRegres[incRegres$Plot == 2, ]
# incRegres <- dummy
tail(incRegres)
incRegres <- incRegres[incRegres$RGR > 0, ]

# install.packages("quantreg", dep = T)
library(quantreg)
nlsout <- nlrq(RGR ~ a * dbh^b, data = incRegres, start = list(a = 0.67, b = -0.068), 
               tau = 0.05, trace = TRUE, na.omit) # 0.975
summary(nlsout)

param <- c(summary(nlsout)$coefficients[1], summary(nlsout)$coefficients[2])

# Plot RGR over size
pdf(file = "MortalityRGR.pdf")
par(mar = c(2.0, 3.8, 0.5, 0.5))
plot(incRegres$dbh, incRegres$RGR, axes = FALSE, ylab = "", xlab = "", main = "", pch = 16, cex = 1, ylim = c(0, 0.20), xlim = c(0, 85))
# lines(incRegres$dbh, dn$px, col = "red", lwd = 2, lty = 1)
curve(param[1] * x^param[2], from = min(incRegres$dbh, na.rm = T), to = max(incRegres$dbh, na.rm = T), lwd = 4, lty = 1, col = "red", add = TRUE)
# curve(abdn.L2$par[1] * x^(abdn.L2$par[2] - 1) * exp(abdn.L2$par[3] * x), from = min(incRegres$dbh), to = max(incRegres$dbh), lwd = 4, lty = 1, col = "red", add = TRUE)
axis(side = 1, lwd = 2, las = 1, cex.axis = 1.7)
axis(side = 2, lwd = 2, las = 1, cex.axis = 1.7)
box(lwd = 2)
dev.off()

