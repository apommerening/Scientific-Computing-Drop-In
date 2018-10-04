# Modelling of relative volume, height, diameter using the Chapman-Richards growth function. - Ap, 12.10.2014. Updated: 20.08.2018

rm(list = ls())

xPath <- "/Users/arng0001/Dropbox/Gwaith/Cyfrifiaduro/DropIn/Exercises/Session1/"
# setwd("C:/Users/arng0001/Dropbox/Gwaith/Cyfrifiaduro/DropIn/Exercises/Session1")

options(digits = 14, width = 100)

# Select data 
dataFile <- "Clg1_5000"

tdata <- read.table(paste(xPath, dataFile, ".txt", sep = ""), header = TRUE)
# tdata <- read.table(paste(dataFile, ".txt", sep = ""), header = TRUE)

names(tdata)
tdata

tdata$size <- tdata$dbh # tdata$dbh tdata$h tdata$v

# Calculate observed relative growth rates.
tdata$p <- NA
for (i in 2 : length(tdata$age)) 
  tdata$p[i] <- (log(tdata$size[i]) -  log(tdata$size[i - 1])) / (tdata$age[i] - tdata$age[i - 1])


calcGrowthFunction <- function(xdata, abdn) {
  xdata$px <- abdn[1] * abdn[2] / (1 - exp(-abdn[1] * xdata$age)) # Chapman Richards
  return(xdata)	
}

loss.L2 <- function(abdn, xdata) {
  xdata <- calcGrowthFunction(xdata, abdn)
  dev <- (xdata$p - xdata$px)^2
  return(sum(dev, na.rm = TRUE))
}


# Regression 
abdn0 <- c(-0.67, -0.068)
abdn.L2 <- optim(abdn0, loss.L2, xdata = tdata, control = list(maxit = 30000, temp = 2000, trace = TRUE, REPORT = 500))

abdn.L2$par


# Calculate statistics
dn <- calcGrowthFunction(tdata, abdn.L2$par)
dn <- dn[c("age", "px")]
(varres <- var(tdata$p[-1] - dn$px[-1], na.rm = TRUE))
(bias <- mean(tdata$p[-1] - dn$px[-1], na.rm = TRUE))
(rmse <- sqrt(varres + bias^2))
(rss <- sum((tdata$p[-1] - dn$px[-1])^2, na.rm = TRUE))
(trss <- length(tdata$p[-1]) * var(tdata$p[-1], na.rm = TRUE))
(r2 <- 1 - rss / trss)


# Show relative results
# pdf(file = paste(xpath, "paper/stemAnalysesAgeRD.pdf", sep = "")) 
par(mar = c(2, 4, 0.5, 0.5))
plot(tdata$age[-1], tdata$p[-1], las = 1, ylab = "", xlab = "", cex = .9, col = "black", pch = 16, axes = FALSE, ylim = c(0, 0.3), xlim = c(0, 60)) # ylim = c(0, 0.2), xlim = c(0, 40)
lines(tdata$age[-1], dn$px[-1], col = "red", lwd = 2, lty = 1) # red
# curve(abdn.L2$par[1] * x^abdn.L2$par[2], from = min(tdata$age[-1]), to = max(tdata$age[-1]), lwd = 4, lty = 1, col = "black", add = TRUE)
# curve(abdn.L2$par[1] * abdn.L2$par[2] / (1 - exp(-abdn.L2$par[1] * x)), from = min(tdata$age[-1]), to = max(tdata$age[-1]), lwd = 4, lty = 1, col = "black", add = TRUE)
axis(1, lwd = 2, cex.axis = 1.8, at = seq(0, 70, 10)) 
axis(2, las = 1, lwd = 2, cex.axis = 1.8)
box(lwd = 2)

