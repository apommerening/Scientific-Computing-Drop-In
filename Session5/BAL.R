rm(list = ls())
options(digits=6, width = 50)

bal <- function(ba, area) {
  sumba <- sum(ba)
  basmaller <- 0
  pix <- 0
  bal <- 0
  for (i in 1 : length(ba)) {
    bax <- ba[i]
    basmaller <- sum(ba[ba <= bax])
    pix <- basmaller / sumba
    bal[i] <- sumba * (1 - pix) / area
  }
  return(bal)
}

xPath <- "/Users/arng0001/Dropbox/Gwaith/Cyfrifiaduro/DropIn/Sessions/Session5/"
myData <- read.table(paste(xPath, "Mand1.txt", sep = ""), header = TRUE)
names(myData)

# Calculate basal area of individiual trees
myData$ba <-  pi * (myData$dbh / 200) ^ 2

# Calculate BAL using the function from above
xmax <-  80 
ymax <-  80 
xarea <- xmax * ymax / 10000
myData$bal <- bal(myData$ba, xarea)
sum(myData$ba) / xarea # Total basal area per hectare
  
# Plot BAL over dbh
table(myData$Species)
colour <- ifelse(myData$Species == 1, "black", "red") # Visualise species: 1 - Quercus petraea, 2: Fagus sylvatica
par(mar = c(2, 3.1, 0.5, 0.5))
plot(myData$dbh, myData$bal, pch = 16, col = colour, ylab = "", xlab = "", main = "", axes = F, ylim = c(0, 30), xlim = c(0, 60))
axis(1, lwd = 2, cex.axis = 1.8)
axis(2, las = 1, lwd = 2, cex.axis = 1.8)
box(lwd = 2)

