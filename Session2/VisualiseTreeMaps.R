# Code for session 2, 14.09.2018; Using spatstat for mapping trees.
rm(list = ls())

xPath <- "/Users/arng0001/Dropbox/Gwaith/Cyfrifiaduro/DropIn/Exercises/Session2/"
# setwd("C:/Users/arng0001/Dropbox/Gwaith/Cyfrifiaduro/DropIn/Exercises/Session2")

plotName <- "Plot1.txt" 
myData <- read.table(paste(xPath, plotName, sep = ""), header = T)
# tdata <- read.table(paste(plotName, ".txt", sep = ""), header = TRUE)
names(myData)
table(myData$Species)


# Check your coordinates, they should be "centred" towards (0, 0).
range(myData$x)
range(myData$y)

# Simple map without spatstat
# Without considering marks
par(mar = c(0.5, 0.5, 0.5, 0.5))									
plot(myData$x, myData$y, main = "", lwd = 2, xaxt='n', yaxt = 'n', ann = FALSE) 
# Considering dbh marks
par(mar = c(0.5, 0.5, 0.5, 0.5))									
plot(myData$x, myData$y, main = "", lwd = 2, pch = 1, cex = myData$dbh / 10, xaxt = 'n', yaxt = 'n', ann = FALSE) 
# Considering dbh marks and species
myData$colour <- c("blue") # ash, 1
myData$colour[myData$Species == 2] <- c("green") # sycamore, 2
myData$colour[myData$Species > 2] <- c("orange") # everything else (hazel, holly)
tail(myData)
par(mar = c(0.5, 0.5, 0.5, 0.5))									
plot(myData$x, myData$y, main = "", lwd = 2, pch = 16, cex = myData$dbh / 10, xaxt = 'n', yaxt = 'n', ann = FALSE, col = myData$colour) 
points(myData$x, myData$y, cex = myData$dbh / 10)
text(myData$x + 0.3, myData$y - 0.3, myData$Number, cex = 0.9) # Number trees?


# More sophisticated maps in spatstat
# install.packages("spatstat", dep = T)
library(spatstat)										
xwindow <-  owin(c(0, 31), c(0, 31)) # xmax = 31, ymax = 31 m

# Simple map regardless of species
par(mar = c(0, 0, 0, 0))									
myDatap <- ppp(x = myData$x, y = myData$y, window = xwindow, marks = myData$dbh) 
plot(myDatap, main = "", legend = T, bg = "white", fg = "black", markscale = 0.04, lwd = 2) 

# Considering dbh marks and species
# pdf(file = paste(filePath, "MapPlot1.pdf", sep = ""))
par(mar = c(0, 0, 0, 0))									
myData1 <- subset(myData, myData$Species == 1)		
myDatap1 <- ppp(x = myData1$x, y = myData1$y, window = xwindow, marks = myData1$dbh) 
plot(myDatap1, main = "", legend = F, bg = "blue", fg = "black", markscale = 0.04, lwd = 2) # bg = "white"
myData2 <- subset(myData, myData$Species == 2)		
myDatap2 <- ppp(x = myData2$x, y = myData2$y, window = xwindow, marks = myData2$dbh) 
plot(myDatap2, main = "", legend = F, bg = "green", fg = "black", markscale = 0.04, lwd = 2, add = T) # bg = "white"
myData3 <- subset(myData, myData$Species > 2)		
myDatap3 <- ppp(x = myData3$x, y = myData3$y, window = xwindow, marks = myData3$dbh) 
plot(myDatap3, main = "", legend = F, bg = "orange", fg = "black", markscale = 0.04, lwd = 2, add = T) # bg = "white"
text(myData$x + 0.3, myData$y - 0.3, myData$Number, cex = 0.9) # Number trees?
# dev.off()




