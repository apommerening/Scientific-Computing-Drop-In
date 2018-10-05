# Code for session 4, 05.10.2018; Using functions in R.
rm(list = ls())

xPath <- "/Users/arng0001/Dropbox/Gwaith/Cyfrifiaduro/DropIn/Exercises/Session4/"
# setwd("C:/Users/arng0001/Dropbox/Gwaith/Cyfrifiaduro/DropIn/Exercises/Session4")

plotName <- "Plot1.txt" 
myData <- read.table(paste(xPath, plotName, sep = ""), header = T)
# tdata <- read.table(paste(plotName, ".txt", sep = ""), header = TRUE)
names(myData)
table(myData$Species)


# Coefficient of variation of all trees
sd(myData$dbh) / mean(myData$dbh)

# Coefficient of variation of species 1
sd(myData$dbh[myData$Species == 1]) / mean(myData$dbh[myData$Species == 1])

# Coefficient of variation of species 2
sd(myData$dbh[myData$Species == 2]) / mean(myData$dbh[myData$Species == 2])

# Function for calculating the coefficient of variation
cv <- function(x) {
  return(sd(x, na.rm = T) / mean(x, na.rm = T))
}

# Try it
cv(myData$dbh)

# By species
tapply(myData$dbh, myData$Species, cv)

# Apply the function to height
cv(myData$height)
tapply(myData$height, myData$Species, cv)

# Apply the function to a new variable, i.e. the height-diameter ratio
myData$hd <- 100 * myData$height / myData$dbh
head(myData)
cv(myData$hd)
tapply(myData$hd, myData$Species, cv)
