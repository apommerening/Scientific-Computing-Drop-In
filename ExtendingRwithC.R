rm(list = ls())	

# install.packages("Rcpp", dep = TRUE, lib = "C:/Program Files/R/R-3.3.0")
# install.packages("devtools", dep = TRUE, lib = "C:/Program Files/R/R-3.3.0")
# library(Rcpp, lib.loc="C:/Program Files/R/R-3.3.0")
library(Rcpp)
Rcpp::evalCpp("2 + 2")
sourceCpp("/Users/arng0001/Dropbox/Gwaith/Cyfrifiaduro/DropIn/Sessions/Session7/meanVector.cpp")

v <- c(1.5, 2.9, 3.2, 4.1, 5.5)
mean(v)
meanVector(v)
