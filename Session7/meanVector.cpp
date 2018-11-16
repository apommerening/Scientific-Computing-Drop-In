/*
 *  meanVector.cpp
 *  
 *
 *  Created by Arne Pommerening on 12/01/2013.
 *  Copyright 2013 Philodendron International. All rights reserved.
 *
 */

#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double meanVector(NumericVector a) {
    
    int n = a.size();
    double xsum = 0;
	
    for (int i = 0; i < n; i++)
            xsum += a[i];
    
    return xsum / n;
}
