#=========================================================
# Functions to test
#=========================================================

computeReturns <- function(testDf){
  return(diff(as.matrix(testDf)))  
}

computeLog <- function(testData, base = exp(1)){
  return(log(as.matrix(testData), base = base))  
}

