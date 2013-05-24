#===========================================================================
#-- Project Name: MINT
#-- Task : Convert the different test cases for mint into a test suite and
#          run it.
#-- version : 1.0
#-- date : 28/Feb/13
#-- authors : Avinash Joshi/Karthik Matta
#-- Description : This file requires a folder called "tests" in the current
#--               workspace. It checks all the files starting with "test" in 
#--               the folder tests and take all the test functions and make
#--               a test suite. It runs and prints the results too.
#===========================================================================


#=====================================================================
# Enusre that the "tests" folder and the mint proto code file can be 
# found in your current workspace
# Or set the required workspace
#=====================================================================
#setwd("trunk/LabsRepo/Labs/code/MINT/agencies/proto_agency/Continuous_Build/")

#=============================================================
# Check if "RUnit" package has been installed if not install it
#=============================================================

if(!is.element("RUnit", installed.packages())){
  install.packages("RUnit",repos="http://lib.stat.cmu.edu/R/CRAN")
}

#=========================================================
# Load required libraries and source the files to be tested
#=========================================================

library("RUnit")
source("mint_main_fn.R")


#_______________________Create test data__________________________________________
testLRData <- as.matrix(data.frame(v1=c(1:1000), v2=2*c(1:1000), v3=3*c(1:1000), v4=4*c(1:1000)))
correctReturns <- as.matrix(read.csv("tests/correctReturns.csv"))
correctLogs <- as.matrix(read.csv("tests/correctLogs.csv"))
testData = read.csv("tests/simTestData.csv")
testData_known_corOutput = matrix(read.csv("tests/simTestData_cor.csv"))

#Make the test suite

test.suite <- defineTestSuite("mintProto",
                              dirs = file.path("tests"),
                              testFileRegexp = '^test\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result,"test_logs.txt",showDetails=TRUE)

nTestFncs = test.result$mintProto$nTestFunc

unlistTestResult = unlist(test.result$mintProto$sourceFileResults)

resultsTable = t(matrix(unlistTestResult,nrow=3,ncol=nTestFncs))

if(any(nchar(resultsTable[,2]) != 1)){
  resultsTable[which(nchar(resultsTable[,2]) != 1),2] = resultsTable[which(nchar(resultsTable[,2]) != 1),3]
}

testFuncNames = unique(sapply(names(unlistTestResult), FUN=function(x) gsub(".*?test\\.(.*?)\\..*","\\1",x)))

resultsTable = cbind(testFuncNames,resultsTable[,c(-2,-3)])

colnames(resultsTable) = c("Test Function Name","Test Result")

