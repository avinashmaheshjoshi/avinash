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
#setwd("~/Mint_Prototype/")

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
source("mint_main_fn_v5.R")


#_______________________Create test data__________________________________________
testLRData <- as.matrix(data.frame(v1=c(1:1000), v2=2*c(1:1000), v3=3*c(1:1000), v4=4*c(1:1000)))
correctReturns <- as.matrix(read.csv("tests/correctReturns.csv"))
correctLogs <- as.matrix(read.csv("tests/correctLogs.csv"))
correctLogReturns <- as.matrix(read.csv("tests/correctLogReturns.csv"))
testData = read.csv("tests/simTestData.csv")
testData_known_corOutput = matrix(read.csv("tests/simTestData_cor.csv"),)

test.suite <- defineTestSuite("mintProto",
                              dirs = file.path("tests"),
                              testFileRegexp = '^test\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result, fileName ="Test_MINT_Results.txt")