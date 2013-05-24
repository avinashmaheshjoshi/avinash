#===========================================================================
#-- Project Name: MINT Log returns correlation agent
#-- Task : Convert the different test cases for mint into a test suite and
#          run it.
#-- version : 1.0
#-- date : 20/Mar/13
#-- authors : Karthik Matta
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
source("correlationAgentLogRetMINT.R")
source("testFunctions.R")

#_______________________Create test data__________________________________________
testLRData <- as.matrix(data.frame(v1=c(1:1000), v2=2*c(1:1000), v3=3*c(1:1000), v4=4*c(1:1000)))
correctReturns <- as.matrix(read.csv("TestCorAgent/correctReturns.csv"))
correctLogs <- as.matrix(read.csv("TestCorAgent/correctLogs.csv"))
correctLogReturns <- as.matrix(read.csv("TestCorAgent/correctLogReturns.csv"))
testData = read.csv("TestCorAgent/simTestData.csv")
testData_known_corOutput = matrix(read.csv("TestCorAgent/simTestData_cor.csv"),)

test.suite <- defineTestSuite("corLogRet",
                              dirs = file.path("TestCorAgent/"),
                              testFileRegexp = '^test\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result, fileName ="Test_MINT_CorLogRet.txt")