#===========================================================================
#-- Project Name: MINT
#-- Task : Convert the different test cases for News Agent into a test suite and
#          run it.
#-- version : 1.0
#-- date : 10/Mar/13
#-- author : Karthik Matta
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
source("NewsAgent.R")

dummyEmpty <- read.csv("dummyDf.csv")
dummyJson <- read.csv("dummyJson.csv")

test.suite <- defineTestSuite("NewsAgent",
                              dirs = file.path("tests"),
                              testFileRegexp = '^test\\d+\\.R')

test.result <- runTestSuite(test.suite)

