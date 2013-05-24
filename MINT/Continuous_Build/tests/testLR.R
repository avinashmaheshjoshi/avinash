############################################################################# 
#-- Project Name: MINT
#-- Task : Write test cases for different modules used in mint code
#-- version : 1.0
#-- date : 28/Feb/13
#-- authors : Karthik
#-- Description : This file contains a set of test cases to be run on mint 
#--               code. This file is sourced in runLRTests.R
#############################################################################

test.computeLog <- function(){
  testLog <- computeLog(testLRData)
   
  checkEquals(dim(testLog),dim(correctLogs))
  
  for(i in 1:nrow(testLog)){
    checkEquals(correctLogs[i,],testLog[i,], tolerance = 1e-6)
  }
}

test.computeReturns <- function(){
  testReturns <- computeReturns(testLRData)
   
  checkEquals(dim(testReturns),dim(correctReturns))
  
  for(i in 1:nrow(testReturns)){
    checkEquals(correctReturns[i,],testReturns[i,], tolerance = 1e-6)
  }
}