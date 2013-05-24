############################################################################# 
#-- Project Name: MINT correlation agent for log returns
#-- Task : Write test cases for different modules used in mint code
#-- version : 1.0
#-- date : 19/Mar/13
#-- authors : Karthik Matta
#-- Description : This file contains a set of test cases to be run on mint 
#--               code. This file is sourced in runTests_mintProto.R
#############################################################################

test.computeLogReturns <- function(){
  testLogReturns <- computeLogReturns(testLRData)
   
  #checkEquals(class(correctLogReturns),class(testLogReturns))
  
  checkEquals(dim(testLogReturns),dim(correctLogReturns))
  
  for(i in 1:nrow(testLogReturns)){
    checkEquals(correctLogReturns[i,],testLogReturns[i,], tolerance = 1e-6)
  }
}



test.getCor = function(){
  
  testCorMat <<- cor(testData)
  
  checkEquals(class(testData_known_corOutput),class(testCorMat))
  
  checkEquals(c(10,10),dim(testCorMat))
  
  checkEquals(100,length(testCorMat))
  
  checkTrue(all(testCorMat >= -1 && testCorMat <= 1))
  
}


test.mint_main_fn = function(){
  checkEquals("character",class(mint_main_fn()))
  
}

