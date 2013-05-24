############################################################################# 
#-- Project Name: MINT
#-- Task : Write test cases for different modules used in mint code
#-- version : 1.0
#-- date : 19/Feb/13
#-- authors : Avinash Joshi
#-- Description : This file contains a set of test cases to be run on mint 
#--               code. This file is sourced in runTests_mintProto.R
#############################################################################

# test.computeLogReturns <- function(){
#   testLogReturns <- computeLogReturns(testLRData)
#    
#   #checkEquals(class(correctLogReturns),class(testLogReturns))
#   
#   checkEquals(dim(testLogReturns),dim(correctLogReturns))
#   
#   for(i in 1:nrow(testLogReturns)){
#     checkEquals(correctLogReturns[i,],testLogReturns[i,], tolerance = 1e-6)
#   }
# }

test.mint_main_fn = function(){

  checkEquals("character",class(mint_main_fn()))

}

test.getCor = function(){
  
  testCorMat <<- getCor(testData)
  
  checkEquals(class(testData_known_corOutput),class(testCorMat))
  
  checkEquals(c(10,10),dim(testCorMat))
  
  checkEquals(100,length(testCorMat))
  
  checkTrue(all(testCorMat >= -1 && testCorMat <= 1))
  
}

test.getSammonsXYZ3D = function(){
  
  testCorMat_1 = cbind(testCorMat,1)
  
  #Sammons should return -1 as the matrix is not a square matrix
  checkEquals(-1,getSammonsXYZ3D(testCorMat_1))
  
  testCorMat_2 = testCorMat
  testCorMat_2[1,2] = 10
  
  #Sammons should return -1 as the matrix is not symmetric
  checkEquals(-1,getSammonsXYZ3D(testCorMat_2))
  
  checkEquals(c(10,3),dim(getSammonsXYZ3D(testCorMat,dimension=3)))
  
  checkEquals("matrix",class(getSammonsXYZ3D(testCorMat,dimension=3)))
  
  #inserting NA in the data set the one with NA should be last
  testCorMat_3 = testCorMat
  testCorMat_3[1,2] = NA
  testCorMat_3 = cor(testCorMat_3)
  checkEquals("X2",rownames(getSammonsXYZ3D(testCorMat_3,dimension=3))[10])
  
}
test.getSammonsXYZ2D = function(){
  
  testCorMat_1 = cbind(testCorMat,1)
  
  #Sammons should return -1 as the matrix is not a square matrix
  checkEquals(-1,getSammonsXYZ2D(testCorMat_1))
  
  testCorMat_2 = testCorMat
  testCorMat_2[1,2] = 10
  
  #Sammons should return -1 as the matrix is not symmetric
  checkEquals(-1,getSammonsXYZ2D(testCorMat_2))
  
  checkEquals(c(10,2),dim(getSammonsXYZ2D(testCorMat,dimension=2)))
  
  checkEquals("matrix",class(getSammonsXYZ2D(testCorMat,dimension=2)))
  
  #inserting NA in the data set the one with NA should be last
  testCorMat_3 = testCorMat
  testCorMat_3[1,2] = NA
  testCorMat_3 = cor(testCorMat_3)
  checkEquals("X2",rownames(getSammonsXYZ2D(testCorMat_3,dimension=2))[10])
  
}

test.getForcDir = function(){
  
  checkEquals(c(10,2),dim(getForceDir(testCorMat)))
  
  checkEquals("matrix",class(getForceDir(testCorMat)))
  
  checkEquals("numeric",class(getForceDir(testCorMat)[,1]))
  
  
}

test.getMst = function(){
  
  testDistMat = 1 - abs(testCorMat)
  
  testAdjMat <<- getMst(testDistMat)
    
  checkEquals(dim(testCorMat),dim(testAdjMat))
  
  checkTrue(testAdjMat == 0 || testAdjMat == 1)
  
  #Check if the number of links is 18
  checkEquals(18,sum(testAdjMat))
  
}

test.getCommCent = function(){
  
  testCorMat <<- cor(testData)
  testAdjMat <<- getMst(testDistMat)
  
  #Should return error 300 if incorrect arguments
  checkEquals(300,getCommCent(testCorMat))
  
  commCent = getCommCent(testCorMat,testAdjMat)

  checkEquals("communities",class(commCent[[1]]))
  
  checkEquals("integer",class(commCent[[2]]))
  
  checkEquals(3,length(commCent[[2]]))
  
}
