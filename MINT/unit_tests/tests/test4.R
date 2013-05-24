############################################################################# 
#-- Project Name: MINT - News agent
#-- Task : Write test cases for different modules used in News Agent
#-- version : 1.0
#-- date : 10/Mar/13
#-- authors : Karthik Matta
#-- Description : This file contains a set of test cases to be run on mint 
#--               code. This file is sourced in runTests_NewsAgent.R
#############################################################################


test.getNewsFromGoogle <- function(){
  testData <- getNewsFormGoogle("TYX", 4)
  checkEquals(class(testData), "data.frame")
  testData <- getNewsFormGoogle("VIX", 4)
  checkEquals(class(testData), "data.frame")
}


test.getNewsFromYahoo <- function(){
  testData <- getNewsFormYahoo("TYX", 4)
  checkEquals(class(testData), "data.frame")
  testData <- getNewsFormYahoo("VIX", 4)
  checkEquals(class(testData), "data.frame")
}

test.getNewsFromNASDAQ <- function(){
  testData <- getNewsFormNASDAQ("TYX", 4)
  checkEquals(class(testData), "data.frame")
  testData <- getNewsFormNASDAQ("VIX", 4)
  checkEquals(class(testData), "data.frame")
}

test.getNewsFromFC<- function(){
  testData <- getNewsFormFC("TYX", 4)
  checkEquals(class(testData), "data.frame")
  testData <- getNewsFormFC("VIX", 4)
  checkEquals(class(testData), "data.frame")
}

test.getNewsFromFC<- function(){
  testData <- getNewsFormFC("financial", 4)
  checkEquals(class(testData), "list")
}

test.makeNewsJson<- function(){
  testData <- makeNewsJson(dummyJson)
  checkEquals(class(testData), "character")
}

test.getNewsMain<- function(){
  testData <- getNewsMain(number = 1, from = "google")
  checkEquals(class(testData), "character")
  testData <- getNewsMain(number = 1)
  checkEquals(class(testData), "character")
  testData <- getNewsMain(number = 1, from = "yahoo")
  checkEquals(class(testData), "character")
  testData <- getNewsMain(number = 1, from = "nasdaq")
  checkEquals(class(testData), "character")
}