############################################################################# 
#-- Project Name: MINT
#-- Task : Write test cases for different modules used in mint code
#-- version : 1.0
#-- date : 19/Feb/13
#-- authors : Avinash Joshi
#-- Description : This file contains a set of test cases to be run on mint 
#--               code. This file is sourced in runTests_mintProto.R
#############################################################################

test.consumeFn <- function(){
 checkEquals("character",class(consumeFn()))
}

