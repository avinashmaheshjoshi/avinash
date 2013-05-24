#===========================================================================
#-- Project Name: MINT
#-- Task : Profile the mint code for time for generating the daily report
#-- version : 1.0
#-- Date : 13/Mar/13
#-- Authors : Avinash Joshi
#-- Dependent Files : 1) retDf.csv 
#--                   2) mint_main_fn.R
#-- Description : 
#===========================================================================


#=============================================================
# Check if "RUnit" package has been installed if not install it
#=============================================================

if(!is.element("profr", installed.packages())){
  install.packages("profr",repos="http://lib.stat.cmu.edu/R/CRAN")
}

#=========================================================
# Load required libraries and source the files to be tested
#=========================================================

library("profr")
library("reshape2")
library("ggplot2")
source("mint_main_fn.R")
retDf = read.csv("tests/dummyDataFrame_Charged.csv",header = TRUE)
rawFrameCount = 10000
fillCount = 2
Rprof("timeProfMint.out",memory.profiling=TRUE)
json_output = mint_main_fn()
Rprof(NULL)

profResults = summaryRprof("timeProfMint.out",memory="both")
profResults = profResults$by.total
profResults$function_id = rownames(profResults)

fnsProfiled = rownames(profResults)
fnsProfiled = gsub("\"","",fnsProfiled)

listOfFns = lsf.str()

rlvntProfResults = profResults[which(fnsProfiled %in% listOfFns),]

actualTime = rlvntProfResults[,c(1,4,6)]

pctTime = rlvntProfResults[,c(2,5,6)]

memVal = rlvntProfResults[,c(3,6)]

melt_actualTime = melt(actualTime,id.vars="function_id")
melt_actualTime$function_id = factor(melt_actualTime$function_id,levels=melt_actualTime$function_id,ordered=TRUE)

melt_pctTime = melt(pctTime, id.vars="function_id")
melt_pctTime$function_id = factor(melt_pctTime$function_id,levels=melt_pctTime$function_id,ordered=TRUE)

melt_memVal = melt(memVal, id.vars="function_id")
melt_memVal$function_id = factor(melt_memVal$function_id,levels=melt_memVal$function_id,ordered=TRUE)

actualTime_plot = ggplot(data=melt_actualTime,aes(x=function_id,y=value,fill=variable)) + geom_bar(stat = "identity", position = "dodge",width=0.5) + xlab("Function Names") + ylab("Time taken in seconds") + theme(axis.text.x = element_text(angle=90),axis.text.y = element_text(size = 15),axis.title.x = element_text(size = 15,face="bold"),legend.title = element_blank())

pctTime_plot = ggplot(data=melt_pctTime,aes(x=function_id,y=value,fill=variable)) + geom_bar(stat = "identity", position = "dodge",width=0.5) + xlab("Function Names") + ylab("Percentage time taken") + theme(axis.text.x = element_text(angle=90),axis.text.y = element_text(size = 15),axis.title.x = element_text(size = 15),legend.title = element_blank())

memVal_plot =  ggplot(data=melt_memVal,aes(x=function_id,y=value,fill=variable)) + geom_bar(stat = "identity", position = "dodge",width=0.5) + xlab("Function Names") + ylab("Memory in Megabytes") + theme(axis.text.x = element_text(angle=90),axis.text.y = element_text(size = 15),axis.title.x = element_text(size = 15),legend.title = element_blank())