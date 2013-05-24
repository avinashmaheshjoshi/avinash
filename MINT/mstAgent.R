# ======================================================================================#
#-- Project Name: MINT
#-- Task : Compute mst of a correlation matrix
#-- version : 1.0
#-- date : 22/MAR/13
#-- authors : Avinash Joshi
# ======================================================================================#

#_______________________________________________________________________________________#
#-- Glossary of Error Codes :
#--                        1. 100 : Insufficient Data/data frame charging
#--                        2. 200 : Connection to database on 162.192.100.48 failed
#--                        3. 201 : Connection to database on 162.192.100.157 failed
#--                        4. 10  : R version error
#--                        5. 11  : R package installation error
#--                        6. 300 : Optimization error in community detection.
#--                        7. -1  : Unknown error
#--                        8. -4  : R error in mst agent
#--                        9. 400 : Connection to activeMQ on 162.192.100.48 failed
#--                        10. 500 : Data not received from correlations agent
#--                        10.  0  : No error
#_______________________________________________________________________________________#

#==============================
# Get the environment variables
#==============================
mripPath = Sys.getenv("MRIP_HOME")

errorCode<-0
checPkgVec<-0

#====================================
# Check R version and package version
#====================================

checkRversion<-function(){
  r<-getRversion()
  if (r<'2.15.1'){
    return(10)
  } else {
    return(0)
  }
}
packinfo <- installed.packages (fields = c ("Package", "Depends", "Version")) 

checkpackver<-function(pack_name,req_version)
{
  req_ver<-unlist(strsplit(req_version,"[[:punct:][:space:]]+"))
  count<-0
  tryCatch({
    version<-packinfo[pack_name,3]
    p<-unlist(strsplit(version,"[[:punct:][:space:]]+"))
  
    if(length(p) > length(req_ver) ){
      req_ver = c(req_ver,rep("0",length(p) - length(req_ver)))
    }
    if(length(p) < length(req_ver) ){
      p = c(p,rep("0",length(req_ver) - length(p)))
    }
    
    for(i in 1:length(p))
    {
      if (p[i]>req_ver[i])  return(0)
      else if(p[i]<req_ver[i]) stop()
    }
    return(0)
  },error=function(err)
  {
    while(!(pack_name %in% packinfo[,1]) || count<3){
      install.packages(pack_name,dependencies=TRUE)
      packinfo <- installed.packages (fields = c ("Package", "Depends", "Version"))
      count<<-count+1
    }
    if (count==3){
      #    cat("package",pack_name,"could not be installed.. Please try after some time")
      return(return(11))
    }
    
  },finally={})
}

errorCode<-checkRversion()

checPkgVec[1]<-checkpackver("ape",'3.0-6')
checPkgVec[2]<-checkpackver("rjson",'0.2.11')
checPkgVec[3]<-checkpackver("RPostgreSQL",'0.3-3')
checPkgVec[4]<-checkpackver("futile.logger",'1.3.0')


if (any(checPkgVec!=0)){
  errorCode<<-11
}

#==============================
# Loading the required libraries
#==============================

library("rjson")
library("ape")
library("RPostgreSQL")
library("futile.logger")

#=================================
# Setting futile logger thresholds
#=================================

flog.threshold(INFO, name="info_mst")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/mst_info.log",sep="")), name="info_mst")
#flog.appender(appender.file("logs/mst_info.log"), name="info_mst")
flog.threshold(ERROR, name="error_mst")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/mst_error.log",sep="")), name="error_mst")
#flog.appender(appender.file("logs/mst_error.log"), name="error_mst")

flog.info("R libraries loaded succesfully for mst agent.", name="info_mst")

#================================================
# Querying the database for symbols. 
# If connection fails, read it from a local file.
#================================================

drv <- dbDriver("PostgreSQL")

dbProperties2 <- suppressWarnings(as.vector(as.matrix(read.table(paste(mripPath,"/wd/db.properties",sep=""), header = F, sep = ",", skip = 1))))

tryCatch({con1 <- dbConnect(drv, host=dbProperties2[1], port = as.numeric(dbProperties2[2]), dbname = dbProperties2[3], user = dbProperties2[4], pass=dbProperties2[5])}, error=function(err){errorCode<-201
                                                                                                                                                                                              flog.warn("Connection to database on 162.192.100.157 failed. Reading the symbol vector from a local properties file.", name="info_mst")                                                                                                                                                                                              },finally={})
if (exists("con1")){
  flog.info("Connection to database 162.192.100.157 established.",  name="info_mst")
  rs <- dbSendQuery(con1, statement = "select dep_tickers from jade_agents")
  dat <- fetch(rs, n=-1)
  symbolVec <- unlist(strsplit(dat[1,], ","))
  if (exists("symbolVec") && class(symbolVec) == "character"){
    flog.info("Symbol vector queried succesfully")
  } 
  dbDisconnect(con1)
  dbClearResult(rs)
} else {
  errorCode <<- 201
}
if (errorCode==201){
  symbolVec <- suppressWarnings(as.vector(as.matrix(read.table(paste(mripPath,"/wd/prediction.properties",sep=""), header = F, sep = ",", skip = 3))))
  if (exists("symbolVec") && class(symbolVec) == "character"){
    flog.info("Symbol vector read from the local properties file succesfully",name="info_mst")
    flog.info("It contains %s tickers",length(symbolVec),name="info_mst")
    errorCode <<- 0

  } else {
    flog.fatal("Symbol vector not read in. Application fails without the Symbol Vector",name="error_mst")
  }
}

#======================
# initializing variables 
#======================

eJson <- paste("\"ErrorCode\":",500,sep="")
prevJson <- "{\"mst\":\"NULL\""
fieldsJson <- NULL
nSymbols <- length(symbolVec)
corMat <- matrix(1,nSymbols,nSymbols)
colnames(corMat) <- rownames(corMat) <- symbolVec

#=============================================
# Helper function for function convJsonToMat
# Will be used as a part of an apply statement
#=============================================

convStrngToRow <- function(strng){
  
  src <- as.numeric(gsub(".*?source.*?: (.*?) ,.*","\\1",strng)) + 1
  to <- as.numeric(gsub(".*?target.*?: (.*?) ,.*","\\1",strng)) + 1
  value <- as.numeric(gsub(".*?value.*?: (.*?) .*","\\1",strng))
  corMat[src,to] <<- value
  corMat[to,src] <<- value
  
}

#==================================
# Converting the Json into a matrix
#==================================

convJsonToMat <- function(inputJson){
  
  drpExtraInfo = strsplit(inputJson,"nodes",fixed=TRUE)[[1]][1]
  corJson = strsplit(drpExtraInfo,split="[{}]")[[1]]
  corJson = corJson[seq(from=3,to=length(corJson),by=2)]
  tmp = sapply(corJson, FUN=convStrngToRow)
  rm(tmp)
}

#========================
# Function to get the MST 
#========================

getMst <- function(distMat,corMat){
  mstMat <- mst(distMat) * corMat
  return(mstMat)
}

#===========================
# Create Json for this agent
#===========================

createJson <- function(mstMat){
  
  jsonMst ="\"mst\":["
  
  values<- mstMat[lower.tri(mstMat, diag = F)]
  dims <- which(lower.tri(mstMat, diag = F), arr.ind =T)-1
  
  jsonMst[1]<-paste(jsonMst, "{\"source\":", dims[1,2],",", "\"target\":", dims[1,1],",", "\"value\":", values[1],"}")
  jsc<-lapply(2:length(values), function(i)
    jsonMst[i]<- paste("{\"source\":", dims[i,2],",", "\"target\":", dims[i,1],",", "\"value\":", values[i],"}")) 
  jscfinal<-paste(jsc,sep=",",collapse=",")
  jsonMst<-paste(jsonMst,",",jscfinal, "]",sep="")
  
  jsonMst <- paste("{",jsonMst,sep="")
  
  return(jsonMst)
}

#==================
# Mst main function
#==================

mst_main_fn <- function(inputJson = NA){
  
  if (is.na(inputJson)){
    flog.fatal("Data not received from the correlation agent. Sending the last valid json.", name = "error_mst")
    if(is.null(fieldsJson)){
      eJson <- paste("\"ErrorCode\":",500,sep="")
      retJson <- paste(prevJson,",",eJson,"}",sep="")
      return(retJson)
    }
    errFieldsJson <- gsub("\"ErrorCode\":[0-9]{1,10}","\"ErrorCode\":500",fieldsJson)
    retJson <- paste(prevJson,",",errFieldsJson,"}",sep="")
    return(retJson)
  }
  
  errorCode <<- as.numeric(gsub(".*?ErrorCode.*?:(.*?)}","\\1",inputJson))
  fieldsJson <<- paste("\"TimeStamp\"",gsub(".*?\"TimeStamp\"(.*?)}","\\1",inputJson),sep="")
  
  if (errorCode != 0){
    flog.error("Received an error from the correlation agent. Sending the last valid json.", name = "error_mst")
    
    retJson <- paste(prevJson,",",fieldsJson,"}",sep="")
    return(retJson)
  }
  
  tryCatch(convJsonToMat(inputJson), error = function(e){
    flog.error("Json was not converted to matrix properly",name = "error_mst")
    errorCode <<- -4
  })
  
  corMat <- round(corMat, digits=3)
  distMat <- sqrt(2*(1-abs(corMat)))
  
  #Get the weighted mst graph.
  mstMat <- getMst(distMat,corMat)

  if (errorCode == 0){
    
    retJson <- createJson(mstMat)
    flog.info("Mst graph Json sent succesfully.",name="info_mst")
    prevJson <<- retJson
    retJson <- paste(retJson,",",fieldsJson,"}",sep="")
    return(retJson)
  } else {
    errFieldsJson <- gsub("\"ErrorCode\":[0-9]{1,10}",paste("\"ErrorCode\":",errorCode),fieldsJson)
    retJson <- paste(prevJson,",",errFieldsJson,"}",sep="")
    return(retJson)
  }
}
