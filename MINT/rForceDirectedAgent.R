# ======================================================================================#
#-- Project Name: MINT
#-- Task : Compute force directed layout of a correlation matrix
#-- version : 1.0
#-- date : 19/MAR/13
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
#--                        8. -3  : R error in Force directed agent
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

checPkgVec[1]<-checkpackver("qgraph",'1.1.0')
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
library("qgraph")
library("RPostgreSQL")
library("futile.logger")

#=================================
# Setting futile logger thresholds
#=================================

flog.threshold(INFO, name="info_forceDir")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/forceDir_info.log",sep="")
), name="info_forceDir")
#flog.appender(appender.file("logs/forceDir_info.log"), name="info_forceDir")
flog.threshold(ERROR, name="error_forceDir")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/forceDir_error.log",sep="")), name="error_forceDir")
#flog.appender(appender.file("logs/forceDir_error.log"), name="error_forceDir")

flog.info("R libraries loaded succesfully for force directed agent.", name="info_forceDir")

#================================================
# Querying the database for symbols. 
# If connection fails, read it from a local file.
#================================================

drv <- dbDriver("PostgreSQL")

dbProperties2 <- suppressWarnings(as.vector(as.matrix(read.table(paste(mripPath,"/wd/db.properties",sep=""), header = F, sep = ",", skip = 1))))

tryCatch({con1 <- dbConnect(drv, host=dbProperties2[1], port = as.numeric(dbProperties2[2]), dbname = dbProperties2[3], user = dbProperties2[4], pass=dbProperties2[5])}, error=function(err){errorCode<-201
                                                                                                                                                                                              flog.warn("Connection to database on 162.192.100.157 failed. Reading the symbol vector from a local properties file.", name="info_forceDir")                                                                                                                                                                                              },finally={})
if (exists("con1")){
  flog.info("Connection to database 162.192.100.157 established.",  name="info_forceDir")
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
    flog.info("Symbol vector read from the local properties file succesfully",name="info_forceDir")
    flog.info("It contains %s tickers",length(symbolVec),name="info_forceDir")
    errorCode <<- 0

  } else {
    flog.fatal("Symbol vector not read in. Application fails without the Symbol Vector",name="error_forceDir")
  }
}

#======================
# initializing variables 
#======================

eJson <- paste("\"ErrorCode\":",500,sep="")
prevJson <- "{\"FD\":\"NULL\""
fieldsJson <- NULL
initforceDir <- NULL
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

#=====================
# Force Directed Graph 
#=====================

getForceDir <- function(corMat){
  
  if(is.null(initforceDir)){
    initforceDir <<- qgraph(corMat, layout = "spring", DoNotPlot = TRUE)$layout
  }
  
  layout.par=list(init=initforceDir)
  
  forceDir <- qgraph(corMat, layout = "spring", layout.par = layout.par, DoNotPlot = TRUE)$layout
  initforceDir <<- forceDir
  return(forceDir)
}

#==========================
# Creat Json for this agent
#==========================

createJson <- function(FDMat){
  
  jsonFD ="\"FD\":["
  
  jsonFD <- paste(jsonFD,"{\"id\":" ,paste( "\"", FDMat[1,1],"\"",sep=""), ",", "\"x\":", FDMat[1,2], ",", "\"y\":", FDMat[1,3],"}")  
  jss<- lapply(2:nrow(FDMat), function(i){
    jsonFD[i] <- paste("{\"id\":" ,paste( "\"", FDMat[i,1],"\"",sep=""), ",", "\"x\":", FDMat[i,2], ",", "\"y\":", FDMat[i,3],"}")})    
  jssfinal<-paste(jss,collapse=",")
  jsonFD<-paste(jsonFD,",",jssfinal, "]")
  
  jsonFD <- paste("{",jsonFD,sep="")
  return(jsonFD)
}

#=============================
# Force Directed main function
#=============================

FD_main_fn <- function(inputJson = NA){
  
  if (is.na(inputJson)){
    flog.fatal("Data not received from the correlation agent. Sending the last valid json.", name = "error_forceDir")
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
    flog.error("Received an error from the correlation agent. Sending the last valid json.", name = "error_forceDir")
    
    retJson <- paste(prevJson,",",fieldsJson,"}",sep="")
    return(retJson)
  }
  
  tryCatch(convJsonToMat(inputJson), error = function(e){
    flog.error("Json was not converted to matrix properly",name = "error_forceDir")
    errorCode <<- -3
  })
  FDMat <- getForceDir(corMat)
  
  if (errorCode == 0){
    FDMat <- cbind(symbolVec,FDMat)
    retJson <- createJson(FDMat)
    flog.info("Force Directed graph Json sent succesfully.",name="info_forceDir")
    prevJson <<- retJson
    retJson <- paste(retJson,",",fieldsJson,"}",sep="")
    return(retJson)
  } else {
    errFieldsJson <- gsub("\"ErrorCode\":[0-9]{1,10}",paste("\"ErrorCode\":",errorCode),fieldsJson)
    retJson <- paste(prevJson,",",errFieldsJson,"}",sep="")
    return(retJson)
  }
}
