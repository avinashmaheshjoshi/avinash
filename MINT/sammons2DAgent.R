# ======================================================================================#
#-- Project Name: MINT
#-- Task : Compute 2D Sammons projection of a correlation matrix
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
#--                        8. -2  : R error in Sammons agent
#--                        9. 400 : Connection to activeMQ on 162.192.100.48 failed
#--                        10. 500 : Data not received from correlations agent
#--                        11.  0  : No error
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

checPkgVec[1]<-checkpackver("MASS",'7.3-22')
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
library("MASS")
library("RPostgreSQL")
library("futile.logger")

#=================================
# Setting futile logger thresholds
#=================================

flog.threshold(INFO, name="info_sam2D")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/sammons_2D_info.log",sep="")
), name="info_sam2D")
#flog.appender(appender.file("logs/sammons_2d_info.log"), name="info_sam2D")
flog.threshold(ERROR, name="error_sam2D")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/sammons_2D_error.log",sep="")
), name="error_sam2D")
#flog.appender(appender.file("logs/sammons_2d_error.log"), name="error_sam2D")

flog.info("R libraries loaded succesfully for Sammons 2D agent.", name="info_sam2D")

#================================================
# Querying the database for symbols. 
# If connection fails, read it from a local file.
#================================================

drv <- dbDriver("PostgreSQL")

dbProperties2 <- suppressWarnings(as.vector(as.matrix(read.table(paste(mripPath,"/wd/db.properties",sep=""), header = F, sep = ",", skip = 1))))

tryCatch({con1 <- dbConnect(drv, host=dbProperties2[1], port = as.numeric(dbProperties2[2]), dbname = dbProperties2[3], user = dbProperties2[4], pass=dbProperties2[5])}, error=function(err){errorCode<-201
flog.warn("Connection to database on 162.192.100.157 failed. Reading the symbol vector from a local properties file.", name="info_sam2D")                                                                                                                                                                                              },finally={})
if (exists("con1")){
  flog.info("Connection to database 162.192.100.157 established.",  name="info_sam2D")
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
    flog.info("Symbol vector read from the local properties file succesfully",name="info_sam2D")
    flog.info("It contains %s tickers",length(symbolVec),name="info_sam2D")
    errorCode <<- 0

  } else {
    flog.fatal("Symbol vector not read in. Application fails without the Symbol Vector",name="error_sam2D")
  }
}

#======================
# initializing variables 
#======================

eJson <- paste("\"ErrorCode\":",500,sep="")
prevJson <- "{\"sammons\":\"NULL\""
fieldsJson <- NULL
samObj2D <- NULL
samMetaObj2D <- NULL
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
# Sammons projection - 2D
#========================

getSammonsXYZ2D <- function(corMatrix,dimension=2,nIterations=10000,tolerance=1e-6){
  #Check the matrix if it is sqaure and symmetric
  if (nrow(corMatrix)!=ncol(corMatrix)){
    flog.fatal("Matrix given to sammons 2d function is not a square matrix.", name = "error_sam2D")
    errorCode <<- -2
    return(-2)
  }
  if (!isSymmetric(corMatrix)){
    flog.fatal("Matrix given to sammons 2d function is not a symmetric matrix.", name = "error_sam2D")
    errorCode <<- -2
    return(-2) 
  }
  #Remove NA correlation values for sammons
  naRows = NA
  if (length(which(is.na(corMatrix))) != 0){
    naRows <- which(table(which(is.na(corMatrix),arr.ind=T)[,2])==(nrow(corMatrix)-1))
    #rownames(corMatrix)
    cnames<-rownames(corMatrix)[naRows]
    corMatrix <- corMatrix[-as.vector(naRows), -as.vector(naRows)]
  }
  #convert correlations to a distance metric
  corMatrix = 1 - abs(corMatrix)
  corNames <- rownames(corMatrix)
  
  if (length(which(is.nan(corMatrix)))!=0){
    errorCode<<-100
    return(100)
  }
  rownames(corMatrix) <- colnames(corMatrix) <- corNames
  
  if (!(dim(corMatrix)[1]<=2)){
    if (is.null(samObj2D)){
      tryCatch({
        samObj2D <<- sammon(corMatrix,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points}
               ,error=function(e){y<-cmdscale(corMatrix,dimension)
                                  if (anyDuplicated(y)){
                                    while(anyDuplicated(y))
                                    {
                                      p<-anyDuplicated(y)
                                      # offset the coordinates slightly
                                      y[p,]=y[p,]+runif(1)
                                    }
                                    samObj2D <<- sammon(corMatrix,y,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points
                                  }}
               
               ,finally={
                 samMetaObj2D <<- samObj2D
               })
    } else {
      samObj2D <<- try(sammon(corMatrix,y=samObj2D,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points, silent =T)
      if (class(samObj2D)=="try-error"){
        samObj2D <<- try(sammon(corMatrix,y=samMetaObj2D,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points, silent =T)
        if (class(samObj2D)=="try-error"){
          tryCatch({
            samObj2D <<- sammon(corMatrix,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points}
                   ,error=function(e){y<-cmdscale(corMatrix,dimension)
                                      if (anyDuplicated(y)){
                                        while(anyDuplicated(y))
                                        {
                                          p<-anyDuplicated(y)
                                          # offset the coordinates slightly
                                          y[p,]=y[p,]+runif(1)
                                        }
                                        samObj2D <<- sammon(corMatrix,y,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points
                                      }}
                   
                   ,finally={})        
        }
      }
      samMetaObj2D <<- samObj2D
    }
    # assign arbit vaues of coordinates which were NA
    if (!is.na(naRows)[1]){
      outliers = c()
      maxX <- max(samObj2D[,1])
      maxY <- max(samObj2D[,2])
      
      for(i in 1:length(naRows)){
        outliers <- c(outliers, c(maxX+runif(1,min=0.1,max=0.2), maxY+runif(1,min=0.1,max=0.2)))
      }
      
      outliers <- matrix(unlist(outliers), nrow = i,ncol=2, byrow = T)
      rownames(outliers) <- cnames
      samObj2D <<- rbind(samObj2D, outliers)
    }
    return(samObj2D)
  } else {
    #return("error:Insufficient data")
    if ((errorCode != 200) && (errorCode != 201)){
      errorCode<<-100
    }
    return(100)
  }
}

#==========================
# Creat Json for this agent
#==========================

createJson <- function(samMat){
    
    jsonSam ="\"sammons\":["
    
    jsonSam <- paste(jsonSam,"{\"id\":" ,paste( "\"", samMat[1,1],"\"",sep=""), ",", "\"x\":", samMat[1,2], ",", "\"y\":", samMat[1,3],"}")  
    jss<- lapply(2:nrow(samMat), function(i){
      jsonSam[i] <- paste("{\"id\":" ,paste( "\"", samMat[i,1],"\"",sep=""), ",", "\"x\":", samMat[i,2], ",", "\"y\":", samMat[i,3],"}")})    
  jssfinal<-paste(jss,collapse=",")
  jsonSam<-paste(jsonSam,",",jssfinal, "]")
    
  jsonSam <- paste("{",jsonSam,sep="")
  return(jsonSam)
}

#=========================
# sammons 2D main function
#=========================

sammons2D_main_fn <- function(inputJson = NA){
  
  if (is.na(inputJson)){
    flog.fatal("Data not received from the correlation agent. Sending the last valid json.", name = "error_sam2D")
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
    flog.error("Received an error from the correlation agent. Sending the last valid json.", name = "error_sam2D")
      
    retJson <- paste(prevJson,",",fieldsJson,"}",sep="")
    return(retJson)
  }
  
  tryCatch(convJsonToMat(inputJson), error = function(e){
                                                          flog.error("Json was not converted to matrix properly",name = "error_sam2D")
                                                          errorCode <<- -2
                                                        })
  samMat <- getSammonsXYZ2D(corMat)
  
  if (class(samMat) == "matrix" && errorCode == 0){
    if (any(rownames(samMat) != symbolVec)){
      samMat <- samMat[symbolVec,]
    }
    samMat <- cbind(rownames(samMat),samMat)
    retJson <- createJson(samMat)
    flog.info("2D Sammons Json sent succesfully.",name="info_sam2D")
    prevJson <<- retJson
    retJson <- paste(retJson,",",fieldsJson,"}",sep="")
    return(retJson)
  } else {
    errFieldsJson <- gsub("\"ErrorCode\":[0-9]{1,10}",paste("\"ErrorCode\":",errorCode),fieldsJson)
    retJson <- paste(prevJson,",",errFieldsJson,"}",sep="")
    return(retJson)
  }
}
