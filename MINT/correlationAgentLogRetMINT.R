# ======================================================================================#
#-- Project Name: MINT : correlation agent on log return of prices
#-- Task : Charge data frame and compute correlations and return a json
#-- version : 1.0
#-- date : 19/MAR/13
#-- authors : Karthik
#-- SVN Directory: \xxxx
#-- Dependent files: 1. db.properties
#--                  2. prediction.properties
#-- Comments : 1. This file uses raw prices
#--            2. The data frame charging happens from the consumer agent
#--            3. The json format is not finalized
# ======================================================================================#
#_______________________________________________________________________________________#
#-- Glossary of Error Codes :
#--                        1. 100 : Insufficient Data/data frame charging
#--                        2. 200 : Connection to database on 162.192.100.48 failed
#--                        3. 201 : Connection to database on 162.192.100.157 failed
#--                        4. 10  : R version error
#--                        5. 11  : R package installation error
#--                        6. -1  : Unknown error
#--                        7.  0  : No error
#--                        8. 400 : Queue down
#_______________________________________________________________________________________#

#==============================
# Get the environment variables
#==============================
mripPath = Sys.getenv("MRIP_HOME")

# initilizing parameters 


errorCode<-0
a<-0
prevJson<-""
checPkgVec<-0

#___________________________________________________________________________________________________________________________
# Check package version
#____________________________________________________________________________________________________________________________
checkRversion<-function(){
  r<-getRversion()
  if(r<'2.15.1'){
    # print("Error. R needs to be upgraded to version 2.15.1 or later")
    return(10)
  }else{
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
#____________________________________________________________________________________________________________________________________________________


errorCode<-checkRversion()


checPkgVec[1]<-checkpackver("RPostgreSQL",'0.3-3')
checPkgVec[2]<-checkpackver("futile.logger",'1.3.0')
checPkgVec[3]<-checkpackver("Rjms",'0.0.4')

if(any(checPkgVec!=0)){
  errorCode<<-11
}

#______________________________
#Loading the required libraries
#______________________________

library("RPostgreSQL")
library('Rjms')
library('futile.logger')

#=================================
# Setting futile logger thresholds
#=================================

flog.threshold(INFO, name="info_CorLT")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/corrAgentLR_info.log",sep="")), name="info_CorLT")
flog.threshold(ERROR, name="error_CorLT")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/corrAgentLR_error.log",sep="")), name="error_CorLT")

flog.info("R libraries loaded succesfully", name="info_CorLT")


#================================================
# Querying the database for symbols. 
# If connection fails, read it from a local file.
#================================================

drv <- dbDriver("PostgreSQL")

dbProperties2 <- suppressWarnings(as.vector(as.matrix(read.table(paste(mripPath,"/wd/db.properties",sep=""), header = F, sep = ",", skip = 1))))

tryCatch({con1 <- dbConnect(drv, host=dbProperties2[1], port = as.numeric(dbProperties2[2]), dbname = dbProperties2[3], user = dbProperties2[4], pass=dbProperties2[5])}, error=function(err){errorCode<-201
                                                                                                                                                                                              flog.warn("Connection to database on 162.192.100.157 failed. Reading the symbol vector from a local properties file.", name="info_CorLT")                                                                                                                                                                                              },finally={})
if (exists("con1")){
  flog.info("Connection to database 162.192.100.157 established.",  name="info_CorLT")
  rs <- dbSendQuery(con1, statement = "select dep_tickers from jade_agents")
  dat <- fetch(rs, n=-1)
  symbolVec <- unlist(strsplit(dat[1,], ","))
  if (exists("symbolVec") && class(symbolVec) == "character"){
    flog.info("Symbol vector queried succesfully", name="info_CorLT")
  } 
  dbDisconnect(con1)
  dbClearResult(rs)
} else {
  errorCode <<- 201
}
if (errorCode==201){
  symbolVec <- suppressWarnings(as.vector(as.matrix(read.table(paste(mripPath,"/wd/prediction.properties",sep=""), header = F, sep = ",", skip = 3))))
  if (exists("symbolVec") && class(symbolVec) == "character"){
    flog.info("Symbol vector read from the local properties file succesfully",name="info_CorLT")
    flog.info("It contains %s tickers",length(symbolVec),name="info_CorLT")
    errorCode <<- 0

  } else {
    flog.fatal("Symbol vector not read in. Application fails without the Symbol Vector",name="error_CorLT")
  }
}

#_________________initilize global variables____________________________________


prevJson<-""
activeMQIp <- 'tcp://162.192.100.156:61616'
frameSize <- 2
nSymbols <- length(symbolVec)
#Added extra column for time
dfMatrix <- matrix(-1,nrow=frameSize,ncol=nSymbols+1)
fillCount <- 1
iterC <- 1
retDf <- as.data.frame(matrix(-1, nrow = 1, ncol=nSymbols))
colnames(retDf) <- symbolVec
tradeTime <- "\"NULL\""
marketIndicator <- "\"NULL\""
consumer<-initialize.consumer(activeMQIp,'T','jadeStockStream')
retFrameCount <- 0
updateCount <- 10
returnsDfFrameSize <- 2000
# set the period in which to compute the returns (in secs)
timeDiff <- 1
closedFlag<-TRUE
refreshFlag<-FALSE


#______________________________________________________________________________________________________________________________________________________________
# querying the database for the initial values for the first row of the data frame
#_______________________________________________________________________________________________________________________________________________________________

drv <- dbDriver("PostgreSQL")

dbProperties <- suppressWarnings(as.vector(as.matrix(read.table(paste(mripPath,"/wd/db.properties",sep=""), header = F, sep = ","))))

readFromDb = function(){
  tryCatch({con <- dbConnect(drv, host=dbProperties[1], port = as.numeric(dbProperties[3]), dbname = dbProperties[5], user = dbProperties[6], pass=dbProperties[5])}, error=function(err){errorCode<-200}, finally={})
  #try(con <- dbConnect(drv, host="162.192.100.48", port = 5432, dbname = "postgres", user = "postgres", pass="postgres"), silent = T)
  
  # check if the connection was established
  if(exists("con")){
    rs <- dbSendQuery(con, statement = "select lasttimestamp from mstream_lasttime")
    dateTime <- fetch(rs, n = -1)
    dbNameS <- paste("stockdto_objects_", dateTime, sep="")
    dbNameI <- paste("indexdata_", dateTime, sep="")
    syms <- paste("'",symbolVec, "'", sep="")
    lastPriceVecS = lapply(1:length(syms), function(i){
      rs <- dbSendQuery(con, statement = paste("select buy_price from", dbNameS, "where symbol =", syms[i] ,"order by stockcreate_datetime DESC LIMIT 1"))
      lastP <- fetch(rs, n = -1)
      
      if(nrow(lastP)==0){
        lastP=-1
      }
      return(lastP)
    })
    ii = which(lastPriceVecS==-1)
    lastPriceVecI = lapply(1:length(ii), function(i){
      rs <- dbSendQuery(con, statement = paste("select last_price from", dbNameI, "where symbol =", syms[ii[i]] ,"order by indexcreate_datetime DESC LIMIT 1"))
      lastP <- fetch(rs, n = -1)
      if(nrow(lastP)==0){
        lastP=-1
      }
      return(lastP)
    })
    lastPriceVecS[ii] <- lastPriceVecI
    rs <- dbSendQuery(con, statement = paste("select stockcreate_datetime from", dbNameS, "order by stockcreate_datetime DESC LIMIT 1"))
    lastTime <- as.POSIXct(strptime(fetch(rs, n = -1), "%Y-%m-%d %H:%M:%OS"))
    dfMatrix[1,1:length(symbolVec)] <<- unname(unlist(lastPriceVecS))
    dfMatrix[1,length(symbolVec)+1] <<- unname(lastTime)
    dbClearResult(rs)
	flog.info("Successfully read the last updated prices from the data base", name="info_CorLT")
  }else{
    errorCode <<- 200
	flog.fatal("Failed to read the last updated prices from the data base!", name="error_CorLT")
  }
}

# call the above function
readFromDb()

#___________________________________________________________________________________________________________________
# Consumer function to consume from ActiveMQ
#___________________________________________________________________________________________________________________

consumeFn<-function(){
  z<-consume(consumer,asString=TRUE)  
  a1<-strsplit(strsplit(z,split="args\":")[[1]][2],split=",")[[1]]
  a3<-gsub('[^ a-zA-Z0-9:.-]', '',a1)
  return(paste(a3[-c(8,9)],collapse=","))
}

#______________________________________________________________________________________
# data frame charging function
#______________________________________________________________________________________

dfChargeUsePrev <- function(tickerId, askPrice, tradeTime){
  temp <- gsub('.*:','', tradeTime)
  tradeTime <- sub(paste(':',temp,sep=""),paste(':',temp,sep=""), tradeTime)
  if(!is.na(askPrice)){
    symIndex <- which(symbolVec == tickerId)
    #First fill row no 1 in the matrix
    if(is.element(-1,as.vector(dfMatrix[1,]))){
      dfMatrix[1,symIndex] <<- askPrice
      dfMatrix[1,(nSymbols+1)] <<- as.POSIXct(strptime(tradeTime,"%Y-%m-%d %H:%M:%OS"))
      dFrame = data.frame(dfMatrix)
      colnames(dFrame) <- c(symbolVec,'Time')
      return(list("dFrame"=dFrame,"full"=FALSE))
    }
    #We have a complete row now
    else{
      if(fillCount == frameSize){
        #Pop the first element and insert at bottom
        tVec <- dfMatrix[fillCount[1],]
        dfMatrix[1:frameSize-1,] <<- dfMatrix[2:frameSize,]
        dfMatrix[frameSize,] <<- tVec
        dfMatrix[frameSize,symIndex] <<- askPrice
        dfMatrix[frameSize,(nSymbols+1)] <<- as.POSIXct(strptime(tradeTime,"%Y-%m-%d
                                                                 %H:%M:%OS"))
        dFrame <- data.frame(dfMatrix)
        colnames(dFrame) <- c(symbolVec,'Time')
        return(list("dFrame"=dFrame,"full"=TRUE))
      }
      else{
        #Add the element
        fillCount <<- fillCount + 1
        #Replicate previous value
        dfMatrix[fillCount,] <<- dfMatrix[fillCount-1,]
        dfMatrix[fillCount,symIndex] <<- askPrice
        dfMatrix[fillCount,(nSymbols+1)] <<- as.POSIXct(strptime(tradeTime,"%Y-%m-%d
                                                                 %H:%M:%OS"))
        if(fillCount == frameSize){
          dFrame <- data.frame(dfMatrix)
          colnames(dFrame) <- c(symbolVec,'Time')
          return(list("dFrame"=dFrame,"full"=TRUE))
        }
        else{
          dFrame = data.frame(dfMatrix)
          colnames(dFrame) <- c(symbolVec,'Time')
          return(list("dFrame"=dFrame,"full"=FALSE))
        }
      }
  }
  }
}

#_____________________________________________________________________________________
# Function to create a data frame consisting of all the tickers
#______________________________________________________________________________________

dfCharge_for_multi_ticks = function(streamData){
  
  if(length(streamData)!=0){
    d1<-unlist(strsplit(strsplit(streamData,split=",")[[1]],split=":"))
    if(!is.na(d1[20])){
      askPrice<-as.numeric(d1[7])
      tickerId<-d1[20]
      tradeTime<<-paste(d1[15:18],collapse=":")
      marketIndicator <<- d1[22]
    }
    else{
      askPrice<-as.numeric(d1[2])
      tickerId<-d1[9]
      tradeTime<<-paste(d1[4:7],collapse=":")
      marketIndicator <<- d1[15]
    }
    if(marketIndicator == "refreshed"){
      refreshFlag <<- TRUE
    }
    res=dfChargeUsePrev(tickerId,askPrice,tradeTime)
    res = res$dFrame
    # If the connection to the database was failed
    if(errorCode != 200){
      res <- res[-1,]
      # Now we have a data frame of row 1 which is called after a certain period
      return(res)
    }else{
      return(res[1,])
    }
    
  }
  
}

#_____________________________________________________________________________________
# Function to create a data frame consisting of all the tickers for a specified period
#______________________________________________________________________________________

chargeDfForReturns = function(res){
  
  if(retFrameCount == 0){
    retDf <<- res
    retFrameCount <<- retFrameCount + 1
  }else{
    retDf[nrow(retDf)+1,] <<- res
    retFrameCount <<- retFrameCount + 1
  }
  # if the frame size limit is reached, pop the first row of the DF
  if(returnsDfFrameSize < retFrameCount){
    retDf <<- retDf[-1,]
    retFrameCount <<- retFrameCount - 1
  }
  
  return(retDf)
}

#_______________________________________
# Function create the correlation matrix
#_______________________________________
getCor <- function(mat){
  return(cor(mat))
}

#____________________________________________________________________________________
# Function to create the JSON for UI
#____________________________________________________________________________________
jsonCreator <- function(corMat){
  
  #Calling community and centrality function
  
  
  values<- corMat[lower.tri(corMat, diag = F)]
  dims <- which(lower.tri(corMat, diag = F), arr.ind =T)-1
  
  #-------------------------------------------------------------------------------------------------------
  # Correlation Json (jsonCor)
  #-------------------------------------------------------------------------------------------------------
  jsonCor = "\"relations\":"
  jsonCor[1]<-paste(jsonCor, "[{\"source\":", dims[1,2],",", "\"target\":", dims[1,1],",", "\"value\":", values[1],"}")
  jsc<-lapply(2:length(values), function(i)
    jsonCor[i]<- paste("{\"source\":", dims[i,2],",", "\"target\":", dims[i,1],",", "\"value\":", values[i],"}")) 
  jscfinal<-paste(jsc,sep=",",collapse=",")
  jsonCor<-paste(jsonCor,",",jscfinal, "]")
  #_______________________________________________________________________________________________________
  
  
  #-------------------------------------------------------------------------------------------------------
  # Nodes Json
  #-------------------------------------------------------------------------------------------------------
  nodes = paste("\"nodes\":[{\"id\":","\"", symbolVec[1],"\"}", sep ="")
  for(i in 2:length(symbolVec)){
    nodes <- paste(nodes, paste("{\"id\":","\"", symbolVec[i],"\"}", sep =""), sep=",")
  }
  nodes<- paste(nodes, "]")
  #-------------------------------------------------------------------------------------------------------
  
  return (paste( "{", paste(jsonCor, nodes,sep=",")))
}
#_________________________________________________________________________________________________________


#____________________________________________________________________________________
# Function to call it all (get the corr graph)
#____________________________________________________________________________________
cor_main_fn_LR = function(){
  
  #tryCatch({
  # to refresh the data frame when the market is open
  # closedFlag ensures that that the dataFrame is refreshed only once
  if(!is.na(marketIndicator)){
    if(marketIndicator == "closed" || marketIndicator == "open"){
      closedFlag<<-TRUE
    }
    if(refreshFlag && closedFlag){
      retDf <<- as.data.frame(matrix(-1, nrow = 1, ncol=nSymbols))
      colnames(retDf) <<- symbolVec
      retFrameCount <<- 0
      dfMatrix <<- matrix(-1,nrow=frameSize,ncol=nSymbols+1)
      fillCount <<-1
	  flog.info("Market opened/poller refreshed", name="info_CorLT")
      readFromDb()
      closedFlag <<- FALSE
      refreshFlag <<- FALSE
    }
  }
  # updateCount is the number of rows of the data drame to be updated after every iteration
  for(i in 1:updateCount){
    
	# charge the data frame for exactly timeDiff secs
    timeLim = Sys.time() + timeDiff
    while(Sys.time() < timeLim){
      # check for ActiveMQ connection
      tryCatch({streamData <- consumeFn()}, error=function(err){errorCode<<-400}, finally={})
      # res1 is the data frame that contains 1 latest row of data
      if((errorCode != 400) && exists("streamData")){
        res1 = dfCharge_for_multi_ticks(streamData)
      }  
      else{
        errorCode <<- 0
		flog.fatal("The queue on %s is disconnected!",activeMQIp, name="error_CorLT")
		flog.info("Error:The consumer queue is down!", name="info_CorLT")
        Sys.sleep(timeDiff * updateCount)
        # send the last updated json with the error code of 400
        return(paste(prevJson,"\"ErrorCode\":400}", sep=","))
      }
    }  
    
    #remove the time column
    res1 <- res1[,-ncol(res1)]
    if(!(-1 %in% res1)){
      # Charge the dataframe where each row is a timeLim period of data
      res2 <- chargeDfForReturns(res1)
    }
  }
  
  if(retFrameCount  >= returnsDfFrameSize){
    startTime <- Sys.time()
    flog.info("computation began at %s",startTime, name="info_CorLT")
    # compute the log returns
    res2 = diff(log(as.matrix(res2)))
    colnames(res2) = symbolVec 
    
    
    #write.csv(res2, paste("df", as.character(iterC), ".csv", sep=""))
    pearson_mat = getCor(res2)
    # sometimes, when the frame size is too low, we may get cor values to be 1/-1. To circumvent this, we make the cor values 0.99
    correlationPOnes <- which(pearson_mat==1)
	correlationMOnes <- which(pearson_mat==-1)
	pearson_mat[correlationPOnes] <- pearson_mat[correlationPOnes] - 0.01
	pearson_mat[correlationMOnes] <- pearson_mat[correlationMOnes] + 0.01
	diag(pearson_mat) <- 1
    #________________________________________________________________________________
    # 1. if the column remains constant, the cor value returned is NA. for this,
    # we replace that value with 0
    #__________________________________________________________________________________
    
    
    
    if(length(which(is.na(pearson_mat))) != 0){
      pearson_mat[which(is.na(pearson_mat))] = 0
      
    }
    #write.csv(pearson_mat, paste("mat", as.character(iterC), ".csv", sep=""))
    
    # Normalizing corr values to a distance matrix
    pearson_mat <- round(pearson_mat, digits=3)
    
    json <- jsonCreator(pearson_mat)
    eJson <- paste("\"ErrorCode\":",errorCode,sep="")
    marketFlag <- paste("\"marketStatus\":","\"",marketIndicator,"\"",sep="")
    dfSize <- paste("\"DataFrameSize\":","\"",retFrameCount, "/", returnsDfFrameSize,"\"",sep="")
    json <- paste(json, paste("\"TimeStamp\":", "\"",tradeTime,"\"", sep=""), marketFlag, dfSize, sep=",")
    # the previous json w/o the error code
    prevJson <<- json
    json <- paste(json,eJson,sep = ",")
    json <- paste(json, "}", sep="")
    errorCode<<-0
	procTime <- difftime(Sys.time(),startTime)
    flog.info("computation ended at %s, processing time: %s secs",Sys.time(),procTime, name="info_CorLT")
     return(json)
  }else{
    if((errorCode != 200) && (errorCode != 201)){
	  errorCode <<- 100
      prevJson <<- paste("{\"graph\":\"NULL\", \"TimeStamp\":", "\"",tradeTime,"\"", ",\"marketStatus\":","\"",marketIndicator,"\"",",\"DataFrameSize\":","\"", retFrameCount, "/", returnsDfFrameSize,"\"", sep = "")
      return(paste(prevJson, ",\"ErrorCode\":", errorCode,"}", sep=""))
    }else{
	  flog.info("Check the error log file, an error has occoured!", name="info_CorLT")
	  flog.info("returning the same json again..", name="info_CorLT")
      prevJson <<- paste("{\"graph\":\"NULL\",\"TimeStamp\":","\"", tradeTime,"\"", ",\"marketStatus\":","\"",marketIndicator,"\"",",\"DataFrameSize\":","\"",retFrameCount, "/", returnsDfFrameSize,"\"", "," , "\"ErrorCode\":", errorCode,"}", sep="")
      return(prevJson)
    }
  }
  #}, error=function(err){
  # write.csv("'ErrorCode':-1", "j.csv")
  # return("'ErrorCode':-1, 'Correlation': 'NULL', 'Sammons': 'NULL'")
  #}, finally={})
  
  
}


