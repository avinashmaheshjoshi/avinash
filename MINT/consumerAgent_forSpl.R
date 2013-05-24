# ======================================================================================#
#-- Project Name: MINT : consumer agent
#-- Task : To consume messages from activeMQ and send it to the other agents
#-- version : 1.0
#-- date : 9/JAN/13
#-- authors : Karthik
#-- SVN Directory: \xxxx
#-- Dependent files: 
# ======================================================================================#
#_______________________________________________________________________________________#
#-- Glossary of Error Codes : 1. 400 : Connection to activeMQ on 162.192.100.48 failed
#--                           2.  0  : No error
#--                           3. 201 : Connection to database on 162.192.100.157 failed
#_______________________________________________________________________________________#

#==============================
# Get the environment variables
#==============================
mripPath = Sys.getenv("MRIP_HOME")

#__________initilizing parameters_____________

errorCode <- 0
chunkSize <- 20
a <- 0
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
    for(i in 1:length(p))
    {
      if(p[i]<req_ver[i])
        stop()
    }
    return(0)
  },error=function(err)
  {
    while(!(pack_name %in% packinfo[,1]) || count<3){
      install.packages(pack_name,dependencies=TRUE)
      packinfo <- installed.packages (fields = c ("Package", "Depends", "Version"))
      count<<-count+1
    }
    if(count==3){
      #flog.fatal("package %s could not be installed.. Trying again..", pack_name)
      return(return(11))
    }
    
  },finally={})
}
#____________________________________________________________________________________________________________________________________________________


errorCode<-checkRversion()


a[1]<-checkpackver("RPostgreSQL",'0.3-3')
a[2]<-checkpackver("Rjms",'0.0.4')
a[3]<-checkpackver("futile.logger",'1.3.0')


if(a[1]|a[2]|a[3]){
  errorCode<<-11
}
#______________________________
#Loading the required libraries
#______________________________

library('Rjms')
library("RPostgreSQL")
library('futile.logger')
consumer<-initialize.consumer('tcp://162.192.100.156:61616','T','jadeStockStream')

#=================================
# Setting futile logger thresholds
#=================================

flog.threshold(INFO, name="info_consume")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/consumerAgent_info.log",sep="")), name="info_consume")
flog.threshold(ERROR, name="error_consume")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/consumerAgent_error.log",sep="")), name="error_consume")

flog.info("R libraries loaded succesfully", name="info_consume")

#================================================
# Querying the database for symbols. 
# If connection fails, read it from a local file.
#================================================

drv <- dbDriver("PostgreSQL")

dbProperties2 <- suppressWarnings(as.vector(as.matrix(read.table(paste(mripPath,"/wd/db.properties",sep=""), header = F, sep = ",", skip = 1))))

tryCatch({con1 <- dbConnect(drv, host=dbProperties2[1], port = as.numeric(dbProperties2[2]), dbname = dbProperties2[3], user = dbProperties2[4], pass=dbProperties2[5])}, error=function(err){errorCode<-201
                                                                                                                                                                                              flog.warn("Connection to database on 162.192.100.157 failed. Reading the symbol vector from a local properties file.", name="info_consume")                                                                                                                                                                                              },finally={})
if (exists("con1")){
  flog.info("Connection to database 162.192.100.157 established.",  name="info_consume")
  rs <- dbSendQuery(con1, statement = "select dep_tickers from jade_agents")
  dat <- fetch(rs, n=-1)
  symbolVec <- unlist(strsplit(dat[1,], ","))
  if (exists("symbolVec") && class(symbolVec) == "character"){
    flog.info("Symbol vector queried succesfully", name="info_consume")
  } 
  dbDisconnect(con1)
  dbClearResult(rs)
} else {
  errorCode <<- 201
}
if (errorCode==201){
  symbolVec <- suppressWarnings(as.vector(as.matrix(read.table(paste(mripPath,"/wd/prediction.properties",sep=""), header = F, sep = ",", skip = 3))))
  if (exists("symbolVec") && class(symbolVec) == "character"){
    flog.info("Symbol vector read from the local properties file succesfully",name="info_consume")
    flog.info("It contains %s tickers",length(symbolVec),name="info_consume")
    errorCode <<- 0

  } else {
    flog.fatal("Symbol vector not read in. Application fails without the Symbol Vector",name="error_consume")
  }
}



#___________________________________________________________________________________________________________________
# Consumer function to consume from ActiveMQ
#___________________________________________________________________________________________________________________

consumeFn<-function(){
  z<-consume(consumer,asString=TRUE)  
  a1<-strsplit(strsplit(z,split="args\":")[[1]][2],split=",")[[1]]
  a3<-gsub('[^ a-zA-Z0-9:.-]', '',a1)
  return(paste(a3[-c(8,9)],collapse=","))
}

#___________________________________________________________________________________________________________________
# Function to parse the message
#___________________________________________________________________________________________________________________

parseMessage = function(streamData){
  
  if(exists('streamData') && (length(streamData)!=0)){
    d1<-unlist(strsplit(strsplit(streamData,split=",")[[1]],split=":"))
    if(!is.na(d1[20])){
      askPrice<-as.numeric(d1[7])
      tickerId<-d1[20]
      tradeTime<<-paste(d1[15:18],collapse=":")
      marketIndicator <<- d1[22]
	  vol <- d1[11]
    }
    else{
      askPrice<-as.numeric(d1[2])
      tickerId<-d1[9]
      tradeTime<<-paste(d1[4:7],collapse=":")
      marketIndicator <<- d1[15]
	  vol <-0
    }       
    return(list("messag"= paste(tickerId,askPrice,tradeTime,vol,marketIndicator, sep=","), "ticker"= tickerId))
  }
  
}

#__________________________________________________________________________________________________________________________________
# Consumer main fn to send a chunk of a specified size only of symbols specified in the 'symbolVec' (called for the raw prices algo)
#__________________________________________________________________________________________________________________________________

consumerParseForRawPrices <- function(){
    startTime <- Sys.time()
	flog.info("computation began at %s",startTime, name="info_consume")
    tickFrame <- data.frame()
	 i <- 1
     while(nrow(tickFrame)<chunkSize){
       tryCatch({streamData <- consumeFn()}, error=function(err){errorCode<<-400; flog.fatal("The Queue on 100.46 is down!", name="error_consume")}, finally={})
        if(exists('streamData')){	     
		  mes <- parseMessage(streamData)
	      if(mes$ticker %in% symbolVec){
             tickFrame[i,1] <- mes$messag
		     i <- i+1
	      }
        }else{
		   flog.info("could not finish computation.. queue down", name="info_consume")
           return("ErrorCode:400")
        }		   
     }
	 procTime <- difftime(Sys.time(), startTime)
	 flog.info("computation ended at %s, processing time: %s secs",Sys.time(),procTime, name="info_consume")

     return(paste(paste(tickFrame[,1], collapse =";"), ";ErrorCode:", errorCode,sep=""))
}

#___________________________________________________________________________________________________________________
# Consumer main fn to send each tick
#___________________________________________________________________________________________________________________

consumerParseGeneric <- function(){
    tryCatch({streamData <- consumeFn()}, error=function(err){errorCode<<-400; flog.fatal("The Queue on 100.46 is down!", name="error_consume")}, finally={})
	if(exists('streamData')){
      return(paste(parseMessage(streamData)$messag, ",\"ErrorCode:\"", errorCode, sep=""))
    }else{
	  return("ErrorCode:400")
	}
}

#___________________________________________________________________________________________________________________
# Consumer main fn to send each tick in the symbolVec
#___________________________________________________________________________________________________________________

consumeRelTick <- function(){
   tryCatch({streamData <- consumeFn()}, error=function(err){errorCode<<-400; flog.fatal("The Queue on 100.46 is down!", name="error_consume")}, finally={})
   if(exists('streamData')){
     mes <- parseMessage(streamData)
     if(mes$ticker %in% symbolVec){
        return(paste(mes$messag, ",ErrorCode:", errorCode, sep=""))
      }else{
	    consumeRelTick()
	  }
	}else{
	  return("ErrorCode:400")
	}
}

#__________________________________________________________*END*_____________________________________________________
