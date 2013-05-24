# ======================================================================================#
#-- Project Name: MINT - Spark lines Agent
#-- Task : Spark lines Json Creator and time series json creator Agent
#-- Version : 8.0
#-- Date : 26 Feb 2013
#-- Author : Karthik
#-- SVN Directory: \xxxx
#-- Dependent files : 1. db.properties
#--                   2. prediction.properties
#-- Dependent agents : 1. pollerRefreshAgent
#-- Comments : 1. Faster json computation compared to version 1
#--            2. Sends the aggregated price values
#--            3. Computes the time series as well
#=======================================================================================#
#-- Glossary of Error Codes : 1. 201 : Connection to database on 162.192.100.157 failed
#--                           2. 400 : Connection to activeMQ on 162.192.100.46 failed
#--                           3.  0  : No error
#--                           4. 10  : R version error
#--                           5. 11  : R package installation error
#--                           6. 502 : The ticker called for is undefined
#=======================================================================================#

#==============================
# Get the environment variables
#==============================
mripPath = Sys.getenv("MRIP_HOME")

a=0
errorCode<-0

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
      #    cat("package",pack_name,"could not be installed.. Please try after some time")
      return(return(11))
    }
    
  },finally={})
}
#____________________________________________________________________________________________________________________________________________________


errorCode<-checkRversion()

a[1]<-checkpackver("RPostgreSQL",'0.3-3')
a[2]<-checkpackver("stringr",'0.6.2')
a[3]<-checkpackver("zoo",'1.7-7')
a[4]<-checkpackver("Rjms",'0.0.4')
a[5]<-checkpackver("futile.logger",'1.3.0')

if(a[1]|a[2]|a[3]|a[4]|a[5]){
  errorCode<<-11
}

#________________________________________________________________________________________
#Loading the required libraries
#_________________________________________________________________________________________

library("RPostgreSQL")
library('Rjms')
library("stringr")
library("zoo")
library('futile.logger')

#=================================
# Setting futile logger thresholds
#=================================

flog.threshold(INFO, name="info_spl")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/sparklinesAgent_info.log",sep="")), name="info_spl")
flog.threshold(ERROR, name="error_spl")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/sparklinesAgent_error.log",sep="")), name="error_spl")

flog.info("R libraries loaded succesfully", name="info_spl")
#______________________________________________________________________________________________________________________________________________________________
# querying the database for the ticker list
#_______________________________________________________________________________________________________________________________________________________________

drv <- dbDriver("PostgreSQL")

tryCatch({dbProperties2 <- suppressWarnings(as.vector(as.matrix(read.table(paste(mripPath,"/wd/db.properties",sep=""), header = F, sep = ",", skip = 1))))}, error = function(err){traceback(); flog.fatal(" db.properties file not found in the path", name= "error_spl")},finally={})

tryCatch({con1 <- dbConnect(drv, host=dbProperties2[1], port = as.numeric(dbProperties2[2]), dbname = dbProperties2[3], user = dbProperties2[4], pass=dbProperties2[5])}, error=function(err){errorCode<-201},finally={})
#try(con1 <- dbConnect(drv, host="162.192.100.157", port = 5432, dbname = "postgres", user = "postgres", pass="postgres"), silent = T)
# check if the connection was established
if(exists("con1")){
  rs <- dbSendQuery(con1, statement = "select dep_tickers from jade_agents")
  dat <- fetch(rs, n=-1)
  symbolVec <- unlist(strsplit(dat[1,], ","))
  dbDisconnect(con1)
}else{
  errorCode <<- 201
}

# if the connection to the database containing the ticker symbols fails 
if(errorCode==201){
  tryCatch({symbolVec <- suppressWarnings(as.vector(as.matrix(read.table(paste(mripPath,"/wd/prediction.properties",sep=""), header = F, sep = ",", skip = 3))))}, error=function(err){flog.fatal("The connection to the data base on 162.192.100.157 failed. The backup properties file 'prediction.properties' is either modified or does not exist! The agency fails. Please check!", name = "error_spl")}, finally={})
  if(exists('symbolVec')){
     flog.warn("The connection to the database on 162.192.100.157 failed!!", name = "info_spl")
	 errorCode <<- 0
  } 
}


#__________Initialize global Variables__________
aggSpl <- as.list(symbolVec)
names(aggSpl) <- symbolVec
for(i in 1:length(aggSpl)){
  aggSpl[[i]]<-aggSpl[[i]][-1]
}
spl <- as.list(symbolVec)
names(spl) <- symbolVec
jsonRt <- "\"NULL\""
jsonRtMeta <- "\"NULL\""
jsonSp <- NULL
consumer<-initialize.consumer('tcp://162.192.100.156:61616','T','jadeStockStream')
lastTicked <- NULL
aggregateN <- 100
timeDiff <- 10
closedFlag <- TRUE
refreshFlag<-FALSE
marketIndicator<-""


#___________________________________________________________________________________________________________________
# Consumer function to consume from ActiveMQ
#___________________________________________________________________________________________________________________

consumeFn<-function(){
  z<-consume(consumer,asString=TRUE)  
  a1<-strsplit(strsplit(z,split="args\":")[[1]][2],split=",")[[1]]
  a3<-gsub('[^ a-zA-Z0-9:.-]', '',a1)
  return(paste(a3[-c(8,9)],collapse=","))
}



#____________________________________________________________________________________________________________________
# Stream data parser function
#____________________________________________________________________________________________________________________

parseStreamData = function(ticks){

   tickerId <- ticks[1]
   askPrice <- ticks[2]
   tradeTime <<- ticks[3]
   vol <- ticks[4]
   marketIndicator <<- ticks[5]
   if(marketIndicator == "refreshed"){
     refreshFlag <<- TRUE
   }
    # create a list of tickers and its values 
	if(tickerId %in% symbolVec){
	   lastTicked <<- tickerId
	   aggSpl[[which(names(aggSpl)==lastTicked)]][length(aggSpl[[which(names(aggSpl)==lastTicked)]])+1] <<- askPrice

       #populate the sparklines object
       spl[[which(names(spl)==tickerId)]][length(spl[[which(names(spl)==tickerId)]])+1] <<- askPrice
	   spl[[which(names(spl)==tickerId)]][length(spl[[which(names(spl)==tickerId)]])+1] <<- vol
	   spl[[which(names(spl)==tickerId)]][length(spl[[which(names(spl)==tickerId)]])+1] <<- tradeTime
		   
	}
}

#____________________________________________________________________________________________________________________
# Spark lines JSON creation function
#____________________________________________________________________________________________________________________

makeSparkLinesjsonFirstTime = function(){
   	
	
	 vals <- ""
	
	json <- paste("{\"id\":","\"", symbolVec[1],"\"", ",", "\"value\":[", vals, "]}", sep="")
    for( i in 2:length(symbolVec)){
	    json <- paste(json, paste("{\"id\":", "\"", symbolVec[i],"\"", ",", "\"value\":[", vals, "]}", sep=""), sep=",")
    }
	
	return(paste("{\"sparklines\":[", json, "]" ,sep=""))
}
#____________________________________________________________________________________________________________________
# Spark lines aggregation function
#____________________________________________________________________________________________________________________


aggregateSparkLines <- function(numerics, aggN){
   if(length(numerics)==aggN*2){
      nums <- rollapply(numerics, length(numerics)%/%aggN , mean, by = length(numerics)%/%aggN )
	  ind <- which(names(aggSpl)==lastTicked)
	  aggSpl[[ind]] <<- as.numeric(nums)
	  return(aggSpl[[ind]])
	}else {
	   nums <- numerics
	   return(nums)
	}	
}
#____________________________________________________________________________________________________________________
# Spark lines JSON updation function (this function just recomputes the json for the last ticked ticker 
# instead of creating the json from scratch 
#____________________________________________________________________________________________________________________

updateSparkLinesJson <- function(id, numerics, aggN=aggregateN){
    
		if(length(unlist(unname(numerics[1])))!=0){
			numerics <- aggregateSparkLines(numerics, aggN)
		    vals <- paste(unlist(numerics), collapse=",")
		}else{
		   vals <- ""
		}
		x1=strsplit(jsonSp, paste(id, "\",\"value\":",sep=""))[[1]][1]
		x2=strsplit(jsonSp, paste(id, "\",\"value\":",sep=""))[[1]][2]

		x3=str_sub(x2, str_locate(x2, "}"))[2]
        
		jsonSp <<- paste(x1, id, "\",\"value\":[",vals, "]", x3, sep="")
		return(jsonSp)
	
}

#____________________________________________________________________________________________________________________
# Spark lines JSON creation function
#____________________________________________________________________________________________________________________

makeSparkLinesJson <- function(sparklines){
  
      for(i in 1:length(sparklines)){
	     id <- names(sparklines[i])
		 vals <- sparklines[i]
		  if(is.null(jsonSp)){
            json <- makeSparkLinesjsonFirstTime()
			return(json)
		  }else{
		     json <- updateSparkLinesJson(id, vals)
		  }
      } 
     return(json)	  
}

#_____________________________________________________________________________________
# Function to create a data frame consisting of all the tickers
#______________________________________________________________________________________

parseChunk = function(streamData){
  
    ticks <- strsplit(streamData, ";")
	errorCode <<- strsplit(ticks[[1]][length(ticks[[1]])], ":")[[1]][2]
	ticks <- ticks[[1]][-length(ticks[[1]])]
	ticks <- strsplit(ticks, ",")
	
	for(i in 1:length(ticks)){
       if(errorCode != 400){
           parseStreamData(ticks[[i]])
	   } 
	}
}
#____________________________________________________________________________________________________________________
# Spark lines main function
#____________________________________________________________________________________________________________________

getSparkLines = function(streamData){
      
    # parse the ticker chunk        
    parseChunk(streamData) 	
	
	# refresh the spark lines when the poller is refreshed
    if(marketIndicator == "closed" || marketIndicator == "open"){
      closedFlag<<-TRUE
    }
    if((refreshFlag && closedFlag) ){
	   flog.info("Market opened/poller refreshed", name="info_spl")
	   aggSpl <<- as.list(symbolVec) # change#
       names(aggSpl) <<- symbolVec
	   for(i in 1:length(aggSpl)){
          aggSpl[[i]]<<-aggSpl[[i]][-1]
       }
	   spl <<- as.list(symbolVec)
       names(spl) <<- symbolVec
       jsonRt <<- "\"NULL\""
	   jsonRtMeta <<- "\"NULL\""
       jsonSp <<- NULL
       lastTicked <<- NULL
	   closedFlag <<- FALSE
	   refreshFlag <<- FALSE
    }
     
	 jsonSp <<- makeSparkLinesJson(aggSpl)
	 json <- paste(jsonSp, ",\"ErrorCode\":", errorCode, "}", sep="")
	 save.image("/home/musigma/MRIP/wd/sparklinesAgent.RData")
     return(paste(json,  sep=","))
}

#____________________________________________________________________________
# Wrapper fuctions to run it as a bidirectional feature
#____________________________________________________________________________
getJustSpl <- function(streamData){
    return(getSparkLines(streamData))
}

#____________________________________________________________________________________________________________________
# Time Series JSON creation function
#____________________________________________________________________________________________________________________

makeTsJson = function(timeseriesID){
   
   if(timeseriesID %in% symbolVec){
		timeSeries <- spl[[which(names(spl)==timeseriesID)]][-1]
		if(length(timeSeries)!=0){
			price <- paste("{\"price\":", timeSeries[1], sep="")
			vol <-  paste("\"volume\":", timeSeries[2], sep="")
			tickTime <- paste("\"date\":", "\"",timeSeries[3],"\"}",  sep="")
			vals <- paste( price, vol, tickTime, sep =",") 
			i <- 4
			while(i < length(timeSeries)){
				price <- paste("{\"price\":", timeSeries[i], sep="")
				vol <-  paste("\"volume\":", timeSeries[i+1], sep="")
				tickTime <- paste("\"date\":", "\"",timeSeries[i+2],"\"}",  sep="")
				vals <- paste(vals, price, vol, tickTime, sep =",") 
				i <- i+3
			}	
			return(paste("{\"timeSeries\":{\"id\":", "\"",timeseriesID,"\"", ",\"summaryTicks\":[", vals, "]}", sep=""))
		}
	}else{
	   return("")
	}
}

#____________________________________________________________________________
# This function is called in the event handler file 
#____________________________________________________________________________
getSplWithTS <- function(timeseriesID=NA){
    eC <- 0

     
    if(!is.na(timeseriesID)){
	    if(errorCode == 400){
		   eC <- 400
		}
        # run when there is a request for the time series
        jsonTs <- makeTsJson(timeseriesID)
		if(!is.null(jsonTs)){
		  if(jsonTs != ""){
		    json <- paste(jsonTs, ",\"ErrorCode\":", eC, "}", sep="")
		    eC <- 0
		    return(json)
		  }else{
		    return("{\"ErrorCode\":502}")
		 }
		}else{
		  if(errorCode != 400){
		    eC <- 100
		  }else{
		    eC <- 400
		  }
		   json <- paste("{\"timeSeries\":{\"id\":", "\"",timeseriesID,"\"", ",\"summaryTicks\":[]},\"ErrorCode\":", eC, "}", sep="")
		   eC <- 0
		   return(json)
		}
	}else{
	   #when the call was made to this function without specifying the timeseriesID
	   return("{\"ErrorCode\":502}")
	}
 
}
#____________________________________________________* END *__________________________________________________________

