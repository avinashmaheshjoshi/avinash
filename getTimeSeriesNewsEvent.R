# ======================================================================================#
#-- Project Name: MINT
#-- Task : Handle time series request UI event 
#-- Version : 1.0
#-- Date : 05 Apr 2013
#-- Author : Karthik
#-- SVN Directory: \xxxx
#-- Dependent files : 
#-- Dependent agents : 
#-- Comments : 1. 
#=======================================================================================#
#-- Glossary of Error Codes : 1. 400 : Connection to activeMQ on 162.192.100.46 failed
#--                           2.  0  : No error
#--                           3. 600 : The RData file not found
#--                           4. 502 : No external data recieved
#--                           5. 11  : R package installation error
#=======================================================================================#


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


a<-0
a[1]<-checkpackver("stringr",'0.6.2')
a[2]<-checkpackver("RPostgreSQL",'0.3-3')
a[3]<-checkpackver("futile.logger",'1.3.0')
if(a[1]|a[2]|a[3]){
  errorCode<<-11
}

# ___________________________________________________________________________________________________________________________________________________
# Load the Libraries and the dependent .RData file
#_____________________________________________________________________________________________________________________________________________________

tryCatch({load("/home/musigma/MRIP/wd/sparklinesAgent.RData")}, error=function(err){errorCode<<- 600}, finally={})
library("stringr")
library("RPostgreSQL"); 
library("futile.logger")

#=================================
# Setting futile logger thresholds
#=================================

flog.threshold(INFO, name="info_rss")
flog.appender(appender.file("/home/musigma/MRIP/MINTlogs/NewsEvent_info.log"), name="info_rss")
flog.threshold(ERROR, name="error_rss")
flog.appender(appender.file("/home/musigma/MRIP/MINTlogs/NewsEvent_error.log"), name="error_rss")

flog.info("R libraries loaded succesfully", name="info_rss")


#______________________________________________________________________________________________________________________________________________________________
# querying the database for the ticker list
#_______________________________________________________________________________________________________________________________________________________________

drv <- dbDriver("PostgreSQL")
#symbolVec <- suppressWarnings(as.vector(as.matrix(read.table("/home/musigma/jade/agents/prediction.properties", header = F, sep = ",", skip = 3))))

dbProperties2 <- suppressWarnings(as.vector(as.matrix(read.table("/home/musigma/jade/agents/db.properties", header = F, sep = ",", skip = 1))))

tryCatch({con1 <- dbConnect(drv, host=dbProperties2[1], port = as.numeric(dbProperties2[2]), dbname = dbProperties2[3], user = dbProperties2[4], pass=dbProperties2[5])}, error=function(err){errorCode<-201},finally={})
#try(con1 <- dbConnect(drv, host="162.192.100.157", port = 5432, dbname = "postgres", user = "postgres", pass="postgres"), silent = T)
# check if the connection was established
if(exists("con1")){
  rs <- dbSendQuery(con1, statement = "select dep_tickers from jade_agents")
  dat <- fetch(rs, n=-1)
  symbolVec <- unlist(strsplit(dat[1,], ","))
  if(exists("symbolVec") && (class(symbolVec) == "character")){
    flog.info("Symbol vector queried succesfully",  name="info_rss")
  } 
  dbDisconnect(con1)
  dbClearResult(rs)
}else{
  errorCode <<- 201
}

# if the connection to the database containing the ticker symbols fails 
if(errorCode==201){
  tryCatch({symbolVec <- suppressWarnings(as.vector(as.matrix(read.table("/home/musigma/jade/agents/prediction.properties", header = F, sep = ",", skip = 3))))}, error=function(err){flog.fatal("The connection to the data base on 162.192.100.157 failed. The backup properties file 'prediction.properties' is either modified or does not exist! The agency fails. Please check!", name = "error_rss")}, finally={})
  if(exists('symbolVec')){
     flog.warn("The connection to the database on 162.192.100.157 failed!!", name="info_rss")
	 errorCode <<- 0
  } 
}

#___________________________________________________________________________________________________________________________________________________
# Event handler function
#___________________________________________________________________________________________________________________________________________________

getTimeSeries <- function(TID){
   if(errorCode!=600){
     return(getSplWithTS(TID))
   }else{
     return("{\"ErrorCode\":600}")
   }
}
#________________________________________________________________________________________
# Function to extract news from Google finance
#_________________________________________________________________________________________

getGoogleURLs <- function(symbol){
  
  # construct url to news feed rss and encode it correctly
  url.g <- paste('\"http://www.google.com/finance/company_news?q=', symbol, '&output=rss',"\"", sep = '')
  return(url.g)  
}

#________________________________________________________________________________________
# Function to extract news from Yahoo finance
#_________________________________________________________________________________________

getYahooURLs <- function(symbol){
  
  # construct url to news feed rss and encode it correctly
  url.y = paste('\"http://feeds.finance.yahoo.com/rss/2.0/headline?s=', symbol, '&region=US&lang=en-US',"\"", sep = '')
  return(url.y)    
}
#________________________________________________________________________________________
# Function to extract news from NASDAQ
#_________________________________________________________________________________________

getNASDAQURLs <- function(symbol){
  
  ## construct url to news feed rss and encode it correctly
  url.n = paste('\"http://articlefeeds.nasdaq.com/nasdaq/symbols?symbol=', symbol,"\"", sep = '')
  return(url.n)   
}

#________________________________________________________________________________________
# Function to extract news from Financial Content
#_________________________________________________________________________________________

getFCURLs <- function(symbol){
    ## construct url to news feed rss and encode it correctly
    url.f = paste('\"http://markets.financialcontent.com/stocks/action/rssfeed?Symbol=', symbol,"\"", sep = '')
	return(url.f)
}
getFCURLfs <- function(){
    url.ff = paste(url.f, paste('\"http://markets.financialcontent.com/stocks/action/rssfeed?Category=financial\"'), sep=",")
	return(url.ff)
}

#________________________________________________________________________________________
# Function to make the news JSON
#________________________________________________________________________________________

makeNewsJson <- function(newsURLSs){
   json <- "{\"Feeds\":["
   json <- paste(json, "{\"id\":", "\"",symbolVec[1],"\"", ",\"URLs\":[{\"url\":", newsURLSs[[1]][1], "},{\"url\":", newsURLSs[[1]][2], "},{\"url\":", newsURLSs[[1]][3], "},{\"url\":", newsURLSs[[1]][4], "}]}", sep="")
   for(i in 2:length(symbolVec)){
        json <- paste(json, paste("{\"id\":","\"", symbolVec[i], "\"",",\"URLs\":[{\"url\":", newsURLSs[[1]][1], "},{\"url\":", newsURLSs[[1]][2], "},{\"url\":", newsURLSs[[1]][3], "},{\"url\":", newsURLSs[[1]][4], "}]}", sep=""), sep = ",")
    }
	return(paste(json, "],\"ErrorCode\":", errorCode , "}",sep=""))
}

#________________________________________________________________________________________
# Main function
#________________________________________________________________________________________

getNewsURLs <- function(newsClick=NA){
  newsURLSs <- list()  
  for(i in 1:length(symbolVec)){
    newsURLSs[i][[1]] <- getGoogleURLs(symbolVec[i])
    newsURLSs[[i]][[2]] <- getYahooURLs(symbolVec[i])
    newsURLSs[[i]][[3]] <- getNASDAQURLs(symbolVec[i])
    newsURLSs[[i]][[4]] <- getFCURLs(symbolVec[i])
  }
  json <- makeNewsJson(newsURLSs)
  return(json)
}

#______________________________________________________________*END*________________________________________________________________________________