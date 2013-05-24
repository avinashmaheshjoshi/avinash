# ======================================================================================#
#-- Project Name: MINT News Event
#-- Task : Add/Remove news URLs
#-- Version : 1.0
#-- Date : 25 Apr 2013
#-- Author : Karthik
#-- SVN Directory: \xxxx
#-- Dependent files : 
#-- Dependent agents : 
#-- Comments : 1. 
#=======================================================================================#
#-- Glossary of Error Codes : 1. 0  : No error
#--                           2. 11  : R package installation error
#=======================================================================================#

#==============================
# Get the environment variables
#==============================
mripPath = Sys.getenv("MRIP_HOME")

errorCode<-0
a<-0
number <- 2
urlList <- c()


#___________________________________________________________________________________________________________________________
# Check package version
#____________________________________________________________________________________________________________________________
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

#____________________________________________________________________________________________________________________________________________________


a[1]<-checkpackver("RPostgreSQL",'0.3-3')
a[2]<-checkpackver("futile.logger",'1.3.0')

if(a[1]|a[2]){
  errorCode<<-11
}



library("RPostgreSQL")
library("futile.logger")

#=================================
# Setting futile logger thresholds
#=================================

flog.threshold(INFO, name="info_news")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/newsEvent_info.log",sep="")), name="info_news")
flog.threshold(ERROR, name="error_news")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/newsEvent_error.log",sep="")), name="error_news")

flog.info("R libraries loaded succesfully", name="info_news")


#____________________________________________________________ 
#______________________________________________________________________________________________________________________________________________________________
# querying the database for the ticker list
#_______________________________________________________________________________________________________________________________________________________________

drv <- dbDriver("PostgreSQL")

tryCatch({dbProperties2 <- suppressWarnings(as.vector(as.matrix(read.table("/home/musigma/jade/agents/db.properties", header = F, sep = ",", skip = 1))))}, error = function(err){traceback(); flog.fatal(" db.properties file not found in the path", name= "error_news")},finally={})

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
  tryCatch({symbolVec <- suppressWarnings(as.vector(as.matrix(read.table("/home/musigma/jade/agents/prediction.properties", header = F, sep = ",", skip = 3))))}, error=function(err){flog.fatal("The connection to the data base on %s failed. The backup properties file 'prediction.properties' is either modified or does not exist! The agency fails. Please check!",dbProperties2[1], name = "error_news")}, finally={})
  if(exists('symbolVec')){
    flog.warn("The connection to the database on %s failed!!",dbProperties2[1], name = "info_news")
    errorCode <<- 0
  } 
}

#_______________________________________________________________________________________________________________
# Function To make Url Json
#_______________________________________________________________________________________________________________

makeUrlJson <- function(urls){
  urls <- paste(paste("\"",urls,"\"",sep=""), collapse=",")
  json <- paste("\"urls\":[", urls, "]", sep = "")
}
#________________________________________________________________________________________
# Function to make the URLs
#________________________________________________________________________________________

makeUrls <- function(){
  for(i in 1:length(symbolVec)){
    #_________________google_______________________________
    url.b1 = 'http://www.google.com/finance/company_news?q='
    url    = paste(url.b1, symbolVec[i], '&output=rss', "&start=", 1,
                   "&num=", number, sep = '')
    url    = URLencode(url)
    urlList[length(urlList)+1] <<- url
    #_________________yahoo_______________________________
    url.b1 = 'http://feeds.finance.yahoo.com/rss/2.0/headline?s='
    url    = paste(url.b1, symbolVec[i], '&region=US&lang=en-US', sep = '')
    url    = URLencode(url)
    urlList[length(urlList)+1] <<- url
    #_________________nasdaq_______________________________
    url.b1 = 'http://articlefeeds.nasdaq.com/nasdaq/symbols?symbol='
    url    = paste(url.b1, symbolVec[i], sep = '')
    url    = URLencode(url)
    urlList[length(urlList)+1] <<- url
    #_________________fc_______________________________
    url.b1 = 'http://markets.financialcontent.com/stocks/action/rssfeed?'
    url    = paste(url.b1, "Symbol=", symbolVec[i], sep = '')
    url    = URLencode(url)
    urlList[length(urlList)+1] <<- url    
  }
  url.b1 = 'http://markets.financialcontent.com/stocks/action/rssfeed?'
  url    = paste(url.b1, "Category=financial", sep = '')
  url    = URLencode(url)
  urlList[length(urlList)+1] <<- url 

  return(paste("{",makeUrlJson(urlList), "}", sep=""))  
}



# Call the above  Function
makeUrls()
save(  urlList, file= "/home/musigma/MRIP/wd/urlList.RData")

#_______________________________________________________________________________________________________________
# Functions to remove a list of URLS
#_______________________________________________________________________________________________________________

removeUrl <- function(url){
  if(url %in% urlList){
    urlList <<- urlList[-which(url == urlList)] 
    save(  urlList, file= "/home/musigma/MRIP/wd/urlList.RData")	
  }
}

removeUrls <- function(urlL){
  flog.info("URLs to remove: %s",urlL, name="info_news")
  urlL <- unlist(strsplit(urlL, ";"))
  xyz=lapply(urlL, FUN = function(X){removeUrl(X)})
  flog.info("Number of URLs removed:%d",length(urlL), name="info_news")
  urlJson <- makeUrlJson(urlList)
  flog.info("URLs removed successfully", name="info_news")
  return(paste("{",urlJson, "}", sep=""))
}
#_______________________________________________________________________________________________________________
# Functions to restore a list of URLS
#_______________________________________________________________________________________________________________
restoreUrls <- function(){
   flog.info("URLs requested to restore", name="info_news")
   urlList <<- c()
   json <- makeUrls()
   save(  urlList, file= "/home/musigma/MRIP/wd/urlList.RData")
   flog.info("URLs restored successfully", name="info_news")
   return(json)
}
#_______________________________________________*END*_____________________________________________________________