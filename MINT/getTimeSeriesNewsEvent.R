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

#==============================
# Get the environment variables
#==============================
mripPath = Sys.getenv("MRIP_HOME")

errorCode<-0
a<-0
number <- 2
urlList <- c()
newsJson <- ""

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

a[1]<-checkpackver("stringr",'0.6.2')
a[2]<-checkpackver("XML",'3.96-1.1')
a[3]<-checkpackver("plyr",'1.8')
a[4]<-checkpackver("lubridate",'1.2.0')
a[5]<-checkpackver("RPostgreSQL",'0.3-3')
a[6]<-checkpackver("futile.logger",'1.3.0')

if(a[1]|a[2]|a[3]|a[4]|a[5]|a[6]){
  errorCode<<-11
}


#________________________________________________________________________________________
#Loading the required libraries
#_________________________________________________________________________________________

require("XML") 
require("plyr") 
require("stringr")
require("lubridate")
library("RPostgreSQL")
library("futile.logger")

#=================================
# Setting futile logger thresholds
#=================================

flog.threshold(INFO, name="info_news")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/news_info.log",sep="")), name="info_news")
flog.threshold(ERROR, name="error_news")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/news_error.log",sep="")), name="error_news")

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

#___________________________________________________________________________________________________________________________________________________
# Event handler function
#___________________________________________________________________________________________________________________________________________________

getTimeSeries <- function(TID){
  flog.info("before TID = %s",TID,name="info_news")
  source(paste(mripPath,"/wd/loadWorkspace.R",sep=""))
  flog.info("after TID = %s",TID,name="info_news")
   if(errorCode!=600){
     return(getSplWithTS(TID))
   }else{
     return("{\"ErrorCode\":600}")
   }
}

#_______________________________________________*END*_____________________________________________________________