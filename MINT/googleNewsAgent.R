# ======================================================================================#
#-- Project Name: MINT : Google News agent
#-- Task : Fetch RSS feeds from Google and send to UI
#-- Version : 1.0
#-- Date : 25 Apr 2013
#-- Author : Karthik
#-- SVN Directory: \xxxx
#-- Dependent files : 
#-- Dependent agents : 
#-- Comments : 1. 
#=======================================================================================#
#-- Glossary of Error Codes : 1. 400 : Connection to activeMQ on 162.192.100.46 failed
#--                           2.  0  : No error
#--                           3. 800 : Connection error
#--                           4. 11  : R package installation error
#--                           5. 700 : URLs not found
#--                           6. 600 : URLList not found
#=======================================================================================#

#==============================
# Get the environment variables
#==============================
mripPath = Sys.getenv("MRIP_HOME")

errorCode<-0
a<-0
number <- 2
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
flog.appender(appender.file(paste(mripPath,"/MINTlogs/googleNews_info.log",sep="")), name="info_news")
flog.threshold(ERROR, name="error_news")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/googleNews_error.log",sep="")), name="error_news")

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

#load the urlList obj
tryCatch({load(paste(mripPath,"/wd/urlList.RData",sep=""))}, error=function(err){errorCode<<-600;flog.error("RData file not found", name= "error_news")}, finally={})

#________________________________________________________________________________________
# Function to extract news from Google finance
#_________________________________________________________________________________________

getNewsFromGoogle <- function(url){  
  
  
  # parse xml tree, get item nodes, extract data and return data frame
  tryCatch({doc   = xmlTreeParse(url, useInternalNodes = T);}, error=function(err){errorCode<<- 800}, finally={})
  if(errorCode !=800){
    nodes = getNodeSet(doc, "//item");
  
    newsDf  = ldply(nodes, as.data.frame(xmlToList))
  
    # clean up names of data frame
    names(newsDf) = str_replace_all(names(newsDf), "value\\.", "")
  
    # convert pubDate to date-time object and convert time zone
    newsDf$pubDate = strptime(newsDf$pubDate, format = '%a, %d %b %Y %H:%M:%S', tz = 'GMT')
    newsDf$pubDate = with_tz(newsDf$pubDate, tz = 'America/New_york')
    newsDf$pubDate = as.character(newsDf$pubDate)
  
    # drop guid.text and guid..attrs
    newsDf$guid.text = newsDf$guid..attrs = NULL
  
    # Remove the HTML tags from the description  
    if(nrow(newsDf)!=0){
      desc = getNodeSet(nodes[[1]], "//description")
      desc[[1]] <- NULL
      descVec <- lapply(getNodeSet(htmlParse(lapply(desc, FUN=xmlValue)), "//div"), FUN=xmlValue)
      
      if(length(descVec) %% number ==0){
        descVec <- descVec[-which(1:length(descVec) %% 2 ==1)]
      }else{
        descVec[length(descVec)+1] <- descVec[length(descVec)]
        descVec <- descVec[-which(1:(length(descVec)) %% 2 ==1)]
      
      }
      descCol<-do.call("rbind", descVec)
    
      newsDf$description <- descCol
	  
	  # replace the "" with '' for the sake of jsons
	  newsDf$description <- gsub( "\"","'", newsDf$description)
      newsDf$title <- gsub( "\"","'", newsDf$title)
	  # replace "\n" with " "
	  newsDf$description <- gsub( "\n"," ", newsDf$description)
      newsDf$title <- gsub( "\n"," ", newsDf$title)
    }
    
    return(newsDf)  
  }else{
    errorCode <<- 0
    return(800)
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
# Function to make the news JSON
#________________________________________________________________________________________

makeNewsJson <- function(newsObj){
  if(!is.data.frame(newsObj)){
    # converting the list to a data frame
    dfr <- do.call("rbind", newsObj)
  }else{
    dfr <- newsObj
  }
  json <- paste("{\"googleNews\":[{\"title\":", "\"", dfr[1,1], "\"",",\"link\":", "\"",dfr[1,2], "\"",",\"date\":", "\"",dfr[1,3],"\"", ",\"desc\":", "\"",dfr[1,4],"\"", "}", sep="")
  for(i in 2:nrow(dfr)){   
    json <- paste(json, paste("{\"title\":", "\"", dfr[i,1], "\"",",\"link\":", "\"",dfr[i,2], "\"",",\"date\":", "\"",dfr[i,3],"\"", ",\"desc\":", "\"",dfr[i,4],"\"", "}", sep=""),sep=",")
  }
  json <- paste(json,  "],", makeUrlJson(urlList), sep="")
  return(json)   
}

#________________________________________________________________________________________
# Main function
#________________________________________________________________________________________

getGoogleNews <- function(){ 
  tryCatch({source("/home/musigma/MRIP/wd/urlListLoader.R")}, error=function(err){errorCode<<-600;flog.error("RData file not found", name= "error_news")}, finally={})
  
  if(errorCode != 600){
    urlList <<- urlList
    googleUrls <- urlList[grep("google", urlList)]
	if(length(googleUrls!=0)){
      dframe <- lapply(googleUrls, FUN=getNewsFromGoogle)
      if(800 %in% dframe){
	    if(newsJson == ""){
	      flog.error("Some Connection failure, internet down on server", name= "error_news")
		  errorCode <<- 0
	      return("{\"ErrorCode\":800}")
	    }else{
	      flog.error("Some Connection failure, internet down on server", name= "error_news")
		  errorCode <<- 0
          return(paste(newsJson, ",\"ErrorCode\":800}", sep = ""))
	    }
      }
      newsJson <<- makeNewsJson(dframe)
	  json <- paste(newsJson, ",\"ErrorCode\":", errorCode, "}", sep="")
      return(json)
	}else{
	    if(newsJson == ""){
	      flog.error("The google Urls's not found", name= "error_news")
		  errorCode <<- 0
	      return("{\"ErrorCode\":700}")
	    }else{
	      flog.error("The google Urls's not found", name= "error_news")
		  errorCode <<- 0
          return(paste(newsJson, ",\"ErrorCode\":700}", sep = ""))
	    }
    }
  }else{
	  errorCode <<- 0
      return("{\"ErrorCode\":600}")
  }
}



#_______________________________________________*END*_____________________________________________________________