# ======================================================================================#
#-- Project Name: MINT - real time ticker Update Agent
#-- Task : to send out a json of the latest updated tick
#-- Version : 1.0
#-- Date : 04 Apr 2013
#-- Author : Karthik
#-- SVN Directory: \xxxx
#-- Dependent files : 
#-- Dependent agents : 
#-- Comments : 1. The consumption happens from the consumer agent
#=======================================================================================#
#-- Glossary of Error Codes : 1. 400 : Connection to activeMQ on 162.192.100.46 failed
#--                           2.  0  : No error
#--                           3. 10  : R version error
#--                           4. 11  : R package installation error
#--                           5. 501 : Data not received from consumer agent
#=======================================================================================#

#==============================
# Get the environment variables
#==============================
mripPath = Sys.getenv("MRIP_HOME")

a<-0
errorCode<-0
json <- ""

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
a[1]<-checkpackver("futile.logger",'1.3.0')

if(a[1]){
  errorCode<<-11
}

#________________________________________________________________________________________
#Loading the required libraries
#_________________________________________________________________________________________

library('futile.logger')

#=================================
# Setting futile logger thresholds
#=================================

flog.threshold(INFO, name="info_rt")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/realTimeJsonAgent_info.log",sep="")), name="info_rt")
flog.threshold(ERROR, name="error_rt")
flog.appender(appender.file(paste(mripPath,"/MINTlogs/realTimeJsonAgent_error.log",sep="")), name="error_rt")

flog.info("R libraries loaded succesfully", name="info_rt")

#____________________________________________________________________________________________________________________
# Make the real time JSON (this json contains the latest tick) Why this??
#____________________________________________________________________________________________________________________
makeRTJson = function(tickerId, price, tt, vol){
    json <- paste("{\"realTime\":{\"id\":", "\"", tickerId, "\"", ",\"summaryTicks\":[{\"price\":", price, ",\"volume\":", vol, ",\"date\":", "\"", tt, "\"","}]}", sep="")
    return(json)
}

#____________________________________________________________________________________________________________________
# Stream data parser function
#____________________________________________________________________________________________________________________

getRealTimeJSMain = function(streamData){
    if(streamData != "ErrorCode = 400"){
       # Extract the fields from the tick data
	   streamData <- strsplit(streamData, ",")
	   #extract error code
	   errorCode <<- as.numeric(strsplit(streamData[[1]][length(streamData[[1]])], ":")[[1]][2])
	   streamData <- streamData[[1]][-length(streamData[[1]])]
       if(length(streamData)!=0){
          tickerId <- streamData[1]
	      askPrice <- streamData[2]
	      tradeTime <<- streamData[3]
		  vol <- streamData[4]
		
	      # create a list of tickers and its values 
		  
		     lastTicked <<- tickerId
		     # json to send out the real time ticks
		     json <<- makeRTJson(tickerId, askPrice, tradeTime, vol)
		     json <- paste(json, ",\"ErrorCode\":", errorCode, "}",sep="")
		     return(json)
		  
	   }else{
	      flog.error("The message was not received from the queue! Check network connections", name="error_rt")
		  return(paste(json, ",\"ErrorCode\":501}",sep=""))
	   }
	}else{
	   flog.error("The consumer queue is down", name="error_rt")
	   return(paste(json, ",\"ErrorCode\":400}",sep=""))
	}
}

#_____________________________________________________*END*__________________________________________________________________________