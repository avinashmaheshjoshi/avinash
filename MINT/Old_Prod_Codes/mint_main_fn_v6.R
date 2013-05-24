# ======================================================================================#
#-- Project Name: MINT
#-- Task : compute the correlation graph of streaming data ,MST and find its sammons projection and perform community detection
#-- version : 7.0
#-- date : 9/JAN/13
#-- authors : Subir/Karthik/Vignesh
#-- SVN Directory: \xxxx
#-- Dependent files: 1. db.properties
#--                  2. prediction.properties
#-- Comments : 1. This file querys the database each time the market opens through a function
#--            2. The distance metric for Sammon's is 1-|rho|
#--            3. This file uses raw prices
#--            4. Gets data chunk from Java
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
#--                        8. 400 : Connection to activeMQ on 162.192.100.48 failed
#--                        9.  0  : No error
#_______________________________________________________________________________________#

# initilizing parameters 

errorCode<-0
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

checPkgVec[1]<-checkpackver("ape",'3.0-6')
checPkgVec[2]<-checkpackver("MASS",'7.3-22')
checPkgVec[3]<-checkpackver("igraph",'0.6-3')
checPkgVec[4]<-checkpackver("qgraph",'1.1.0')
checPkgVec[5]<-checkpackver("RPostgreSQL",'0.3-3')
checPkgVec[6]<-checkpackver("rjson",'0.2.11')
checPkgVec[7]<-checkpackver("xts",'0.8-0')
checPkgVec[8]<-checkpackver("Rjms",'0.0.4')

if(any(checPkgVec!=0)){
  errorCode<<-11
}

#______________________________
#Loading the required libraries
#______________________________

library("RPostgreSQL")
library('Rjms')
library("rjson")
library("qgraph")
library("ape")
library("igraph")
require("MASS")

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
  dbDisconnect(con1)
}else{
  errorCode <<- 201
}

# if the connection to the database containing the ticker symbols fails 
if(errorCode==201){
  symbolVec <- suppressWarnings(as.vector(as.matrix(read.table("/home/musigma/jade/agents/prediction.properties", header = F, sep = ",", skip = 3))))
}

#_________________initilize global variables____________________________________

samObj2D <- samObj3D <- samMetaObj2D <- samMetaObj3D <- initforceDir <- NULL
prevJson<-""
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
#consumer<-initialize.consumer('tcp://162.192.100.46:61616','T','jadeStockStream')
rawFrameCount <- 0
updateCount <- 100
rawDfFrameSize <- 10000
closedFlag<-TRUE





#______________________________________________________________________________________________________________________________________________________________
# querying the database for the initial values for the first row of the data frame
#_______________________________________________________________________________________________________________________________________________________________

drv <- dbDriver("PostgreSQL")

dbProperties <- suppressWarnings(as.vector(as.matrix(read.table("/home/musigma/jade/agents/db.properties", header = F, sep = ","))))

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
    dbDisconnect(con)
  }else{
    errorCode <<- 200
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
# Function to create a data frame where each row is a new tick
#______________________________________________________________________________________

chargeDfForReturns = function(res){
  
  if(rawFrameCount == 0){
    retDf <<- res
    rawFrameCount <<- rawFrameCount + 1
  }else{
    retDf[nrow(retDf)+1,] <<- res
    rawFrameCount <<- rawFrameCount + 1
  }
  # if the frame size limit is reached, pop the first row of the DF
  if(rawDfFrameSize < rawFrameCount){
    retDf <<- retDf[-1,]
    rawFrameCount <<- rawFrameCount - 1
  }
  
  return(retDf)
}
#_______________________________________
# Function create the correlation matrix
#_______________________________________
getCor <- function(mat){
  return(cor(mat))
}

#__________________________________________________________________________________
# Sammons projection
# Needs a symmetric matrix,correlation matrices are symmetric
# so nothing to worry
#Returns a nRow*dimensions matrix
#__________________________________________________________________________________

getSammonsXYZ2D <- function(corMatrix,dimension=2,nIterations=10000,tolerance=1e-6){
  #Check the matrix if it is sqaure and symmetric
  if(nrow(corMatrix)!=ncol(corMatrix)){
    return(-1)
  }
  if(!isSymmetric(corMatrix)){
    return(-1) 
  }
  #Remove NA correlation values for sammons
  naRows = NA
  if(length(which(is.na(corMatrix))) != 0){
    naRows <- which(table(which(is.na(corMatrix),arr.ind=T)[,2])==(nrow(corMatrix)-1))
    #rownames(corMatrix)
    cnames<-rownames(corMatrix)[naRows]
    corMatrix <- corMatrix[-as.vector(naRows), -as.vector(naRows)]
  }
  #Take the absolute values of the correlations
  corMatrix = abs(corMatrix)
  corNames <- rownames(corMatrix)
  #Take the inverse of the elements
  corMatrix <- 1-corMatrix
  if(length(which(is.nan(corMatrix)))!=0){
    errorCode<<-100
    return(100)
  }
  rownames(corMatrix) <- colnames(corMatrix) <- corNames
  
  if(!(dim(corMatrix)[1]<=2)){
    if(is.null(samObj2D)){
      tryCatch({
        samObj2D <<- sammon(corMatrix,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points}
               ,error=function(e){y<-cmdscale(corMatrix,dimension)
               if(anyDuplicated(y)){
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
    }else{
      samObj2D <<- try(sammon(corMatrix,y=samObj2D,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points, silent =T)
      if(class(samObj2D)=="try-error"){
        samObj2D <<- try(sammon(corMatrix,y=samMetaObj2D,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points, silent =T)
        if(class(samObj2D)=="try-error"){
          tryCatch({
            samObj2D <<- sammon(corMatrix,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points}
                   ,error=function(e){y<-cmdscale(corMatrix,dimension)
               if(anyDuplicated(y)){
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
    if(!is.na(naRows)[1]){
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
  }else{
    #return("error:Insufficient data")
    if((errorCode != 200) && (errorCode != 201)){
      errorCode<<-100
    }
    return(100)
  }
}

getSammonsXYZ3D <- function(corMatrix,dimension=3,nIterations=10000,tolerance=1e-6){
  #Check the matrix if it is sqaure and symmetric
  if(nrow(corMatrix)!=ncol(corMatrix)){
    return(-1)
  }
  if(!isSymmetric(corMatrix)){
    return(-1) 
  }
  #Remove NA correlation values for sammons
  naRows = NA
  if(length(which(is.na(corMatrix))) != 0){
    naRows <- which(table(which(is.na(corMatrix),arr.ind=T)[,2])==(nrow(corMatrix)-1))
    #rownames(corMatrix)
    cnames<-rownames(corMatrix)[naRows]
    corMatrix <- corMatrix[-as.vector(naRows), -as.vector(naRows)]
  }
  #Take the absolute values of the correlations
  corMatrix = abs(corMatrix)
  corNames <- rownames(corMatrix)
  #Take the inverse of the elements
  corMatrix <- 1-corMatrix
  if(length(which(is.nan(corMatrix)))!=0){
    errorCode<<-100
    return(100)
  }
  rownames(corMatrix) <- colnames(corMatrix) <- corNames
  
  if(!(dim(corMatrix)[1]<=2)){
    if(is.null(samObj3D)){
      tryCatch({
        samObj3D <<- sammon(corMatrix,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points}
               ,error=function(e){y<-cmdscale(corMatrix,dimension)
               if(anyDuplicated(y)){
                 while(anyDuplicated(y))
                 {
                   p<-anyDuplicated(y)
                   # offset the coordinates slightly
                   y[p,]=y[p,]+runif(1)
                 }
                 samObj3D <<- sammon(corMatrix,y,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points
                 }}
               ,finally={
                 samMetaObj3D <<- samObj3D
               })
    }else{
      samObj3D <<- try(sammon(corMatrix,y=samObj3D,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points, silent =T)
      if(class(samObj3D)=="try-error"){
        samObj3D <<- try(sammon(corMatrix,y=samMetaObj3D,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points, silent =T)
        if(class(samObj3D)=="try-error"){
          tryCatch({
            samObj3D <<- sammon(corMatrix,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points}
                   ,error=function(e){y<-cmdscale(corMatrix,dimension)
               if(anyDuplicated(y)){
                 while(anyDuplicated(y))
                 {
                   p<-anyDuplicated(y)
                   # offset the coordinates slightly
                   y[p,]=y[p,]+runif(1)
                 }
                 samObj3D <<- sammon(corMatrix,y,k=dimension,niter=nIterations,trace=FALSE,tol=1e-6)$points
                 }}

                   ,finally={})        
        }
      }
      samMetaObj3D <<- samObj3D
    }
    # assign arbit vaues of coordinates which were NA
    if(!is.na(naRows)[1]){
      outliers = c()
      maxX <- max(samObj3D[,1])
      maxY <- max(samObj3D[,2])
      maxZ <- max(samObj3D[,3])
      
      for(i in 1:length(naRows)){
        outliers <- c(outliers, c(maxX+runif(1,min=0.1,max=0.2), maxY+runif(1,min=0.1,max=0.2), maxZ+runif(1,min=0.1,max=0.2)))
      }
      
      outliers <- matrix(unlist(outliers), nrow = i,ncol=3, byrow = T)
      rownames(outliers) <- cnames
      samObj3D <<- rbind(samObj3D, outliers)
    }
    return(samObj3D)
  }else{
    #return("error:Insufficient data")
    if((errorCode != 200) && (errorCode != 201)){
      errorCode<<-100
    }
    return(100)
  }
}

#__________________________________________
# Function to get the force directed layout 
#__________________________________________

getForceDir <- function(corMat){
  
  if(is.null(initforceDir)){
    initforceDir <<- qgraph(corMat, layout = "spring", DoNotPlot = TRUE)$layout
  }
  
  layout.par=list(init=initforceDir)
  
  forceDir <- qgraph(corMat, layout = "spring", layout.par = layout.par, DoNotPlot = TRUE)$layout
  initforceDir <<- forceDir
  return(forceDir)
}

#_________________________
# Function to get the MST 
#_________________________

getMst <- function(mat){
  return(mst(mat))
}

#_________________________________________
# Function to get community and centrality
#_________________________________________

#using igraph for community detection
getCommCent <- function(corMat,adjMat){
  tryCatch({
    
    #creating weighted adjacency matrix
    adjnew <- abs(corMat * adjMat)
    class(adjnew) <- "matrix"
    #adjnew<-abs(matrix(adjnew,nrow=sqrt(length(adjMat)),dimnames=dimnames(corMat)))
    G1<-graph.adjacency(adjnew,mode="undirected",weighted=TRUE)
    
    #leading eigenvector algorithm to detect communities
    lec_mst <- leading.eigenvector.community(G1)
    
    
    
    #calculating the central tickers in each community
    central<-0
    lmm<-lec_mst$membership
    cv<-centralization.evcent(G1)$vector
    
    central<-sapply(1:max(lmm), function(i)
      intersect(which(cv==max(cv[c(which(lmm==i))])),which(lmm==i)))
    return(list(lec_mst,central))
  }, error = function(err){
    errorCode <<- 300
    return(errorCode)
  }, finally = {})
  
}

#______________________________________________________________________________________
# Json for visualizing the dataframe
#______________________________________________________________________________________

makeDFJson = function(datF){
  
  dfJson = paste("'dataF:'", toJSON(datF), sep="")
  return(dfJson)
}


#____________________________________________________________________________________
# Function to create the JSON for UI
#____________________________________________________________________________________
jsonCreator <- function(corMat, samMat, adjMat){
  
  #Calling community and centrality function
  commCent <- getCommCent(corMat,adjMat)
  lec_mst <- commCent[[1]]
  central <- commCent[[2]]
  
  values<- corMat[lower.tri(corMat, diag = F)]
  dims <- which(lower.tri(corMat, diag = F), arr.ind =T)-1
  dims<- cbind(dims, rep(0, nrow(dims)))
  dims[which(adjMat[lower.tri(adjMat, diag = F)]==1, arr.ind = T),3] = 1
  #-------------------------------------------------------------------------------------------------------
  # Correlation Json (jsonCor)
  #-------------------------------------------------------------------------------------------------------
  jsonCor = "\"relations\":"
  jsonCor[1]<-paste(jsonCor, "[{\"source\":", dims[1,2],",", "\"target\":", dims[1,1],",", "\"value\":", values[1],",", "\"a\":", dims[1,3],"}")
  jsc<-lapply(2:length(values), function(i)
    jsonCor[i]<- paste("{\"source\":", dims[i,2],",", "\"target\":", dims[i,1],",", "\"value\":", values[i],",", "\"a\":", dims[i,3],"}")) 
  jscfinal<-paste(jsc,sep=",",collapse=",")
  jsonCor<-paste(jsonCor,",",jscfinal, "]")
  #_______________________________________________________________________________________________________
  
  #_------------------------------------------------------------------------------------------------------
  # Sammons json (jsonSam)
  #-------------------------------------------------------------------------------------------------------
  p<-0
  k<-0
  #This part gives Sammons along with community to which that ticker 
  #belongs and also if it is the centralnode in that community
  if(!is.null(samMat)){
    jsonSam ="\"sammons\":["
    #p[1]<-which(samMat[,1]==symbolVec[1])
    p[1] <- 1
    
    k[1] <- as.numeric(1 %in% central)
    
    jsonSam <- paste(jsonSam,"{\"ticker\":" ,paste( "\"", samMat[p[1],1],"\"",sep=""), ",", "\"xs\":", samMat[p[1],2], ",", "\"ys\":", samMat[p[1],3],",", "\"xFD\":", samMat[p[1],7],",", "\"yFD\":", samMat[p[1],8],",","\"xt\":", samMat[p[1],4], ",", "\"yt\":", samMat[p[1],5],",","\"zt\":",samMat[p[1],6],",","\"Community\":",lec_mst$membership[1],",","\"central\":",k[1],"}")  
    jss<- lapply(2:nrow(samMat), function(i){
      p[i]<- i
      k[i]<- as.numeric(i %in% central)
      jsonSam[i] <- paste("{\"ticker\":" ,paste( "\"", samMat[p[i],1],"\"",sep=""), ",", "\"xs\":", samMat[p[i],2], ",", "\"ys\":", samMat[p[i],3],",", "\"xFD\":", samMat[p[i],7],",", "\"yFD\":", samMat[p[i],8],",","\"xt\":", samMat[p[i],4], ",", "\"yt\":", samMat[p[i],5],",","\"zt\":",samMat[p[i],6],",","\"Community\":",lec_mst$membership[i],",","\"central\":",k[i],"}")})    
    jssfinal<-paste(jss,collapse=",")
    jsonSam<-paste(jsonSam,",",jssfinal, "]")  
  }else{
    jsonSam = "\"sammons\":[\"NULL\"]"
  }
  #-------------------------------------------------------------------------------------------------------
  # Nodes Json
  #-------------------------------------------------------------------------------------------------------
  nodes = paste("\"nodes\":[{\"id\":","\"", symbolVec[1],"\"}", sep ="")
  for(i in 2:length(symbolVec)){
    nodes <- paste(nodes, paste("{\"id\":","\"", symbolVec[i],"\"}", sep =""), sep=",")
  }
  nodes<- paste(nodes, "]")
  #-------------------------------------------------------------------------------------------------------
  
  return (paste( "{", paste(jsonSam, "\"graph\":{",sep=","), paste(jsonCor, nodes,sep=","), "}"))
}
#_________________________________________________________________________________________________________


#____________________________________________________________________________________
# Function to call it all (get the corr graph)
#____________________________________________________________________________________
mint_main_fn = function(dat){
  
  debugFlag <- F
  #tryCatch({
  # to refresh the data frame when the market is open
  # closedFlag ensures that that the dataFrame is refreshed only once
  if(!is.na(marketIndicator)){
    if(marketIndicator == "closed"){
      closedFlag<<-TRUE
    }
    if(marketIndicator == "open" && closedFlag){
      retDf <<- as.data.frame(matrix(-1, nrow = 1, ncol=nSymbols))
      colnames(retDf) <<- symbolVec
      rawFrameCount <<- 0
      dfMatrix <<- matrix(-1,nrow=frameSize,ncol=nSymbols+1)
      fillCount <<-1
      readFromDb()
      closedFlag <<- FALSE
    }
  }
  
  # updateCount is the number of rows of the data drame to be updated after every iteration
  for(i in 1:updateCount){
    # check for ActiveMQ connection
    tryCatch({streamData <- consumeFn()}, error=function(err){errorCode<<-400}, finally={})
    # res1 is the data frame that contains 1 latest row of data
    if((errorCode != 400) && exists("streamData")){
       res1 = dfCharge_for_multi_ticks(streamData)
    }else{
       errorCode <<- 0
       Sys.sleep(10)
       # send the last updated json with the error code of 400
       return(paste(prevJson,"\"ErrorCode\":400}", sep=","))
    } 
    #remove the time column
    res1 <- res1[,-ncol(res1)]
    if(!(-1 %in% res1)){
      # Charge the dataframe where each row is a timeLim period of data
      res2 <- chargeDfForReturns(res1)
    }
  }
  
  
  if(rawFrameCount  >= 2000){
    
    pearson_mat = getCor(res2)
    # sometimes, when the frame size is too low, we may get cor values to be 1/-1. To circumvent this, we make the cor values 0.99
    correlationOnes <- setdiff(which(abs(pearson_mat)==1),setdiff(which(lower.tri(pearson_mat, diag=T)), which(lower.tri(pearson_mat, diag = F))))
    if(length(correlationOnes) != 0){
      pearson_mat[correlationOnes] = pearson_mat[correlationOnes]-0.01
    }
    #________________________________________________________________________________
    # 1. if the column remains constant, the cor value returned is NA. for this,
    # we replace that value with 0
    # 2. may throw an error sometimes depending on the data coming in:
    # invalid initial configuration
    #__________________________________________________________________________________
    
    sammonsMatrix2D <- getSammonsXYZ2D(pearson_mat)
    sammonsMatrix3D <- getSammonsXYZ3D(pearson_mat)
    
    if(any(rownames(sammonsMatrix2D) != symbolVec) || any(rownames(sammonsMatrix3D) != symbolVec)){
      sammonsMatrix2D <- sammonsMatrix2D[symbolVec,]
      sammonsMatrix3D <- sammonsMatrix3D[symbolVec,]
    }
    
    suppressWarnings(if((sammonsMatrix2D == -1) || (sammonsMatrix2D ==100)) {
      sammonsMatrix2D = NULL
    })
    
    suppressWarnings(if((sammonsMatrix3D == -1) || (sammonsMatrix3D ==100)) {
      sammonsMatrix3D = NULL
    })
    
    if(length(which(is.na(pearson_mat))) != 0){
      pearson_mat[which(is.na(pearson_mat))] = 0
      
    }
    #write.csv(pearson_mat, paste("mat", as.character(iterC), ".csv", sep=""))
    iterC <<- iterC + 1
    
    forceDir <- getForceDir(pearson_mat)
    
    if(!is.null(sammonsMatrix2D) && !is.null(sammonsMatrix3D)){
      #merging the two objects for json
      sammonsMatrix <- cbind(rownames(sammonsMatrix2D), sammonsMatrix2D, sammonsMatrix3D)
      #forceDir <- cbind(colnames(pearson_mat), forceDir)
      #colnames(forceDir)<-c("ID", "x", "y")
      #colnames(sammonsMatrix)<- c("ID", "x", "y", "z")
      samObjFD <- cbind(sammonsMatrix,forceDir)
      colnames(samObjFD) <- c("ID","xs","ys","xt","yt","zt","xFD","yFD")
    }else{
      samObjFD<-NULL
    }
    # MST adjacency matrix
    
    # Normalizing corr values to a distance matrix
    pearson_mat <- round(pearson_mat, digits=3)
    distMat <- sqrt(2*(1-abs(pearson_mat)))
    adjMat <- getMst(distMat)
    json <- jsonCreator(pearson_mat, samObjFD, adjMat)
    eJson <- paste("\"ErrorCode\":",errorCode,sep="")
    marketFlag <- paste("\"marketStatus\":","\"",marketIndicator,"\"",sep="")
    dfSize <- paste("\"DataFrameSize\":","\"",rawFrameCount, "/", rawDfFrameSize,"\"",sep="")
    json <- paste(json, paste("\"TimeStamp\":", "\"",tradeTime,"\"", sep=""), marketFlag, dfSize, sep=",")
    # the previous json w/o the error code
    prevJson <<- json
    json <- paste(json,eJson,sep = ",")
    json <- paste(json, "}", sep="")
    errorCode<<-0
    
    # to display the data frame for debug purpose
    if(debugFlag){
      if(nrow(res2)<6){
        j <- makeDFJson(res2[nrow(res2),])
      }
      else{
        j <- makeDFJson(res2[(nrow(res2)-5):nrow(res2),])
      }
      json <- paste(json, j, sep = ",")
    }  
    #write.csv(json, "j.csv")
    return(json)
  }
  else{
    if((errorCode != 200) && (errorCode != 201)){
      prevJson <<- paste("{\"sammons\":\"NULL\",\"graph\":\"NULL\", \"TimeStamp\":", "\"",tradeTime,"\"", ",\"marketStatus\":","\"",marketIndicator,"\"",",\"DataFrameSize\":","\"", rawFrameCount, "/", rawDfFrameSize,"\"","," ,"\"ErrorCode\":100}", sep = "")
      return(prevJson)
    }
    else{
      prevJson <<- paste("{\"sammons\":\"NULL\",\"graph\":\"NULL\",\"TimeStamp\":","\"", tradeTime,"\"", ",\"marketStatus\":","\"",marketIndicator,"\"",",\"DataFrameSize\":","\"",rawFrameCount, "/", rawDfFrameSize,"\"", "," , "\"ErrorCode\":", errorCode,"}", sep="")
      return(prevJson)
    }
  }
  #}, error=function(err){
  # write.csv("'ErrorCode':-1", "j.csv")
  # return("'ErrorCode':-1, 'Correlation': 'NULL', 'Sammons': 'NULL'")
  #}, finally={})
  

  
}


