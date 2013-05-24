#----------------------------------------------------#
#Set Working Directory                               #
#----------------------------------------------------#

setwd("~/Desktop/Muphoria")

#----------------------------------------------------#
#Declaring variables                                 #
#----------------------------------------------------#
nStores <<- 600 #No. of stores
nStops <<- 35 #No. of stops
maxTravelTime <<- 3*60*60 #In secs
maxDistPerTrk <<- 65 #In kms
maxTrkLoad <<- 1000
distMatrix <<- read.csv("Table1.csv")
rownames(distMatrix) <- distMatrix[,1]
distMatrix <<- distMatrix[,-1]
demandMatrix <<- read.csv("Table2.csv")
rownames(demandMatrix) <- demandMatrix[,1]
firstSolutionNams <- as.data.frame(read.csv("December/out_Rajavinoth.csv",header=FALSE,colClasses = "character"))
for (k in 1:ncol(firstSolutionNams)){
  firstSolutionNams[which(firstSolutionNams[,k]==""),k] = "DC"
  #firstSolutionNams[which(firstSolutionNams[,k]==""),k] = firstSolutionNams[length(which(firstSolutionNams[,k]!="")),k]
}
if(nrow(firstSolutionNams) <= 35){
  for(t in (nrow(firstSolutionNams)+1):36)
    firstSolutionNams[t,] = "DC"
}
firstSolution <<- firstSolutionNams

#----------------------------------------------------#
# Energy function for Muphoria Problem               #
#----------------------------------------------------#

costFnc = function(routeMatrix){
  #routeMatrix[nrow(routeMatrix)+1,] = "DC"
  newRouteMatrix = matrix(0,nrow(routeMatrix),ncol(routeMatrix))
  newRouteMatrix[1,] = apply(as.matrix(routeMatrix[1,]),1,function(x) distMatrix[as.character(x),"DC"] )
  
  for (i in 1:ncol(routeMatrix)){
    for (j in 2:nrow(routeMatrix)){
      newRouteMatrix[j,i] = distMatrix[routeMatrix[j,i],routeMatrix[j-1,i]]
    }
  }
  newRouteMatrix = newRouteMatrix[,which(colSums(newRouteMatrix) != 0)]
  
  if(class(newRouteMatrix) == "numeric"){
    nTrucks = length(newRouteMatrix)
  }
  else{
    nTrucks = ncol(newRouteMatrix)
  }
  
  costOnRoad = sum(newRouteMatrix) * 30 * 365 * 3
  costOnTrks = nTrucks * 20000 * 36 
  totalCost = costOnRoad + costOnTrks
  print(nTrucks)
  print(sum(newRouteMatrix))
  return(totalCost)
}

constraintFnc = function(routeMatrix){
  routeMatrix = as.matrix(routeMatrix)
  #Converting routes to distances
  newRouteMatrix = matrix(0,nrow(routeMatrix),ncol(routeMatrix))
  newRouteMatrix[1,] = apply(as.matrix(routeMatrix[1,]),1,function(x) distMatrix[as.character(x),"DC"] )
  
  for (i in 1:ncol(routeMatrix)){
    for (j in 2:nrow(routeMatrix)){
      newRouteMatrix[j,i] = distMatrix[routeMatrix[j,i],routeMatrix[(j-1),i]]
    }
  }
  #newRouteMatrix = newRouteMatrix[,which(colSums(newRouteMatrix) != 0)]
  
  #Converting routes to loads
  newLoadMatrix = matrix(0,nrow(routeMatrix),ncol(routeMatrix))
  for (i in 1:ncol(routeMatrix)){
    for (j in 1:nrow(routeMatrix)){
      newLoadMatrix[j,i] = demandMatrix[routeMatrix[j,i],2]
    }
  }
  
  trkDistance = colSums(newRouteMatrix)
  
  trkLoad = colSums(newLoadMatrix)
  trkStops = apply(routeMatrix,2,function(x) length(unique(x)) - 1)
  
  trkStopDistance = trkDistance - apply(matrix(1:ncol(newRouteMatrix)),1,function(x) newRouteMatrix[trkStops[x]+1,x])
  trkTime = trkStopDistance*90 + trkStops*5*60
  
  nStoresVisited = sum(trkStops)
  storeVisitFreq = table(routeMatrix)[-which(rownames(routeMatrix) == "DC")]
  
  if(all(trkDistance <= maxDistPerTrk) &&
    all(trkLoad <= maxTrkLoad) && 
    all(trkStops <= nStops) && 
    all(trkTime <= maxTravelTime) &&
    (nStoresVisited == nStores) &&
    all(storeVisitFreq == 1)){
    finalCheck = TRUE
  }
  else{
    finalCheck = FALSE
    #print("Constraints are failing")
    #print("Load check")
    #print(trkLoad)
    #print("trkTime check")
    #print(trkTime)
    #print("trkDistance check")
    #print(trkDistance)
    #print("trkStops")
    #print(trkStops)
    #print("nStoresVisited")
    #print(nStoresVisited)
    #print("storeVisitFreq")
    #print(storeVisitFreq[storeVisitFreq != 1])
  }
  return(finalCheck)
}
#----------------------------------------------------#
# Neighbourhood function for Muphoria Problem        #
#----------------------------------------------------#

flushDC = function(x){
  nElem = length(unique(x))
  if(any(x[1:(nElem-1)] == "DC")){
    dcElems = which(x == "DC")
    newX = c(x[-dcElems],x[dcElems])
  }
  else{
    newX = x
  }
}

neighFnc = function(routeMatrix){
  finalStop = as.matrix(routeMatrix[nrow(routeMatrix),])
  routeMatrix = as.matrix(routeMatrix[-nrow(routeMatrix),])
  neighRouteMatrix = routeMatrix
  nSwaps = 2 
  swapVec = sample(1:length(routeMatrix),(2*nSwaps))
  neighRouteMatrix[swapVec[1:nSwaps]] = routeMatrix[swapVec[(nSwaps+1):(2*nSwaps)]]
  neighRouteMatrix[swapVec[(nSwaps + 1):(2*nSwaps)]] = routeMatrix[swapVec[1:nSwaps]]
  neighRouteMatrix = apply(neighRouteMatrix,2,flushDC)
  neighRouteMatrix = rbind(neighRouteMatrix,"DC")
  prevIter = neighRouteMatrix
  
  for(i in 1:nSwaps){
    while(!constraintFnc(as.matrix(neighRouteMatrix))){
      neighRouteMatrix = prevIter[-nrow(prevIter),]
      neighRouteMatrix = as.matrix(routeMatrix)
      swapVec[i] = sample(1:length(routeMatrix),1)
      swapVec[i+nSwaps] = sample(1:length(routeMatrix),1)
      neighRouteMatrix[swapVec[i]] = routeMatrix[swapVec[(nSwaps+i)]]
      neighRouteMatrix[swapVec[nSwaps + i]] = routeMatrix[swapVec[i]]
      neighRouteMatrix = apply(neighRouteMatrix,2,flushDC)
      neighRouteMatrix = rbind(neighRouteMatrix,"DC")
    }
    prevIter = as.matrix(neighRouteMatrix[-nrow(neighRouteMatrix),])
  }
  return(neighRouteMatrix) 
}


#=========Main Simulated annealing algorithm========#

SA <- function() {
  
  cooling_sched <- 0.999
  maxIters <- 1e15
  stopTemp <- 1e-150
  eMin <- -1e100
  iterationIndicator <- 1000
  xVec <- firstSolution
  eBest <- costFnc(xVec)
  e <- eBest;
  k=1;
  T=1;
  
  result <- NULL
  
  while (k < maxIters && e >= eMin && T > stopTemp){
    newX <- neighFnc(xVec)
    eNew <- costFnc(newX)
    
    if(eNew < eBest){ 
      eBest <- eNew
      e <- eNew
      xVec <- newX
    }
    else {
      if(runif(1,min=0,max=1)< exp((e-eNew)/(k*T) )){
        eBest <- eNew
        e <- eNew
        xVec <- newX
      }
    }
    print(k)
    print(eBest)
    k <- k+1
    T <- cooling_sched*T
    if(k%%iterationIndicator == 0){
      write.csv(xVec,"muResults_newStops_2_newDemand_Dec.csv",sep = ",",row.names = FALSE)
    }
  }
  result <- list("par"=xVec,"value"=eBest, "iterations"=k,"temp"=T)
  
  return(result)  
}

#======End of Simulated annealing====================#  
a = SA()

#mySolution <- as.data.frame(read.csv("muResults_1swap.csv",header=TRUE,colClasses = "character"))
#mySolution <- mySolution[,-1]
#constraintFnc(mySolution)

#toBeTested = c("out_Abhra.csv","out_adithya.csv","out_Mukesh.csv","out_Mukund.csv","out_prashant.csv","out_Sayantan.csv","out_shashikant.csv","out_Vikas.csv")
