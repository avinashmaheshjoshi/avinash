#----------------------------------------------------#
#Set Working Directory                               #
#----------------------------------------------------#

setwd("~//Desktop//Muphoria")

#----------------------------------------------------#
#Declaring variables                                 #
#----------------------------------------------------#
nStores <<- 600 #No. of stores
nStops <<- 20 #No. of stops
maxTravelTime <<- 3*60*60 #In secs
maxDistPerTrk <<- 65 #In kms
maxTrkLoad <<- 1000
distMatrix <<- read.csv("Table1.csv")
rownames(distMatrix) <- distMatrix[,1]
distMatrix <<- distMatrix[,-1]
demandMatrix <<- read.csv("Table2.csv")
rownames(demandMatrix) <- demandMatrix[,1]
firstSolutionNams <- as.data.frame(read.csv("muResults_40swaps.csv",header=FALSE,colClasses = "character"))
for (k in 1:ncol(firstSolutionNams)){
  firstSolutionNams[which(firstSolutionNams[,k]==""),k] = "DC"
  #firstSolutionNams[which(firstSolutionNams[,k]==""),k] = firstSolutionNams[length(which(firstSolutionNams[,k]!="")),k]
}
firstSolutionNams[nrow(firstSolutionNams)+1,] = "DC"
firstSolution <<- firstSolutionNams
#firstSolution <- matrix("",20,40)
#firstSolution <<

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
  print(sum(newRouteMatrix))
  return(totalCost)
}

constraintFnc = function(routeMatrix){
  
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

  trkStopDistance = apply(newRouteMatrix,2,function(x) sum(x) - x[length(unique(x))])
  trkTime = trkStopDistance*90 + trkStops*5*60
  
  if(all(trkDistance <= maxDistPerTrk) &&
     all(trkLoad <= maxTrkLoad) && 
     all(trkStops <= nStops) && 
     all(trkTime <= maxTravelTime)){
    finalCheck = TRUE
  }
  else{
    finalCheck = FALSE
  }
  #print("Load check")
  #print(all(trkLoad <= maxTrkLoad))
  #print("trkTime check")
  #print(all(trkTime <= maxTravelTime))
  #print("trkDistance check")
  #print(trkDistance)
  #print(all(trkDistance <= maxDistPerTrk))
 #if(!all(trkTime <= maxTravelTime)){
#  print(trkTime)
#  }
return(finalCheck)
}

#----------------------------------------------------#
# Neighbourhood function for Muphoria Problem        #
#----------------------------------------------------#

neighFnc = function(routeMatrix){
  routeMatrix = as.matrix(routeMatrix)
  neighRouteMatrix = as.matrix(routeMatrix)
  nSwaps = 60 
  swapVec = sample(1:length(routeMatrix),(2*nSwaps))
  neighRouteMatrix[swapVec[1:nSwaps]] = routeMatrix[swapVec[(nSwaps+1):(2*nSwaps)]]
  neighRouteMatrix[swapVec[(nSwaps + 1):(2*nSwaps)]] = routeMatrix[swapVec[1:nSwaps]]
  
  for(i in 1:nSwaps){
    while((routeMatrix[swapVec[i]] == "DC") || (routeMatrix[swapVec[i+nSwaps]] == "DC") || !constraintFnc(as.matrix(neighRouteMatrix))){
      neighRouteMatrix = as.matrix(routeMatrix)
      swapVec[i] = sample(1:length(routeMatrix),1)
      swapVec[i+nSwaps] = sample(1:length(routeMatrix),1)
      neighRouteMatrix[swapVec[i]] = routeMatrix[swapVec[(nSwaps+i)]]
      neighRouteMatrix[swapVec[nSwaps + i]] = routeMatrix[swapVec[i]]
    }
  }
  return(neighRouteMatrix)  
}

neighFnc1 = function(routeMatrix){
  routeMatrix = as.matrix(routeMatrix)
  neighRouteMatrix = as.matrix(routeMatrix)
  nSwaps = 1 
  swapVec = sample(1:length(routeMatrix),(2*nSwaps))
  neighRouteMatrix[swapVec[1:nSwaps]] = routeMatrix[swapVec[(nSwaps+1):(2*nSwaps)]]
  neighRouteMatrix[swapVec[(nSwaps + 1):(2*nSwaps)]] = routeMatrix[swapVec[1:nSwaps]]
  
  while((routeMatrix[swapVec[1]] == "DC") || (routeMatrix[swapVec[2]] == "DC") || !constraintFnc(as.matrix(neighRouteMatrix))){
    neighRouteMatrix = as.matrix(routeMatrix)
    swapVec[1] = sample(1:length(routeMatrix),1)
    swapVec[2] = sample(1:length(routeMatrix),1)
    neighRouteMatrix[swapVec[1]] = routeMatrix[swapVec[2]]
    neighRouteMatrix[swapVec[2]] = routeMatrix[swapVec[1]]
  }

  return(neighRouteMatrix)  
}


#=========Main Simulated annealing algorithm========#

SA <- function() {
  
  cooling_sched <- 0.995
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
      write.csv(xVec,"muResults.csv")
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

