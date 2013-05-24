#--                                                                                   --#
#--  Project Name:   Facebook ABM--# 
#--  Task : Run a VCG auction and get the winning advertisers--# 
#--  version : 1.0 date :27/04/12 
#--  author : Avinash Joshi--#
#--  SVN Directory: http://xp-dev.com/svn/MxO/Labs/code/Avinash/Facebook--#
#---------------------------------------------------------------------------------------#

#This code takes the dataset containing the advertisers and their bids as input and runs a VCG 
#auction on them. The output is the winning configuration, their payouts and the impressions/clicks
#they have recieved at the end of this auction.
#A couple of points to note are that the code requires some standard naming criterion for the
#coluns. The column names should be as follows, AgentID,AgenttType,AuctionBid,if_CPC,if_CPM,AdCTR.
#Also the types for agent must be "Premium","Netego","Marketplace" as R is case sensitive.

runVCG <- function(dataset,slotValue,scaleUp)
{  
  
  #Initializing required global
  #----------------------------
  slotValue <<- slotValue
  nslots <<- length(slotValue)
  
  #Segregating the different agents into different dataframes
  #----------------------------------------------------------
  premiumAgents <- dataset[which(dataset$AgentType=="Premium"),]
  netEgoAgents <- dataset[which(dataset$AgentType=="Netego"),]
  marketPlaceAgents <- dataset[which(dataset$AgentType=="Marketplace"),]
  
  #Picking the best premium agents
  #-------------------------------
  premiumAgents <- premiumAgents[sort(premiumAgents$AuctionBid,decreasing=TRUE,index = TRUE)$ix,]  
  bestPremiumAgents <- premiumAgents[1:2,] 
  bestPremiumAgents <- bestPremiumAgents[!is.na(bestPremiumAgents),] #A check if less than 2 premium agents are present
  
  #Picking the best Netego agents
  #------------------------------
  netEgoAgents <- netEgoAgents[sort(netEgoAgents$AuctionBid,decreasing=TRUE,index = TRUE)$ix,]
  bestNetEgoAgents <- netEgoAgents[1:(nslots+1),]
  bestNetEgoAgents <- bestNetEgoAgents[!is.na(bestNetEgoAgents$AuctionBid),] #A check if less than nslots+1 Netego agents are present
  
  #Picking the best Marketplace agents
  #-----------------------------------
  marketPlaceAgents <- marketPlaceAgents[sort(marketPlaceAgents$AuctionBid,decreasing=TRUE,index = TRUE)$ix,]
  bestMarketPlaceAgents <- marketPlaceAgents[1:(nslots+1),]
  bestMarketPlaceAgents <- bestMarketPlaceAgents[!is.na(bestMarketPlaceAgents$AuctionBid),]
  
  #A dummy vector to use apply on
  #------------------------------
  indexVector <- nrow(bestNetEgoAgents):0
  
  #A list of all legal Netego and marketplace combinations
  #-------------------------------------------------------
  configList <- lapply(indexVector,getPrice,slotValue,bestNetEgoAgents,bestMarketPlaceAgents)
  
  #A dummy vector to use apply on
  #------------------------------
  premIndexVector <- 0:2
  
  #A list of all legal Netego and Premium combinations
  #---------------------------------------------------
  premConfigs <- lapply(premIndexVector,getPremiumPrice,slotValue,bestPremiumAgents,bestNetEgoAgents)
  
  #Final List of legal combinations
  #--------------------------
  configList <- c(configList,premConfigs)
  
  #Final List of legal and valid combinations
  #------------------------------------------
  configList <- configList[!unlist(lapply(configList,function(x) any(x$config[,"AuctionBid"]<x$config[,"AgentPayout"])))]
  
  #Choosing winner by comparing the total revenue
  #----------------------------------------------
  winningConfig <- configList[which(max(unlist(lapply(configList,function(x)x$Total))) == unlist(lapply(configList,function(x)x$Total)))]
  bestConfig <- as.data.frame(winningConfig[[1]]$config)
  
  #The desired output
  #-----------------
  auctionResult <- cbind(bestConfig,Impressions = rep(scaleUp,nrow(bestConfig)),clicks = scaleUp*as.numeric(bestConfig$AdCTR))
  
  return(auctionResult)
}

#Functions that generates a valid combination and calculates the payout for agents

#Netego + Marketplace 
getPrice <- function(x,slotValue,bestNetEgoAgents,bestMarketPlaceAgents)
{ 
  AgentPayout = NULL
  BidType = NULL
  
  #Chooses the slot value vector elements that are required 
  #if they don't add up to 7 the following condition is applied
  #--------------------------------------------------------
  slotValue = slotValue[1:max(nrow(bestNetEgoAgents),nrow(bestMarketPlaceAgents))]
  slotValue = slotValue[!is.na(slotValue)]
  
  #Generates a configuration while x moves from 8 to 0, the number of Netego ads will go 
  #from 8 to 0 and marketplace will go from 0 to 8
  validConfig <- rbind(bestNetEgoAgents[0:x,],bestMarketPlaceAgents[0:(nslots+1-x),])
  validConfig <- validConfig[!is.na(validConfig$AuctionBid),]
  while (nrow(validConfig) < nslots+1)
  {
    validConfig <- rbind(validConfig,0)
  }
  
  #Caclucates the difference of the slot with the next slot value and multiplies with the corresponding bid
  #--------------------------------------------------------------------------------------------------------
  diffSlot <- c(-diff(slotValue),slotValue[nslots])
  product <- validConfig$AuctionBid[-1] * diffSlot
  
  #Finds the payout by caluclating the cumulative sum of the above vector
  #----------------------------------------------------------------------
  VCGpayout <-  rev(cumsum(rev(product)))
  
  AgentPayout[which(validConfig$if_CPM==1)] <- VCGpayout[which(validConfig$if_CPM==1)]*scaleUp/1000 
  AgentPayout[which(validConfig$if_CPC==1)] <- VCGpayout[which(validConfig$if_CPC==1)]*validConfig$AdCTR[which(validConfig$if_CPC==1)]*scaleUp
  AgentPayout <- AgentPayout[!is.na(AgentPayout)]
  
  BidType[which(validConfig$if_CPC==1)] <- "CPC"
  BidType[which(validConfig$if_CPM==1)] <- "CPM"
  
  result <-list(config = cbind(AgentID=validConfig$AgentID[-(nslots+1)],AgentType=as.character(validConfig$AgentType[-(nslots+1)]),AuctionBid = validConfig$AuctionBid[-(nslots+1)]*scaleUp/1000,VCGpayout,BidType=BidType[-(nslots+1)],AdCTR=validConfig$AdCTR[-(nslots+1)],AgentPayout),Total = sum(AgentPayout))
  
  return(result)
}

#Netego and Premium ads
#----------------------
getPremiumPrice <- function(x,slotValue,bestPremiumAgents,bestNetEgoAgents)
{
  AgentPayout = NULL
  BidType = NULL
  
  premSlotValue = slotValue[1:(x+1)]
  
  validConfig <- rbind(bestNetEgoAgents[0:x,],bestPremiumAgents[1:2,])
  validConfig <- validConfig[!is.na(validConfig$AuctionBid),]
  
  diffSlot <- c(-diff(premSlotValue),premSlotValue[x+1])
  product <- validConfig$AuctionBid[-1] * diffSlot
  
  VCGpayout <-  rev(cumsum(rev(product)))        
  
  validConfig <- validConfig[1:(x+1),]
  
  AgentPayout[which(validConfig$if_CPM==1)] <- VCGpayout[which(validConfig$if_CPM==1)]*scaleUp/1000 
  AgentPayout[which(validConfig$if_CPC==1)] <- VCGpayout[which(validConfig$if_CPC==1)]*validConfig$AdCTR[which(validConfig$if_CPC==1)]*scaleUp
  AgentPayout <- AgentPayout[!is.na(AgentPayout)]
  
  BidType[which(validConfig$if_CPC==1)] <- "CPC"
  BidType[which(validConfig$if_CPM==1)] <- "CPM"
  
  result <-list(config = cbind(AgentID=validConfig$AgentID,AgentType=as.character(validConfig$AgentType),AuctionBid = validConfig$AuctionBid*scaleUp/1000,VCGpayout=VCGpayout,BidType=BidType,AdCTR=validConfig$AdCTR,AgentPayout),Total = sum(AgentPayout))  
}

runVCG(fbDataset,slotValue,scaleUp)