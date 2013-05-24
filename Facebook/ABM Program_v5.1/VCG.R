runVCG <- function(dataset,slotValue,scaleUp)
{
  BidType <- NULL
  
  nslots <<- length(slotValue)
  
  premiumAgents <- dataset[which(dataset$AgentType=="Premium"),]
  
  netEgoAgents <- dataset[which(dataset$AgentType=="NetEgo"),]
  
  marketPlaceAgents <- dataset[which(dataset$AgentType=="MarketPlace"),]
  
  premiumAgents <- premiumAgents[sort(premiumAgents$AuctionBid,decreasing=TRUE,index = TRUE)$ix,]  
  
  bestPremiumAgents <- premiumAgents[1:2,] 
  
  bestPremiumAgents <- bestPremiumAgents[!is.na(bestPremiumAgents),] 
  
  netEgoAgents <- netEgoAgents[sort(netEgoAgents$AuctionBid,decreasing=TRUE,index = TRUE)$ix,]
  
  bestNetEgoAgents <- netEgoAgents[1:(nslots+1),]
  
  bestNetEgoAgents <- bestNetEgoAgents[!is.na(bestNetEgoAgents$AuctionBid),]
  
  marketPlaceAgents <- marketPlaceAgents[sort(marketPlaceAgents$AuctionBid,decreasing=TRUE,index = TRUE)$ix,]
  
  bestMarketPlaceAgents <- marketPlaceAgents[1:(nslots+1),]
  
  bestMarketPlaceAgents <- bestMarketPlaceAgents[!is.na(bestMarketPlaceAgents$AuctionBid),]
    
  indexVector <- nrow(bestNetEgoAgents):0
  
  getPrice <- function(x,slotValue,bestNetEgoAgents,bestMarketPlaceAgents)
  { 
    AgentPayout = NULL
  
    slotValue = slotValue[1:max(nrow(bestNetEgoAgents),nrow(bestMarketPlaceAgents))]
    slotValue = slotValue[!is.na(slotValue)]
    
    validConfig <- rbind(bestNetEgoAgents[0:x,],bestMarketPlaceAgents[0:(nslots+1-x),])
    validConfig <- validConfig[!is.na(validConfig$AuctionBid),]
    while (nrow(validConfig) < nslots+1)
    {
      validConfig <- rbind(validConfig,0)
    }
   
    diffSlot <- c(-diff(slotValue),slotValue[nslots])
    product <- validConfig$AuctionBid[-1] * diffSlot
    
    VCGpayout <-  rev(cumsum(rev(product)))
    
    AgentPayout[which(validConfig$if_CPM==1)] <- VCGpayout[which(validConfig$if_CPM==1)]*scaleUp/1000 
    AgentPayout[which(validConfig$if_CPC==1)] <- VCGpayout[which(validConfig$if_CPC==1)]*validConfig$AdCTR[which(validConfig$if_CPC==1)]*scaleUp
    AgentPayout <- AgentPayout[!is.na(AgentPayout)]
    
    BidType[which(validConfig$if_CPC==1)] <- "CPC"
    BidType[which(validConfig$if_CPM==1)] <- "CPM"
    
    result <-list(config = cbind(AgentId=validConfig$AgentId[-(nslots+1)],AgentType=as.character(validConfig$AgentType[-(nslots+1)]),VCGpayout,BidType=BidType[-(nslots+1)],AdCTR=validConfig$AdCTR[-(nslots+1)],AgentPayout),Total = sum(AgentPayout))
      
    return(result)
  }
  
  configList <- lapply(indexVector,getPrice,slotValue,bestNetEgoAgents,bestMarketPlaceAgents)
  
  premIndexVector <- 0:2
  
  getPremiumPrice <- function(x,slotValue,bestPremiumAgents,bestNetEgoAgents)
  {
    AgentPayout = NULL
    
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
    
    result <-list(config = cbind(AgentId=validConfig$AgentId,AgentType=as.character(validConfig$AgentType),VCGpayout=VCGpayout,BidType=BidType,AdCTR=validConfig$AdCTR,AgentPayout),Total = sum(AgentPayout))
  }
  
  premConfigs <- lapply(premIndexVector,getPremiumPrice,slotValue,bestPremiumAgents,bestNetEgoAgents)
  
  configList <- c(configList,premConfigs)
  
  winningConfig <- configList[which(max(unlist(lapply(configList,function(x)x$Total))) == unlist(lapply(configList,function(x)x$Total)))]
  
  bestConfig <- as.data.frame(winningConfig[[1]]$config)
  
  auctionResult <- cbind(bestConfig,Impressions = rep(scaleUp,nrow(bestConfig)),clicks = scaleUp*as.numeric(bestConfig$AdCTR))
  
  return(auctionResult)
}

#agentData <- read.csv('agentData.csv')
#slotValue <- c(1,0.8,0.7,0.5,0.4,0.2,0.1)
#fbDataset <- read.csv('FacebookABM.csv')

win = runVCG(agentData,slotValue,10000)

runVCG(fbDataset,slotValue,10000)