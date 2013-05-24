
##-----------------------------------------------------------------
## Project              : Incremental Revenue Impact Measurement
## Analytic Technique   : Agent Based Simulation
## Client               : Facebook 
## Start Date           : April 2nd 
## File Type            : Function Library
## Version              : 5.1
##-----------------------------------------------------------------

##-----------------------------------------------------------------
## Clear Memory and Load required libraries
##-----------------------------------------------------------------

library(sqldf)
library(plyr)

##-----------------------------------------------------------------
## Set working directory
##-----------------------------------------------------------------

WorkingDir <- function(path)  {
  
  setwd(path)
  
}

##--------------------------------------------------------------------------------------
#Function to modify the bid to CPM for participating in auction

#   CPC Bids are converted to equivalent CPM by using AdCTR
#   All bids after being converted to CPM are paced

#   y[1] is Check for the CPC flag
#   y[2] is CPC Bid
#   y[3] is AdCTR
#   y[4] is CPM bid
#   y[5] is Pacing Parameter
##---------------------------------------------------------------------------------------

BidModify <- function(y){
  if(y[1] == 1){
    
    z <- 10* y[2] * y[3] * y[5]
  }
  else{
    z <- y[4] * y[5]
  }
  return(z)
}



##-----------------------------------------------------------------
## Function to execute an auction and load the agents
##-----------------------------------------------------------------

LoadAgents <- function(AuctionSets, AgentProfile, Period)  {
  
  dateCol <<- which(colnames(AuctionSets) == Period) # Date needs to be given "d0102" is read as 1st Feb.
  
  Auction <<- sample(AuctionSets$TargetingID, prob = AuctionSets[,dateCol], 1)
      
      
  ASAgents <<- subset(AgentProfile, TargetingID == Auction | TargetingID == 0)
  
  ##------------------------------------------Types of Agents----------------------------------------------------------------
#   
#   Marketplace CPC
#   Marketplace CPM
#   Premium CPC
#   Premium CPM
#   Premium MOO
#   NetEGO CPM
#   SponsoredStories CPM
#   
  ##-------------------------------------------------------------------------------------------------------------------------
  
    
  ASAgents <<- ddply(ASAgents,.(if_CPC,Bid_CPC,Bid_CPM,AdCTR,PacingParameter),transform, AuctionBid = BidModify(c(if_CPC,Bid_CPC,AdCTR,Bid_CPM,PacingParameter)))
  s_ASAgents <- ASAgents[with(ASAgents, order(-AuctionBid)), ]
  s_ASAgents <- data.frame(AgentID = s_ASAgents$AgentID, AgentType = s_ASAgents$BidderType, AuctionBid = s_ASAgents$AuctionBid, if_CPC = s_ASAgents$if_CPC, if_CPM = s_ASAgents$if_CPM, AdCTR = s_ASAgents$AdCTR)
  return(s_ASAgents)
  
}

##-----------------------------------------------------------------
## Function to execute the VCG if Agents are more than slots
##-----------------------------------------------------------------

AgentCountMoreThanSlots <- function(s_ASAgents, slotvalue) {
  
  
  z <- data.frame(
    
    AgentID = c(5),
    VCGPayout = c(1.5),
    BidType = c("CPC"),
    AgentPayout = c(15),
    Impressions = c(1),
    Clicks = c(1)
    
    )
  
  
  
  return (z)

  }


##---------------------------------------------------------------------
## The Function executes the following tasks (Layer 1 Implementation)
## 1) Generate a Auction
## 2) Identify agents to participate in a auction Set
## 3) Execute VCG auction
## 4) Update result tables - Rev per iteration, Agent level revenue, 
##    Utility and Overall Revenue
##---------------------------------------------------------------------



runSimulation <- function(newpath, slotvalue, agentFile, auctionFile, YearlyPV,  UpdateFreq, BidUpdate, Indate)  {
  
  ##-----------------------------------------------------------------
  ## Read AuctionSets and Agents profile
  ##-----------------------------------------------------------------
  
  WorkingDir(newpath)
  
  AuctionSets <<- read.csv(file = auctionFile , header = TRUE)
  
  AgentProfile <<- read.csv(file = agentFile , header = TRUE)
  
  YearlyPageViews <<- read.csv(file = YearlyPV, header = TRUE)
  
  slots <<- length(slotvalue)
  
  nSims <<- YearlyPageViews$ScaledPV[which(YearlyPageViews$DateCode == rundate)]
  
  
  
  ##------------------------------------------------------------------
  ## Initialize the graph windows
  ##------------------------------------------------------------------
  
   windows(width = 7, height = 3, xpos = -5, ypos = 5)
  
  ##----------------------------------------------------------------------
  ## Initializing Frame to collect revenue per iteration later in the loop
  ##----------------------------------------------------------------------
  
  Tick_No <- NULL
  Auction_Revenue <-  NULL
  Overall_R <- NULL
  AuctionR <<- data.frame(Tick = Tick_No, Revenue = Auction_Revenue, CumulativeRevenue = Overall_R)
  
  
  ##-----------------------------------------------------------------
  ## Start of the loop
  ##-----------------------------------------------------------------
  
  
  Overall_R <- 0
  
  for(i in 1:10) {
    
  ##-----------------------------------------------------------------
  ## Sampling a auction and loading participating agents
  ##-----------------------------------------------------------------
        
    s_ASAgents <<- LoadAgents(AuctionSets, AgentProfile, Indate)
    
    
  ##-----------------------------------------------------------------
  ## Examining all legal configurations, validating them, and executing the maximum value auction
  ##-----------------------------------------------------------------
    
    
    pay.out <- AgentCountMoreThanSlots(s_ASAgents, slotvalue)
      
    
    
    
    ##-----------------------------------------------------------------
    ## Discount Pacing - Updating Pacing Parameter based on budgets and spend
    ##-----------------------------------------------------------------
    
    
    
    df <- data.frame(AgentID = pay.out$AgentID, Spend = pay.out$AgentPayout)  
    
    
    AgentProfile <- merge(AgentProfile, df, by.x = "AgentID", by.y ="AgentID", all.x = TRUE)
    AgentProfile[is.na(AgentProfile)] <- 0
    AgentProfile$RemBudget <- AgentProfile$Budget - AgentProfile$Spend
    AgentProfile$PacingParameter <- AgentProfile$RemBudget/100 
    
    
    ##-----------------------------------------------------------------
    ## Initializing and updating the data frame for storing the 
    ## revenue per iteration
    ##-----------------------------------------------------------------
    
    
    Tick_No <- i
    Auction_Revenue <-  sum(pay.out$payout) 
    
    Overall_R <- Overall_R + Auction_Revenue
    
    tmp_row <- data.frame(Tick = Tick_No, Revenue = Auction_Revenue, CumulativeRevenue = Overall_R)
      
    AuctionR <<- rbind(AuctionR, tmp_row)

    ##-----------------------------------------------------------------
    ## Update graph with Auction Revenue
    ##-----------------------------------------------------------------
    
      dev.set(2)
    if (i <= 100) {
      plot(x = AuctionR$Tick, y = AuctionR$Revenue, col = "red", type = "l", lwd = 1, xlab = "Tick", ylab = "Revenue/Auction")
    } else {
      st <- i - 100
      plot(x = AuctionR$Tick[st:i], y = AuctionR$Revenue[st:i], col = "red", type = "l", lwd = 1,  xlab = "Tick", ylab = "Revenue/Auction")
    }   

      
    
    ##-----------------------------------------------------------------
    ## Update the Overall revenue (Cumulative of revenue per iteration)
    ##-----------------------------------------------------------------
    
    
      
    Impressions <- i * 7
    eCPM <- Overall_R *1000/ Impressions
    Overall <<- data.frame(Overall_R, Impressions, eCPM)

    ##-----------------------------------------------------------------
    ## Update the Agent level revenue
    ## Initialize the data frame if i = 1, else append the dataframe
    ## with Agent level revenues
    ##-----------------------------------------------------------------
        
    if (i == 1) {
      
      AgentsR <<- data.frame(pay.out$AgentP)
      
#       
    } else {
      
#       
      AgentsR <<- rbind(AgentsR, pay.out$AgentP)
      
  ##-----------------------------------------------------------------
  ## Roll up the revenue and impressions at an agent level
  ##-----------------------------------------------------------------
      
      
      if((i %% UpdateFreq) == 0) {
        
        
        # AgentsR <-  aggregate(ValueLost~AgentId, sum, data = AgentsR)
        AgentsR <<- sqldf("select AgentId, sum(ValueLost) as ValueLost, sum(Imps) as Imps from AgentsR group by AgentId")
        
      }      
    }
    
  ##-----------------------------------------------------------------
  ## Response Model - 
  ## Update Utility in agent profile based on utility  and roll up 
  ## agent revenue
  ##-----------------------------------------------------------------
    
    if((i %% BidUpdate) == 0)    {
      
      AgentsR <<- sqldf("select AgentId, sum(ValueLost) as ValueLost, sum(Imps) as Imps from AgentsR group by AgentId")
      
      AgentProfile <<- sqldf("select AgentProfile.AgentID, AgentProfile.TargetingID, AgentProfile.Alpha,
                             AgentProfile.Beta, AgentProfile.Bid, AgentProfile.Utility, AgentProfile.Budget,
                             AgentProfile.if_CPM, AgentProfile.if_CPC, AgentProfile.Imps_Goal, 
                            AgentProfile.Click_Goal, AgentProfile.Camp_len, coalesce(AgentsR.ValueLost,0) as Spend, 
                             coalesce(AgentsR.Imps,0) as Delv_Imps from AgentProfile left join AgentsR on AgentProfile.AgentID = AgentsR.AgentId")
           
      AgentProfile$Utility <<- (AgentProfile$Imps_Goal - AgentProfile$Delv_Imps)/100
       
#   AgentProfile$AuctionBid <<- AgentProfile$AuctionBid + AgentProfile$Utility
    }
  }
  
  ##-----------------------------------------------------------------
  ## Agent Revenue
  ##-----------------------------------------------------------------
  
    z <- list(Revenue_Per_Iteration = AuctionR, Overall_Revenue = Overall_R, agentlevel = AgentsR , Overall)
  
  return(z)
}

