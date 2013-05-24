

x <<- subset(AgentProfile, TargetingID == 0 | TargetingID == Auction)

head(ASAgents)

View(x)

x$Bid <- NULL
view(x)


if(x$if_CPC == 1) {
  x$Bid <- 10* (x$Bid_CPC) * (x$AdCTR) 
}
else {
  
  x$Bid <- x$Bid_CPM
}
  
p <- apply(x, x$if_CPC, sort)

View(p)

df<-data.frame(a = (1:10),b = (11:20))


 myFunc <- function(y){
  if(y[1] == 1){
    
    z <- 10* y[2] * y[3]
  }
  else{
    z <- y[4]
  }
  return(z)
}

abc = ddply(x,.(if_CPC,Bid_CPC,Bid_CPM,AdCTR),transform,new = myFunc(c(if_CPC,Bid_CPC,AdCTR,Bid_CPM)))
l <- df[with(df, order(-b)), ]
l <- k[with(k, order(-AuctionBid)), ]

j <- k






## use character columns of names to get sensible sort order
authors <- data.frame(
  surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  deceased = c("yes", rep("no", 4)))
books <- data.frame(
  name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
  title = c("Exploratory Data Analysis",
            "Modern Applied Statistics ...",
            "LISP-STAT",
            "Spatial Statistics", "Stochastic Simulation",
            "Interactive Data Analysis",
            "An Introduction to R"),
  other.author = c(NA, "Ripley", NA, NA, NA, NA,
                   "Venables & Smith"))

(m1 <- merge(authors, books, by.x = "surname", by.y = "name"))
(m2 <- merge(books, authors, by.x = "name", by.y = "surname"))
stopifnot(as.character(m1[,1]) == as.character(m2[,1]),
          all.equal(m1[, -1], m2[, -1][ names(m1)[-1] ]),
          dim(merge(m1, m2, by = integer(0))) == c(36, 10))


FuncOut <- head(s_ASAgents)

FuncOut$Spend <- FuncOut$AuctionBid*15

FuncOutTest <- data.frame(AgentID = FuncOut$AgentID, Spend = FuncOut$Spend)

Profile <- AgentProfile

m1 <- merge(Profile, FuncOutTest, by.x = "AgentID", by.y ="AgentID", all.x = TRUE)
m1[is.na(m1)] <- 0
m1$RemBudget <- m1$Budget - m1$Spend

