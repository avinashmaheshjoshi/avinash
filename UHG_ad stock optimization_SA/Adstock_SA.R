#---------------------------------------------------------------------------------------#
#--                                                                                   --#
#--  Project Name: UHG                                                                --# 
#--  Task        : To optimize MAPE for the dataset for different Adstocks of Variables-#
#--                using simulated annealing                                          --#
#--  version     : 1.1 date :08/02/12 author : Avinash                   				      --#
#--  SVN Directory: http://xp-dev.com/svn/MxO/Labs/code/Avinash/UHG_ad stock optimization_SA--#
#---------------------------------------------------------------------------------------#


#Var to hold the dataset

saDataSet <- NULL

nameDataSet <- NULL

#Function for generating a matrix of indices of the variables having adstocks
generateIndex = function(adSep)
{  
  #Initializing required variables
  adIndex=NULL 
  rowLn=NULL
  colnam=colnames(saDataSet)
  nVar=length(colnam)  
  
  for(l in 1:nVar)
  {
    baseVar=paste(colnam[l],"",sep=adSep)
    rowLn[l]=length(c(l,grep(baseVar,colnam)))
  }  
  
  maxNoAdvar=max(rowLn)
  
  nVarHavingAdstock=length(which(rowLn!=1))
  
  #This loop creates a matrix of indices where each row corresponds to the indices of a variable
  #and its adstocks. The number of columns of the matrix would be the maximum number of adstocks
  #for any variable. For the rows in which variables have lesser adstocks, it will be filled from 
  #the left of the column and "0's" will be added in the end. 
  for(k in 1:nVar)
  {
    baseVar=paste(colnam[k],"",sep=adSep)
    if(length(c(k,grep(baseVar,colnam))) > 1)
    {        
      diff=(maxNoAdvar - length(c(k,grep(baseVar,colnam)))  
      adIndex=rbind(adIndex,c(k,(grep(baseVar,colnam)),rep(0,diff)))
    }
  }
  
  return(adIndex)
}

#function to generate a neighbourhood solution
generateNeighbourhood = function(degreesOfFrdm,masterIndex,firstsol,indexFirstsol)
{

  #creates the vector of indices to  be changed in this iteration in the solution
  #based on degrees of freedom
  chng=sample(indexFirstsol,degreesOfFrdm)
  
  #Subsets the masterIndex to a smaller matrix according to "chng"
  if(degreesOfFrdm == 1)
  {  
    subsetMasterIndex = t(as.matrix(masterIndex[chng,]))
  }
  else    
  {
    subsetMasterIndex = as.matrix(masterIndex[chng,])
  }  
  
  #Replace elements of first solution depending on the vector chng
  firstsol = replace(firstsol,chng,apply(subsetMasterIndex,1,function(x) sample(x,1))) 
  
  #Ensures that 0 is never chosen as a solution
  while(prod(firstsol)==0)
  {
    firstsol = replace(firstsol,chng,apply(subsetMasterIndex,1,function(x) sample(x,1)))  
  }

  #Converts the indices into their corresponding columb names from the dataset
  tempsolution = colnames(saDataSet[firstsol])
  
  #gives you the set of variables to be used in this iteration 
  #in the form "nameDataSet$Variablename"
  solution=paste(nameDataSet,tempsolution,sep="$")
  
  return(solution)
}

#Objective function to calculate the mape 
findMinMape = function(Vars_no_Adstocks,depVar,solution)
{  
  # Get the dependent variable
  depVar = as.character(depVar)
  
  Var_noAdstocks = as.character(Vars_no_Adstocks) 
  
  rhs= paste(c(paste(Var_noAdstocks,collapse=" + "),paste(solution,collapse=" + ")),collapse=" + ")
  
  #creates the formula in the required format for linear model
  form= as.formula(paste(depVar,rhs,sep=" ~ "))
  
  fit = lm(form,data=saDataSet)
  
  predicted = predict(fit)
  
  mape = mean(abs(predicted - saDataSet[,depVar])/saDataSet[,depVar])
  
  return(mape)
}

#=========Main Simulated annealing algorithm===============#

AdStock.SA <- function(dataset,adSep,degreesOfFrdm,Vars_no_Adstocks,depVar,useJMS,energyFnc=findMinMape,neighFnc=generateNeighbourhood) 
{
  #---------------------Initializing variables to be used in the function---------------------------#
  
  masterIndex <- NULL
  
  firstsol <-NULL
  
  indexFirstsol <- NULL
  
  convCount <- 3000
  
  tolerance <- 10^-3
  
  saDataSet <<- dataset
  
  nameDataSet <<- "saDataSet"
  
  if(useJMS)
  {
  	logger<-InitializeLogger("tcp://localhost:61616","T","MxOProgress")
	}
	
  cooling_sched <- 0.995
	
  maxIters <- 1e15
	
  stopTemp <- 1e-150
	
  eMin <- -1e100
	
  iterationIndicator <- 1000

	progFile <- file.path(getwd(),"progressSA.txt")
	
  masterIndex = generateIndex(adSep) #creates a matrix of indices of all the variables and respective adstocks in each row
  
  firstsol = apply(masterIndex,1,function(x) sample(x,1)) #creates first random solution which are indices of the variables to be chosen
  
  indexFirstsol = (1:length(firstsol)) #creates a vector of indices of the firstsol

  xVec <- paste(colnames(saDataSet[firstsol]),sep="$")
  
	eBest <- energyFnc(Vars_no_Adstocks,depVar,xVec)
  
  e <- eBest
  
	k=1
  
	T=1
  
	pTime <- proc.time()
  
	initTime <- proc.time()
  
	write(NULL,file = progFile,append = FALSE)
	
  result <- NULL
  
  ePrev = 0
  
  counter = 0
    
	while (k < maxIters && e >= eMin && T > stopTemp && counter <= convCount )
	{
    
		newX <- neighFnc(degreesOfFrdm,masterIndex,firstsol,indexFirstsol)
		eNew <- energyFnc(Vars_no_Adstocks,depVar,newX);
		if(eNew < eBest)
    { 
    	eBest <- eNew
    	e <- eNew
    	xVec <- newX
		}
    else if(runif(1,min=0,max=1)< exp((e-eNew)/(k*T) ))
    {
      eBest <- eNew
      e <- eNew
		  xVec <- newX
    }

  #Creating a counter for convergence criteria
    if(abs(eBest-ePrev) <= tolerance)
    {
      counter = counter + 1
    }
    
    else 
    {
      ePrev = eBest
      counter = 0
    }
    
    #code to write progress to DB
		if(k >= iterationIndicator && (k%%iterationIndicator == 0))
    {
		  tTaken <- unlist(strsplit(toString(proc.time()-pTime), ","))[3]
			pTime <- proc.time()
			write(paste('\n Iterations:',k,';Min:',eBest,'; ','Params:[',toString(xVec),']; ','Finished:0 ;','Time taken(seconds) :',tTaken), file = progFile,append = TRUE)
			if(useJMS)
			{
			  Log(logger, paste('UID:',uid,';Algo:SA;Iterations:',k,';Min:',eBest,';Finished:0'))
			}
    }
    k <- k+1
    T <- cooling_sched*T
	}
  
	result <- list("par"=xVec,"value"=eBest, "time"=unlist(strsplit(toString(proc.time()-initTime), ","))[3], "iterations"=k,"temp"=T,"error"=0,"errorMsg"="")
	write(paste('\n Iterations:',k,';Min:',eBest,'; ','Params:[',toString(xVec),']; ','Finished:1;','Time taken(seconds) :',result$time), file = progFile,append = TRUE)
	if(useJMS)
  {
		Log(logger, paste('UID:',uid,';Algo:SA;Iterations:',k,';Min:',eBest,';Finished:1'))
		destroyResult<-DestroyLogger(logger)
	}
	return(result)	
}

#======End of Simulated annealing====================#

