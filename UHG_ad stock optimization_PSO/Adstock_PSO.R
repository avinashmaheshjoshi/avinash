
#---------------------------------------------------------------------------------------#
#--                                                                                   --#
#--  Project Name: UHG Adstock                                                        --# 
#--  Task        : To optimize MAPE for the dataset for different Adstocks of Variables-#
#--                using PSO                                          --#
#--  version     : 1.0 date :08/02/12 author : Avinash                     			      --#
#--  SVN Directory: http://xp-dev.com/svn/MxO/Labs/code/Avinash/UHG_ad stock optimization_PSO--#
#---------------------------------------------------------------------------------------#

#library('jmsLogger')

#Var to hold the dataset
saDataSet <- NULL

#Function for generating a matrix of indices of the variables having adstocks
generateIndex = function(adSep)
{  
  #Initializing required variables
  adIndex=NULL 
  rowLn=NULL
  colnam=colnames(saDataSet)
  nVar=length(colnam)  
  
  #To find out the maximum number of adstocks in the dataset
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
      diff=(maxNoAdvar - length(c(k,grep(baseVar,colnam))))  
      adIndex=rbind(adIndex,c(k,(grep(baseVar,colnam)),rep(0,diff)))
    }
  }
  
  return(adIndex)
}

#----------------------------------------------
#Objective function to calculate the mape 
#----------------------------------------------

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

#-----------------------------------------
# Function for Particle swarm optimization
#-----------------------------------------  
  
AdStock.PSO = function(uid,dataset,adSep,Vars_no_Adstocks,depVar,useJMS,energyFnc=NULL)
{

  #PSO for finding a minimum fitness
	#params
  saDataSet <<- dataset
	noParticles <- 200	
	omega <- 0.1
	phiP <- 0.1
	phiG <- 0.1
	maxIterations <- 1e+2
	fitnessMin <- -1e+3
	iterationIndicator <- 10
  convCount = 30
  tolerance = 10^-3
  
	if(useJMS)
  {
	  logger<-jmsLogger::InitializeLogger("tcp://162.192.100.10:61616","T","MxOProgress")
	}
	progFile <- file.path(getwd(),"progressPSO.txt")
	
	boundsMatrix <- generateIndex(adSep)
  solDimensions <- nrow(boundsMatrix)
  
	#Create position matrix
	posMatrix <- array(0, dim=c(noParticles,solDimensions))
	
  #Create velocity matrix
	velMatrix <- array(0, dim=c(noParticles,solDimensions))

	#Initialize position matrix
	for(i in 1:noParticles)
	{
			posMatrix[i,] <- apply(boundsMatrix,1,function(x) sample(x,1))
      while ( prod(posMatrix[i,]) == 0)
      {
        posMatrix[i,] <- apply(boundsMatrix,1,function(x) sample(x,1))
      }
	}
	
  #Initialize the particle's best known position to its initial position
	bestIndPosMatrix <- posMatrix
	
  #update the swarm's best known position
	swarmBestFitness <- 100
  swarmBestPos <- bestIndPosMatrix[1,]
   
	for(particle in 1:noParticles)
  {
		bestVars = colnames(saDataSet[bestIndPosMatrix[particle,]])
    temp <- energyFnc(Vars_no_Adstocks,depVar,bestVars)
		if(temp < swarmBestFitness)
    {
			swarmBestFitness <- temp
			swarmBestPos <- bestIndPosMatrix[particle,]
      swarmBestVars <- bestVars
		}
	}

  #Initialize velocity matrix
	for(i in 1:noParticles)
	{
		for(j in 1:solDimensions)
    {
			range <- abs(boundsMatrix[j,2]-boundsMatrix[j,1])
			velMatrix[i,j] <- runif(1,min=-range,max=range)
		}
	}
    
	#Run till termination criteria is met
	iters <- 1
  ePrev = 0
  counter = 0
  
  while(iters < maxIterations && swarmBestFitness > fitnessMin && counter <= convCount)
  {
		for(particle in 1:noParticles)
    {
			rP <- runif(1,min=0,max=1)
			rG <- runif(1,min=0,max=1)
      
			#Update particle velocity
			locDiff <- bestIndPosMatrix[particle,]-posMatrix[particle,]
			globDiff <- swarmBestPos - posMatrix[particle,]
			velMatrix[particle,] <- omega*velMatrix[particle,]+(phiP*rP*locDiff)+(phiG*rG*globDiff)
			
      #Update particle position
			posMatrix[particle,] <- round(posMatrix[particle,]+ velMatrix[particle,])
  
      #Checks that the columns of the new position matrix generated, has values that are in the 
      #corresponding rows of the matrix. The internal is.element returns a vector of booleans comparing
      #jth column with the jth row of the bounds matrix. The external is.element checks if the internal
      # is.element fails for even one column.
      while (prod(posMatrix[particle,] == 0) || (is.element ("FALSE",(for(j in 1:nrow(boundsMatrix)) 
                                                                      {
                                                                      (is.element(posMatrix[particle,j],boundsMatrix[j,])) 
                                                                      })))
             )
      {
      rP <- runif(1,min=0,max=1)
  		rG <- runif(1,min=0,max=1)
                                    
			#Update particle velocity
			locDiff <- bestIndPosMatrix[particle,]-posMatrix[particle,]
			globDiff <- swarmBestPos - posMatrix[particle,]
			velMatrix[particle,] <- omega*velMatrix[particle,]+(phiP*rP*locDiff)+(phiG*rG*globDiff)
			
      #Update particle position
			posMatrix[particle,] <- round(posMatrix[particle,]+ velMatrix[particle,])
      }
       
      currentVars = colnames(saDataSet[posMatrix[particle,]])
      bestRowVars = colnames(saDataSet[bestIndPosMatrix[particle,]])
                                  
      #Update local and swarm best
			if(energyFnc(Vars_no_Adstocks,depVar,currentVars) < energyFnc(Vars_no_Adstocks,depVar,bestRowVars))
      {
				bestIndPosMatrix[particle,] <- posMatrix[particle,] 
				bestRowVars = colnames(saDataSet[bestIndPosMatrix[particle,]])
        newFitness <- energyFnc(Vars_no_Adstocks,depVar,bestVars)
				if(newFitness < swarmBestFitness )
        {
					swarmBestPos <- bestIndPosMatrix[particle,]
					swarmBestFitness <- newFitness
				}
			}
		}
    
    if(abs(swarmBestFitness-ePrev) <= tolerance)
    {
      counter = counter + 1
    }
    
    else 
    {
      ePrev = swarmBestFitness
      counter = 0
    }
    
		if((iters >= iterationIndicator) && (iters%%iterationIndicator == 0))
    {
			write(paste('\n Iterations:',iters,';Min:',swarmBestFitness,'; ','Params:[',toString(swarmBestPos),']; ','Finished:0 '), file = progFile,append = TRUE)
			if(useJMS)
      {
				jmsLogger::Log(logger, paste('UID:',uid,';Algo:PSO;Iterations:',iters,';Min:',swarmBestFitness,';Finished:0'))
			}
		}
		iters <- iters+1
	}

	write(paste('\n Iterations:',iters,';Min:',swarmBestFitness,'; ','Params:[',toString(swarmBestPos),']; ','Finished:1 '), file = progFile,append = TRUE)
	if(useJMS)
  {
		jmsLogger::Log(logger, paste('UID:',uid,';Algo:PSO;Iterations:',iters,';Min:',swarmBestFitness,';Finished:1'))
		jmsLogger::DestroyLogger(logger)
	}
	result <- list("par"=swarmBestVars,"value"=swarmBestFitness, "time"=NULL, "iterations"=iters,"error"=0,"errorMsg"="")
	return(result)
}