empty <- vector('numeric', 0)
den <- 5000

#==================
#Functions for SA =
#==================
defaultEnergy <- function(n) {
	return(sum(n))
}
#Generates a random neighbour in the range by default
defaultNeighbour <- function(config,nParams,lowerBounds=NULL,upperBounds=NULL){
	epsilonNhood <- NULL
	for(i in 1:nParams){
		bound <- (upperBounds[i]-lowerBounds[i])/den
		#epsilonNhood <- c(epsilonNhood,runif(1,min=-bound,max=bound))
		epsilonNhood <- c(epsilonNhood,runif(1,min=lowerBounds[i],max=upperBounds[i]))
	}
	config <- config + epsilonNhood
	return(config)
}
#====End of SA functions===

#=====================
#Functions for my GA =
#=====================
InitializePopulation <- function(populationSize,nGenes){
	#Generating population with random genes
	population <- array(0,dim=c(populationSize,nGenes))
	for(i in 1:populationSize){
		population[i,] <- sample(0:1,nGenes,replace=T)
	}
	return(population)
}

DecodeChromosome <- function(nVars,population,i,lowerBounds,upperBounds,nGenes){
	parameterValues <- rep(0,nVars)
	chromosome <- population[i,]
	for (i in 1:nVars){
    		parameterValue <- 0
    		for (j in 1:(nGenes/nVars)){
        		parameterValue <- parameterValue + ((2)^(-1*j)*chromosome[((i-1)*(nGenes/nVars))+j])
    			# Decoding the chromosome above
    		}
      #parameterValues[i]=(-1*range)+((2*range)/(1-(2)^((-1*nGenes)/nVars)))*parameterValue
     	 parameterValues[i]=(lowerBounds[i])+((upperBounds[i]-lowerBounds[i])/(1-(2)^((-1*nGenes)/nVars)))*parameterValue
	}
     return(parameterValues)
}


TournamentSelect <- function(fitness,populationSize,tournamentProbability){
	#generating a number from 1 to populationSize
	individual1 <- ceiling(populationSize*runif(1))
	fitness1 <- fitness[individual1]
	individual2 <- ceiling(populationSize*runif(1))
	fitness2 <- fitness[individual1]
	if(fitness1 < fitness2){
    		individual <- individual2
	}
	else{
    		individual <- individual1
    		individual1 <- individual2
	}
	#The variable individual1 is always the weakest
	r <- runif(1)
	if(r > tournamentProbability){
    		individual <- individual1
	}
	return(individual)
}

Cross <- function(population,i1,i2,nGenes){
	#Randomly generating the crossover point
	min <- 2
	max <- nGenes-1
	crossoverPoint <- ceiling(min +((max-min)*runif(1)))
	individual1 <- population[i1,];
	individual2 <- population[i2,];
	tempIndividual <- individual1;
	individual1[(crossoverPoint+1):length(individual1)] <- individual2[(crossoverPoint+1):length(individual1)] 
	individual2[(crossoverPoint+1):length(individual2)] <- tempIndividual[(crossoverPoint+1):length(tempIndividual)]
	newIndividual <- array(0,dim=c(2,length(individual1)))
	newIndividual[1,] <- individual1;
	newIndividual[2,] <- individual2;
	return(newIndividual)
}

mutate <- function(chromosome,mutationProbability,nGenes){
	for (i in 1:nGenes){
    		r <- runif(1)
    		if(r < mutationProbability){
        	#flip the bit
        		if(chromosome[i] == 0){
            		chromosome[i] <- 1
			}
        		else{
            		chromosome[i] <- 0
			}
        	}
	}
	individual <- chromosome
	return(individual)
}

#Test function for the GA min is at (0,-1)
EvaluateIndividual <- function(parameterValues){
	x1 <- parameterValues[1]
	x2 <- parameterValues[2]
	#x1=0.000;
	#x2=-1;
	#breaking up the objective function for simplicity
	exp1 <- (x1+x2+1)^2;
	exp2 <- (19-(14*x1)+(3*(x1^2))-(14*x2)+(6*x1*x2)+(3*(x2^2)));
	exp3 <- ((2*x1)-(3*x2))^2;
	exp4 <- (18-(32*x1)+(12*(x1)^2)+(48*x2)-(36*x1*x2)+(27*(x2)^2));
	functionVal1 <- 1+(exp1*exp2);
	functionVal2 <- 30+(exp3*exp4);
	functionVal <- functionVal1*functionVal2;
	return(functionVal)
}

#===== End of my GA functions===================

#=========Main Simulated annealing algorithm====

SA <- function(uid,nParams,lowerBounds,upperBounds,useJMS,energyFnc=defaultEnergy,neighFnc=defaultNeighbour) {
	if(useJMS){
		logger<-InitializeLogger("tcp://localhost:61616","T","MxOProgress")
	}	
	cooling_sched <- 0.995
	maxIters <- 1e+15
	stopTemp <- 1e-150
	eMin <- -1e100
	iterationIndicator <- 1000
	progFile <- file.path(getwd(),"progressSA.txt")
	xVec <- rep(0,nParams)
	eBest <- energyFnc(xVec)
	e <- eBest;
	k=1;
	T=1;
	pTime <- proc.time()
	initTime <- proc.time()
	write(NULL,file = progFile,append = FALSE)
	result <- NULL
	while (k < maxIters && e >= eMin && T > stopTemp){
		newX <- neighFnc(xVec,nParams,lowerBounds,upperBounds)
		eNew <- energyFnc(newX);
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
            
      	#Put code to write progress to DB
		if(k >= iterationIndicator && (k%%iterationIndicator == 0)){
			tTaken <- unlist(strsplit(toString(proc.time()-pTime), ","))[3]
			pTime <- proc.time()
			write(paste('\n Iterations:',k,';Min:',eBest,'; ','Params:[',toString(xVec),']; ','Finished:0 ;','Time taken(seconds) :',tTaken), file = progFile,append = TRUE)
			if(useJMS){
				Log(logger, paste('UID:',uid,';Algo:SA;Iterations:',k,';Min:',eBest,';Finished:0'))
			}
	      }

      	k <- k+1
      	T <- cooling_sched*T
	}
	result <- list("par"=xVec,"value"=eBest, "time"=unlist(strsplit(toString(proc.time()-initTime), ","))[3], "iterations"=k,"temp"=T,"error"=0,"errorMsg"="")
	write(paste('\n Iterations:',k,';Min:',eBest,'; ','Params:[',toString(xVec),']; ','Finished:1;','Time taken(seconds) :',result$time), file = progFile,append = TRUE)
	if(useJMS){
		Log(logger, paste('UID:',uid,';Algo:SA;Iterations:',k,';Min:',eBest,';Finished:1'))
		destroyResult<-DestroyLogger(logger)
	}
	return(result)	
}

#======End of Simulated annealing====================

#=====Start of call to rgenoud=======================
GA <- function(uid,nParams,lowerBounds,upperBounds,useJMS,energyFnc=NULL){
	progFile <- file.path(getwd(),"progressGA.txt")
    	result <- genoud(energyFnc,nvars=nParams, max=FALSE, pop.size=1000, max.generations=100, wait.generations=10,
              hard.generation.limit=TRUE,solution.tolerance=0.0001,gr=NULL,print.level=2, project.path=progFile)
	write(paste('Generation:',result$generations,';Min:',result$value,'; ','Params:[',toString(result$par),']; ','Finished:1'), file = progFile,append = TRUE)
	garesult <- list("par"=result$par,"value"=result$value, "time"="", "iterations"=result$generations,"error"=0,"errorMsg"="")
	return(garesult)
}
#======End of call to rgenoud==========================

#=====Start of my implementation of GA=================
myGA <- function(uid,nVars,lowerBounds,upperBounds,useJMS,energyFnc=NULL){
	#Put these parameters in a config file
	#params <- read.table("configGA.txt",header=TRUE)
	if(useJMS){
		logger<-InitializeLogger("tcp://localhost:61616","T","MxOProgress")
	}
	progFile <- file.path(getwd(),"progressmyGA.txt")
	write(NULL,file = progFile,append = FALSE)
	populationSize <- 100 # Even population size assumed in this program.
	nGenes <- 300
	crossoverProbability <- 0.25
	mutationProbability <- 0.01
	tournamentProbability <- 0.75
	#nVars <- 2
	maxGenerations <- 300
	nEvaluationsPerformed <- 0
	maxGenLimit <- 100
	tolerance <- 1e-20

	#populationSize <- params[1,8]
	#nGenes <- params[1,9]
	#crossoverProbability <- params[1,2]
	#mutationProbability <- params[1,1]
	#tournamentProbability <- params[1,7]
	#nVars <- params[1,3]
	#maxGenerations <- params[1,4]
	#maxGenLimit <- params[1,5]
	#tolerance <- params[1,6]
	
	genLimit <- 0
	population <- InitializePopulation(populationSize,nGenes)
	prevFitness <- 0
	pTime <- proc.time()

	for (generation in 1:maxGenerations){
  		maxFitness <- 1e+50
		fitness <- rep(0,populationSize)
  		for (i in 1:populationSize){
    			parameterValues <- DecodeChromosome(nVars,population,i,lowerBounds,upperBounds,nGenes)
    			# Note! Minimization:
    			fitness[i] <- energyFnc(parameterValues)
    			nEvaluationsPerformed <- nEvaluationsPerformed+1
			if (fitness[i] < maxFitness){
       			maxFitness <- fitness[i]
       			iBestIndividual <- i
       			bestParameterValues <- parameterValues
				write(paste('\n Generation: ',generation,';Min:',maxFitness,' ',';Params:[',toString(bestParameterValues),'] ',';Finished :0' ), file = progFile,append = TRUE)
				if(useJMS){
					Log(logger, paste('UID:',uid,';Algo:myGA;Iterations:',generation,';Min:',maxFitness,';Finished:0'))
				}
    		}
  		}
  	if((maxFitness - prevFitness) <= tolerance){
		#print(paste(generation,maxFitness,prevFitness))
		genLimit <- genLimit+1
		#print(paste(genLimit,maxFitness-prevFitness))
		if(genLimit >= maxGenLimit){
			break
		}
  	}
  	else{
 		prevFitness <- maxFitness
		genLimit <- 0
 	}
	
  	temporaryPopulation <- population  
  	for (i in seq(1,populationSize,by=2)){
    		i1 <- TournamentSelect(fitness,populationSize,tournamentProbability)
    		i2 <- TournamentSelect(fitness,populationSize,tournamentProbability)
    		r <- runif(1)
    	 	if (r < crossoverProbability){ 
      		newIndividual <- Cross(population,i1,i2,nGenes)
      		temporaryPopulation[i,] <- newIndividual[1,]
      		temporaryPopulation[i+1,] <- newIndividual[2,]
    		}
    		else{
      		temporaryPopulation[i,] <- population[i1,]
      		temporaryPopulation[i+1,] <- population[i2,]
    		}
  	}
  	temporaryPopulation[1,] <- population[iBestIndividual,]
  	for( i in 2:populationSize){
    		tempIndividual <- mutate(temporaryPopulation[i,],mutationProbability,nGenes)
    		temporaryPopulation[i,] <- tempIndividual
  	}
  	population <- temporaryPopulation
  	#print(paste(generation,maxFitness,bestParameterValues[1],bestParameterValues[2]))
	}
	#print(paste(generation,-maxFitness,bestParameterValues[1],bestParameterValues[2]))
	tTaken <- unlist(strsplit(toString(proc.time()-pTime), ","))[3]
	pTime <- proc.time()
	write(paste('\n Generation:',generation,';Min:',maxFitness,' ',';Params:[',toString(bestParameterValues),'] ',';Finished:1'), file = progFile,append = TRUE)
	if(useJMS){
		Log(logger, paste('UID:',uid,';Algo:myGA;Iterations:',generation,';Min:',maxFitness,';Finished:1'))
		destroyResult<-DestroyLogger(logger)
	}
	result <- list("par"=bestParameterValues,"value"=maxFitness, "time"=tTaken, "generations"=generation,"error"=0,"errorMsg"="")
	return(result)
}
#=====End of my implementation of GA=================
