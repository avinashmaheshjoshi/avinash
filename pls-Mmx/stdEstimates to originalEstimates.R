#--                                                                                   --#
#--  Project Name:   Sanofi MMx--# 
#--  Task : Converting standardized estimates to normal estimates--# 
#--  version : 1.0 date :13/04/12 author : Avinash Joshi--#
#--  SVN Directory: http://xp-dev.com/svn/MxO/Labs/code/Avinash--#
#---------------------------------------------------------------------------------------#

#This code converts estimates calculated from centralized and scaled data to the original
#estimates
#Following are the input parameters
#stEstimates - Vector of standardized estimates
#dataset - The dataset used for modelling
#depVar - Dependent variable (should be input as a character)

getOriginalEstimates <- function(stdEstimates,dataset,depVar)
{
  #Finding the dependent variable index
  #------------------------------------
  depVarindex <- which(colnames(dataset) == depVar)
  
  #Finding the column means
  #-----------------------
  meanData <- mean(dataset)
  
  #Finding the standard deviation of columns
  #-----------------------------------------
  sdData <- sd(dataset)
  
  #Isolating mean and standard deviation of dependent variable
  #-----------------------------------------------------------
  depVarMean <- meanData[depVarindex]
  depVarSd <- sdData[depVarindex]
  restMeanData <- meanData[-depVarindex]
  restSdData <- sdData[-depVarindex]
  
  #Isolating the intercept 
  #-----------------------
  stdIntercept <- stdEstimates[1]
  stdEsti <- stdEstimates[-1]
  
  #Converting the standard estimates to original
  #---------------------------------------------
  orgIntercept <- depVarMean + depVarSd*(stdIntercept - sum(stdEsti*restMeanData/restSdData))
  orgEstimates <- stdEsti*depVarSd/restSdData  
  originalEstimates = array(data = c(orgIntercept,orgEstimates))
  row.names(originalEstimates)=row.names(stdEstimates)
  return(originalEstimates)
}

#An example
pub <- read.csv("workpub.csv") #Change this to your directory
rScaledPub <- scale(pub)
cenScaledPubPls = plsr(formula=rev_disc_pub~., data=rScaledPub, ncomp=9,scale=FALSE,method="kernelpls",validation = "none")
estiCenScaledPub = coef(cenScaledPubPls,intercept = TRUE)
originalEstimates = getOriginalEstimates(estiCenScaledPub,pub,"rev_disc_pub")
