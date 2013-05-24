#---------------------------------------------------------------------------------------#
#--                                                                                   --#
#--  Project Name:   Sanofi MMx--# 
#--  Task : Half life calculation--# 
#--  Sub-Task: 1.1 Get the adstock variable with decayed values--#
#--  version : 1.0 date :21/03/12 author : Avinash Joshi--#
#--  SVN Directory: http://xp-dev.com/svn/MxO/Labs/code/Avinash--#
#---------------------------------------------------------------------------------------#

#The inputs for the function are the decay rate (lambda), initial value(initial pulse), and which 
#decay rate equation to use. If Eqtn = 0 then it uses the geometric decay rate equation.
#If you give Eqtn = 1 then it uses exponential decay rate equation. If you give Eqtn = 2
#then it uses logistic decay equation. For this equation, saturation rate needs to be given.
#Next input is for how many timeStamps you want the code to run. And the minimum value after which 
#it should give 0
#The output of this code will be a dataframe with first column(X) being the timestamp and 
#the second column being the decayed values. And also the first timestamp at which the value becomes 
#less than or equal to half of initial value. And also the exact half life of the model.

getHalfLife <- function(lambda,initialPulse,Eqtn,timePeriod,minValue,satPar = NULL)
{
  
  decayValues = data.frame(x=(1:timePeriod),y = rep(initialPulse,timePeriod)) 
  halfLife = 0
  #Uses the geometric decay equation.
  if(Eqtn == 0)
  {
    for (timeStamp in 2:timePeriod)
    {
      decayValues[timeStamp,2] = lambda*decayValues[timeStamp-1,2]
      
      if(decayValues[timeStamp,2] <= minValue)
      {
        decayValues[timeStamp,2] = 0
      }
    }
    #Calculates halflife with the formula
    halfLife = 1-(log(2)/log(lambda))
  }
  #Uses exponential decay equation
  if(Eqtn == 1)
  {
    for (timeStamp in 2:timePeriod)
    {
      decayValues[timeStamp,2] = decayValues[1,2]*exp(-lambda*timeStamp)             
      
      if(decayValues[timeStamp,2] <= minValue)
      {
        decayValues[timeStamp,2] = 0
      }
    }
    #Calculates halflife with the formula
    halfLife = log(2)/lambda
  }
  #Uses logistic decay equation
  if(Eqtn == 2)
  {
    for (timeStamp in 2:timePeriod)
    {
      decayValues[timeStamp,2] = ((1-1/(1+exp(-satPar*timeStamp)))*initialPulse) + (lambda*decayValues[timeStamp-1,2])           
      
      if(decayValues[timeStamp,2] <= minValue)
      {
        decayValues[timeStamp,2] = 0
      }
    }
    #Objective function for uniroot to calculate the half life 
    #We solve for the equation of initialPulse/2 = Equation
    
    objFnc = function(t) 
    {
      z = (1+exp(satPar*t))^-1 + lambda^(t-1) - 0.5
    }
    
    root = uniroot(objFnc,c(0,timePeriod))
    
    halfLife = root$root
  }
  
  halfLifeApprox = which(decayValues[,2] <= initialPulse*0.5)[1]
  
  result = list("HalfLife" = halfLife, "HalfLifeApprox" = halfLifeApprox, "Adstock" = decayValues)
  
  return(result)
}

