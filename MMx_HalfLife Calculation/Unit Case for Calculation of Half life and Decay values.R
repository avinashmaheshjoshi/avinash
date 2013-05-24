#---------------------------------------------------------------------------------------#
#--                                                                                   --#
#--  Project Name:   Sanofi MMx--# 
#--  Task : Unit Cases for Half life calculation--# 
#--  Sub-Task: Input a variable with known half life--#
#--  version : 1.0 date :27/03/12 author : Avinash Joshi--#
#--  SVN Directory: http://xp-dev.com/svn/MxO/Labs/code/Avinash--#
#---------------------------------------------------------------------------------------#
source("C:\\Documents and Settings\\avinash.joshi\\My Documents\\Done!\\halfLife Calc and Decay Values.R")

checkHalfLife <- function(lambda,initialPulse,Eqtn,timePeriod,minValue,satPar = NULL,knownHalfLife,knownHalfLifeApprox)
{
  result = getHalfLife(lambda,initialPulse,Eqtn,timePeriod,minValue,satPar = NULL)
  if(abs(result$HalfLife - knownHalfLife) <= 10^-3 && abs(result$HalfLifeApprox-knownHalfLifeApprox) <= 10^-3)
  {
    print("getHalfLife code works properly")
  }
  else
  {
    print("getHalfLife code doest not work")
  }
}

#Known halflife value for a geometric decay of lambda 0.5 is 2.
checkHalfLife(0.5,10000,0,100,0.001,NULL,2,2)

