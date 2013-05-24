
#======================Unit case for the SA function======================#

#Function to check if a package is installed
is.installed <- function(pkg){ is.element(pkg, installed.packages()[,1])}
#Check if package var has been installed
if(!is.installed('RUnit')){ install.packages("RUnit",repos="http://lib.stat.cmu.edu/R/CRAN")}

#Loading the RUnit library
library("RUnit")

source('Adstock_SA.R')

#To run the test case, input a dataset with known minimum mape, 
#input the known minimum mape in place of "min"
checkAdStock.SA = function(dataset,adSep,degreesOfFrdm,Vars_no_Adstocks,depVar,useJMS,energyFnc=findMinMape,neighFnc=generateNeighbourhood,min)
{
  checkTolerance = 10^-5
  optimMape = AdStock.SA(dataset,adSep,degreesOfFrdm,Vars_no_Adstocks,depVar,useJMS,energyFnc=findMinMape,neighFnc=generateNeighbourhood)
  checkTrue(abs(min-optimMape$value)<checkTolerance)
}

