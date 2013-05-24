
#======================Unit case for the SA function======================#

#Function to check if a package is installed
is.installed <- function(pkg){ is.element(pkg, installed.packages()[,1])}
#Check if package var has been installed
if(!is.installed('RUnit')){ install.packages("RUnit",repos="http://lib.stat.cmu.edu/R/CRAN")}

#Loading the RUnit library
library("RUnit")

source('Adstock_PSO.R')

#To run the test case, input a dataset with known minimum mape, 
#input the known minimum mape in place of "min"
checkAdStock.PSO = function(uid,dataset,adSep,Vars_no_Adstocks,depVar,useJMS,energyFnc=NULL,min)
{
  checkTolerance = 10^-5
  optimMape = AdStock.PSO(uid,dataset,adSep,Vars_no_Adstocks,depVar,useJMS,energyFnc)
  checkTrue(abs(min-optimMape$value)<checkTolerance)
}
checkAdStock.PSO(123,abc,"_",0,"x",FALSE,energyFnc=findMinMape,0)