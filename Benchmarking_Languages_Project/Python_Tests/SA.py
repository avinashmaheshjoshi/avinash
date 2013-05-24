from random import *
from math import *
from numpy import *
from scipy import *
from time import * 


def runSA(nParams,lowerBounds,upperBounds,energyFnc,neighFnc):  
 cooling_sched = 0.999
 maxIters = 1e15
 stopTemp = 1e-150
 eMin = 1e-150
 iterationIndicator = 1000
 xVec = neighFnc(nParams,lowerBounds,upperBounds)
 eBest = energyFnc(xVec)
 e = eBest
 k=1
 T=1
 result = 0.0
 while (k < maxIters and e >= eMin and T > stopTemp):
  newX = neighFnc(nParams,lowerBounds,upperBounds)
  eNew = energyFnc(newX)
  if (eNew < eBest): 
   eBest = eNew
   e = eNew
   xVec = newX	 
  elif (uniform(0,1)< exp((e-eNew)/(k*T))):
   eBest = eNew
   e = eNew
   xVec = newX
  k = k+1  
  T = cooling_sched*T
 print k   
 return xVec, eBest	


