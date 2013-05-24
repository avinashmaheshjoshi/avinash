function runSA(nParams,lowerBounds,upperBounds,energyFnc,neighFnc)  
 cooling_sched = 0.999
 maxIters = 1e150
 stopTemp = 1e-150
 eMin = 0.001
 iterationIndicator = 1000
 xVec = neighFnc(nParams,lowerBounds,upperBounds)
 eBest = energyFnc(xVec)
 e = eBest;
 k=1
 T=1
 result = 0.0
 while (k < maxIters && e >= eMin && T > stopTemp)
  newX = neighFnc(nParams,lowerBounds,upperBounds)
  eNew = energyFnc(newX)
  if(eNew < eBest) 
   eBest = eNew
   e = eNew
   xVec = newX	 
  elseif((rand(1)< exp((e-eNew)/(k*T)))[1])
   eBest = eNew
   e = eNew
   xVec = newX
  end
  k = k+1
  T = cooling_sched*T
 end
return xVec, eBest	
end

