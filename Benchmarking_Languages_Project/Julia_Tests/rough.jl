function runSA(nParams,lowerBounds,upperBounds,energyFnc,neighFnc)  
 cooling_sched = 0.995
 maxIters = 1e15
 stopTemp = 1e-150
 eMin = -1e100
 iterationIndicator = 1000
 xVec = zeros(nParams)
 eBest = energyFnc(xVec)
 e = eBest;
 k=1
 T=1
 result = 0.0
 while (k < maxIters && e[1] >= eMin && T > stopTemp)
  newX = neighFnc(xVec,nParams,lowerBounds,upperBounds)
  eNew = energyFnc(newX)
  if(eNew[1] < eBest[1]) 
   eBest = eNew
   e = eNew
   xVec = newX	 
  elseif(rand(1)[1]< exp((e-eNew)/(k*T))[1])
   eBest = eNew
   e = eNew
   xVec = newX
  end
  k = k+1
  T = cooling_sched*T
 end
result = hcat(xVec,eBest)
return(result)	
end

