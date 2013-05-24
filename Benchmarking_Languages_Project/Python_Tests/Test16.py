execfile("SA.py")

def energyFnc(x):
	ret = 10*2 + (x[0]**2 - 10*cos(2*pi*x[0])) + (x[1]**2 - 10*cos(2*pi*x[1]))
	return ret

def neighFnc(nParams,lowerBounds,upperBounds):
 Nhood = zeros(nParams)
 for i in range(nParams):
  Nhood[i] = uniform(lowerBounds[i],upperBounds[i]) 
 return Nhood

time1 = time()
T16 = runSA(2,[-5.1,-5.1],[5.1,5.1],energyFnc,neighFnc)
print(time() - time1)
