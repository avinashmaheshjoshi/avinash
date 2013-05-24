from scipy import *
from numpy import *
from time import *

timing = zeros(15)
for i in range(15):
	time1 = time()
	T12 = repeat(range(1,3001),3000)
	T12 = reshape(T12,(3000,3000))
	T12_1 = 0.5*2/(T12.T + matrix(range(3000)).T)	
	timing[i]=time()-time1
print mean(timing)

