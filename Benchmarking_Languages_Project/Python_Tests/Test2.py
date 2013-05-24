from scipy import *
from numpy import *
from time import *

timing = zeros(15)


for i in range(15):	
	time1 = time()
	T2 = matrix(random.normal(size=(2400,2400)),dtype=float64)
	T2_1 = power(T2,1000)
	timing[i] = time()-time1
print mean(timing)
