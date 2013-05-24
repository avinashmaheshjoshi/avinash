from scipy import *
from numpy import *
from time import *
timing = zeros(15)
for i in range(15):
	time1 = time()
	T4 = matrix(random.normal(size=(2800,2800)))
	T4_1 = T4.T * T4
	timing[i]=time()-time1
print mean(timing)
