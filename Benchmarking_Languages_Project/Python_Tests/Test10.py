from scipy import *
from numpy import *
from time import *


timing = zeros(15)
for i in range(15):
	time1 = time()
	T10 = matrix(random.normal(size=(1600,1600)))
	T10_1 = T10.I 
	timing[i]=time()-time1
print mean(timing)

