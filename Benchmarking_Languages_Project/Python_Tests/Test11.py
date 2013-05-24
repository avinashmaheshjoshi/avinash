from scipy import *
from numpy import *
from time import *


timing = zeros(15)
for i in range(15):
	time1 = time()
	T11 = floor(random.normal(size=3500000)*1000)
	T11 = T11.astype(float64)
	phi = 1.618033988749895
	T11_1 = []
	for j in range(3500000):
		T11_1.append((phi**T11[j] - (-phi)**(-T11[j]))/sqrt(5))
	timing[i]=time()-time1
print mean(timing)

