from scipy import *
from numpy import *
from time import *
from fractions import *

timing = zeros(15)
for j in range(15):
	time1 = time()
	a =  ceil(random.uniform(size=400000)*1000)
	b =  ceil(random.uniform(size=400000)*1000)
	T13_1 = zeros(400000)
	for i in range(400000):
		T13_1[i]=gcd(a[i],b[i])
	timing[j]=time()-time1
print mean(timing)

