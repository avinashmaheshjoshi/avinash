from scipy import *
from numpy import *
from time import *

timing = zeros(15)
for i in range(15):
	time1 = time()
	T9 = matrix(random.normal(size=(3000,3000)))
	T9 = T9.T * T9
	T9_1 = linalg.cholesky(T9)
	timing[i]=time()-time1
print mean(timing)

