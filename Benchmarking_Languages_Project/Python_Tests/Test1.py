from scipy import *
from numpy import *
from time import *

timing = zeros(15)

for i in range(15):
	time1 = time()
	T1 = matrix(random.normal(size=(2500,2500)))
	T1_1 = matrix.getT(T1)
	T1_1 = reshape(T1_1,(1250,5000))
	T1 = matrix.getT(T1_1)         
	timing[i] = time() - time1
print mean(timing)
