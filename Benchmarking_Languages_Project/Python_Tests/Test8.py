from scipy import *
from numpy import *
from time import *


timing = zeros(15)
for i in range(15):
	time1 = time()
	T8 = matrix(random.normal(size=(2500,2500)))
	T8_1 = linalg.det(T8)
	timing[i]=time()-time1
print mean(timing)

