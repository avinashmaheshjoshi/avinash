from scipy import *
from numpy import *
from time import *

timing = zeros(15)
for i in range(15):
	time1 = time()
	T7 = matrix(random.normal(size=(640,640)))
	T7_1 = linalg.eig(T7)
	timing[i]=time()-time1
print mean(timing)
