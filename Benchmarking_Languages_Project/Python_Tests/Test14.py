from scipy import *
from numpy import *
from time import *

timing = zeros(15)
for i in range(15):
	time1 = time()
	T14 = zeros((500,500))
 	for j in range(500):
  		for k in range(500):
   			T14[k,j] = abs(j-k) + 1
	timing[i]=time()-time1
print mean(timing)
