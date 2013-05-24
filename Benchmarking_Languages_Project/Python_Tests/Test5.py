from scipy import *
from numpy import *
from time import *

timing = zeros(15)
for i in range(15):
	time1 = time()
	x = matrix(random.normal(size=(3000,3000)))
	y = random.normal(size=3000)
	betas = linalg.solve(x,y)
	timing[i]=time()-time1
print mean(timing)
