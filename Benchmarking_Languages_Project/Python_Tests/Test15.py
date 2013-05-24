from math import *
from random import *
from numpy import *
from scipy import *
from time import * 


def gibbs(N=50000,thin=1000):
	mat = zeros((50000,2))
	x=0
	y=0
	 
	for i in range(N):
		print i
		for j in range(thin):
			x = gammavariate(3,1.0/(y*y+4))
			y = gauss(1.0/(x+1),1.0/sqrt(2*x+2))
		mat[i,] = [x,y]
       

time1 = time()
T15 = gibbs()
print(time()-time1)        	

