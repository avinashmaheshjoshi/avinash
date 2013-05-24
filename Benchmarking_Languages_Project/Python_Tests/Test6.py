from scipy import *
from numpy import *
from time import *

timing = zeros(15)
for i in range(15):
	time1 = time()
	T6 = random.normal(size = 2400000)
	T6_1 = fft.fft(T6)
	timing[i]=time()-time1
print mean(timing)
