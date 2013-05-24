from scipy import *
from numpy import *
from time import *

timing = zeros(15)

def qsort(lst):
    if len(lst) == 0:
        return []
    else:
        pivot = lst[0]
        lesser = qsort([x for x in lst[1:] if x < pivot])
        greater = qsort([x for x in lst[1:] if x >= pivot])
        return lesser + [pivot] + greater

for i in range(15):
	time1 = time()
	T3 = array(random.normal(size = 7000000))
	T3_1 = qsort(T3)
	timing[i] = time()-time1
print mean(timing)
