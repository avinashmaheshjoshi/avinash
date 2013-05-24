function qsort(a,lo,hi)
	i, j = lo, hi
	while i < hi
		pivot = a[(lo+hi)>>>1]
		while i <= j
			while a[i] < pivot; i = i+1; end
			while a[j] > pivot; j = j-1; end
			if i <= j
				a[i], a[j] = a[j], a[i]
				i, j = i+1, j-1
			end
		end
		if lo < j; qsort(a,lo,j); end
		lo, j = i, hi
	end
	return a
end

tic()
for i in 1:15
T3 = randn(7000000)
T3_1 = qsort(T3,1,7000000)
end
timing = toc()
println (timing/15)
