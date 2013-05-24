var timing = DenseVector.zeros[Long](15)

for(i <- 0 until 15){
	
	val start = System.currentTimeMillis()
	var T15 = DenseMatrix.zeros[Double](50000,2)
	var x = 0.0
	var y = 0.0
	for(i <- 0 to 49999)
		for(j <- 1 to 1000)
			x = breeze.stats.distributions.Gamma(3,1.0/(y*y + 4)).draw()
			y = breeze.stats.distributions.Gaussian(1.0/x+1,breeze.numerics.sqrt(2*x + 2)).draw()	
	val finish = System.currentTimeMillis()
	timing(i) = (finish - start)
}

println(mean(timing)/1000)


