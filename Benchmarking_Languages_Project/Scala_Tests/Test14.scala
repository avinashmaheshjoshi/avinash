var timing = DenseVector.zeros[Long](15)
for(i <- 0 until 15){
	
	val start = System.currentTimeMillis()
	var T14 = DenseMatrix.zeros[Double](500,500)

	for(j <- 0 to 499)
		for(k <- 0 to 499)
			T14(k,j) = abs(j-k) + 1

	val finish = System.currentTimeMillis()
	timing(i) = (finish - start)
}

println(mean(timing)/1000)


