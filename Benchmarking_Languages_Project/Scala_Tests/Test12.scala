var timing = DenseVector.zeros[Long](15)
for(i <- 0 until 15){
	
	val start = System.currentTimeMillis()
	var T12 = DenseMatrix.zeros[Int](3000,3000)
	val vec = 1 to 3000
	
	for(i <- 0 to 2999)
		T12(::,i) := vec

	var T12_1 = 1:/(T12.t + (T12-1))	
	val finish = System.currentTimeMillis()
	timing(i) = (finish - start)
}

println(mean(timing)/1000)


