var timing = DenseVector.zeros[Long](15)
for(i <- 0 until 15){
	
	val start = System.currentTimeMillis()
	val T10 = DenseMatrix.randn(1600,1600)
	val T10_1 = inv(T10)
	val finish = System.currentTimeMillis()
	timing(i) = (finish - start)
}

println(mean(timing)/1000)


