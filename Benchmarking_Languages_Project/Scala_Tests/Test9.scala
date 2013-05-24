var timing = DenseVector.zeros[Long](15)
for(i <- 0 until 15){
	
	val start = System.currentTimeMillis()
	val T9 = DenseMatrix.rand(3000,3000)
	val T9_1 = cholesky(T9)
	val finish = System.currentTimeMillis()
	timing(i) = (finish - start)
}

println(mean(timing)/1000)


