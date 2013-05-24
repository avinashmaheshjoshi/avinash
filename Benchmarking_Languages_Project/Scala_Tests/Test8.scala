var timing = DenseVector.zeros[Long](15)
for(i <- 0 until 15){
	
	val start = System.currentTimeMillis()
	val T8 = DenseMatrix.rand(2500,2500)
	val T8_1 = det(T8)
	val finish = System.currentTimeMillis()
	timing(i) = (finish - start)
}

println(mean(timing)/1000)


