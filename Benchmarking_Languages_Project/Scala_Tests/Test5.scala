var timing = DenseVector.zeros[Long](15)
for(i <- 0 until 15){
	
	val start = System.currentTimeMillis()
	val x = DenseMatrix.randn(3000,3000)
	val y = DenseMatrix.randn(3000,1)
	val Betas = x\y
	val finish = System.currentTimeMillis()
	timing(i) = (finish - start)
}

println(mean(timing)/1000)

