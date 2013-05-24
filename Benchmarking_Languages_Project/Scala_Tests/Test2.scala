var timing = DenseVector.zeros[Long](15)
for(i <- 0 until 15){
	
	val start = System.currentTimeMillis()
	var T2 = DenseMatrix.randn(2400,2400)
	var T2_1 = T2 :^1000
	val finish = System.currentTimeMillis()
	timing(i) = (finish - start)
}
println(mean(timing)/1000)


