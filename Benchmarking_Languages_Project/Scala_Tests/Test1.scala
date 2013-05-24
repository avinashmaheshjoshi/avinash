var timing = DenseVector.zeros[Long](15)
for(i <- 0 until 15){
	
	val start = System.currentTimeMillis()
	var T1 = DenseMatrix.randn(2500,2500)
	var T1_1 = T1.t
	var T1_2 = T1.reshape(1250,5000)
	var T1_3 = T1_2.t
	val finish = System.currentTimeMillis()
	timing(i) = (finish - start)
}
println(mean(timing)/1000)


