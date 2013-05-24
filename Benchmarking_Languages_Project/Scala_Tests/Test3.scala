var timing = DenseVector.zeros[Long](15)
for(i <- 0 until 15){
	
	val start = System.currentTimeMillis()
	var T3 = Array.fill(7000000)(scala.util.Random.nextDouble)
	scala.util.Sorting.quickSort(T3)	
	val finish = System.currentTimeMillis()
	timing(i) = (finish - start)
}

println(mean(timing)/1000)


