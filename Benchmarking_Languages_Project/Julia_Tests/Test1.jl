function func1()
 T1 = reshape(randn(2500*2500),2500,2500)
 T1_1 = transpose(T1)
 T1_1 = reshape(T1_1,1250,5000)
 T1 = transpose(T1)
end	

tic()
for i=1:15 result = func1(); end
timing = toc();
println("Test1 took ",(timing/15)," secs")
