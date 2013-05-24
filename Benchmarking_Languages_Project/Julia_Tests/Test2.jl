function func2() 
T2 = abs(reshape(randn(2400*2400),2400,2400))
T2_1 = T2.^1000
end

tic()
for i=1:15 result = func2(); end
timing = toc();
println("Test2 took ",(timing/15)," secs")
