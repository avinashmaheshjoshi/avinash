tic()
for i in 1:15
T7 = reshape(randn(640*640),640,640)
T7_1 = eig(T7)
end
timing = toc()
println(timing/15)
