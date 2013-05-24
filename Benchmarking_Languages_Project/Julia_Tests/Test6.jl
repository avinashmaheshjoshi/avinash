tic()
for i in 1:15
T6 = randn(2400000)
T6_1 = fft(T6)
end
timing = toc()
println (timing/15)

