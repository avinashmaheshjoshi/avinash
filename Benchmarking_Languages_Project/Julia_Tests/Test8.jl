tic()
for i in 1:15
T8 = reshape(randn(2500*2500),2500,2500)
T8_1 = det(T8)
end
timing = toc()
print (timing/15)


