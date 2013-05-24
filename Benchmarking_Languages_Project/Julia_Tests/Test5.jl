tic()
for i in 1:15
T5 = reshape(randn(3000*3000),3000,3000)
x = T5[:,2:end]
y = T5[:,1]
(m,n) = size(x)
(q,r) = qr(x)
betas = r[1:n,:]\(transpose(q) * y)
end
timing = toc()
println (timing/15)

