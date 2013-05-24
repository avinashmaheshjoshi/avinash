function func15(N::Int, thin::Int)
    mat = Array(Float64, (N, 2))
    x   = 0.
    y   = 0.
    for i = 1:N
        for j = 1:thin
            x = randg(3) * (y*y + 4)
            y = 1/(x + 1) + randn()/sqrt(2(x + 1))
        end
        mat[i,:] = [x,y]
    end
    mat
end

tic()
for i in 1:15
T15 = func15(50000,1000)
end
timing = toc()
println(timing/15)
