function corr(x,y)
z1 = (x-mean(x))/std(x)
z2 = (y-mean(y))/std(y)
res = (1/(length(x) - 1))*(sum(z1.*z2))
return(res)
end

function corrmat(a)
(m,n) = size(a)
ret = zeros(n,n)
 for i in 1:n
  for j in 1:n
  x = reshape(a[:,i],m)
  y = reshape(a[:,j],m)
  ret[i,j] = corr(x,y)
  end 
 end
return(ret)
end

