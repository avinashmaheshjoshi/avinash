require(Rcpp)
require(inline)

gibbscode = '
int N = as<int>(n);
int thn = as<int>(thin);
int i,j;
RNGScope scope;
NumericVector xs(N),ys(N);
double x=0;
double y=0;
for (i=0;i<N;i++) {
  for (j=0;j<thn;j++) {
    x = ::Rf_rgamma(3.0,1.0/(y*y+4));
    y= ::Rf_rnorm(1.0/(x+1),1.0/sqrt(2*x+2));
  }
  xs(i) = x;
  ys(i) = y;
}
return Rcpp::DataFrame::create( Named("x")= xs, Named("y") = ys);
'

fun.Rcpp <- cxxfunction( signature(n="int", thin = "int"),
                            gibbscode, plugin="Rcpp")

fun.R <-function(N=50000,thin=1000)
{
  mat=matrix(0,ncol=2,nrow=N)
  x=0
  y=0
  for (i in 1:N) {
    for (j in 1:thin) {
      x=rgamma(1,3,y*y+4)
      y=rnorm(1,1/(x+1),1/sqrt(2*x+2))
    }
    mat[i,]=c(x,y)
  }
  names(mat)=c("x","y")
  mat
}


fun.cmpR <- cmpfun(fun.R)
fun.cmpRcpp <- cmpfun(fun.Rcpp)

benchmark(
  fun.Rcpp(), 
  fun.R(),
  fun.cmpR(),
  fun.cmpRcpp(),   
  replications=10,
  columns=c("test", "replications",
            "elapsed",
            "relative"),
  order='relative')