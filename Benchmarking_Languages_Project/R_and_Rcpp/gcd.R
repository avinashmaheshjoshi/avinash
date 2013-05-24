library(inline)
library(RcppArmadillo)
library(rbenchmark)
library(compiler)

# Function to compute a GCD 
inc <- 'int GCD(int a, int b)
{
    while( 1 )
    {
        a = a % b;
  	if( a == 0 )
			return b;
		b = b % a;

        if( b == 0 )
			return a;
    }
}'

src <-'
 int npairs = as<int>(nPairs);
 //Create the pairs for which to calculate the GCD
 arma::vec  v1 = floor(1000*arma::randu<arma::vec>(npairs));
 arma::vec  v2 = floor(1000*arma::randu<arma::vec>(npairs));
 
 //Loop and replace one of the vectors with the GCD
 for(int i=0;i<npairs;i++){
    v1(i)= GCD(v1(i),v2(i));
 }
 
 return(Rcpp::wrap(v1));
'

 fun.Rcpp <- cxxfunction(body=src,
                        signature(nPairs="numeric"),
                         include=inc,
                        plugin="RcppArmadillo",
                        verbose=FALSE)
 

  gcd2 <- function(x, y) {
    if (sum(y > 1.0E-4) == 0) x else {y[y == 0] <- x[y == 0];
    Recall(y, x %% y)}
  }
  
  fun.R <- function(nPairs){
      a <- ceiling(runif(floor(nPairs))*1000)
      b <- ceiling(runif(floor(nPairs))*1000)
      return(gcd2(a,b))
  }
  
fun.cmpR <- cmpfun(fun.R)
fun.cmpRcpp <- cmpfun(fun.Rcpp)

nPairs=1000

# Run the benchmarks      
benchmark(
   fun.Rcpp(nPairs), 
   fun.R(nPairs),
   fun.cmpR(nPairs),
   fun.cmpRcpp(nPairs),   
   replications=1,
   columns=c("test", "replications",
            "elapsed", "relative"),
   order='relative')
