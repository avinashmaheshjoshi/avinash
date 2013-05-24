library(inline)
library(RcppArmadillo)
library(rbenchmark)
library(compiler)

src <- '
int ncol = as<int>(nCol);
// Generate the first row and create a Toeplitz matrix
return(Rcpp::wrap(arma::toeplitz(arma::randu<arma::vec>(ncol))));

'

fun.Rcpp <- cxxfunction(body=src,
                        signature(nCol="numeric"),
                        plugin="RcppArmadillo",
                        verbose=FALSE)

fun.R <- function(nCol){
  #generate the first row
  M=runif(nCol,0,1)
  #Compute and return the toeplitz matrix
  return(toeplitz(M))
}


fun.cmpR <- cmpfun(fun.R)
fun.cmpRcpp <- cmpfun(fun.Rcpp)

dimension <- 500

benchmark(
   fun.Rcpp(dimension), 
   fun.R(dimension),
   fun.cmpR(dimension),
   fun.cmpRcpp(dimension),   
   replications=100,
   columns=c("test", "replications",
            "elapsed", "relative"),
   order='relative')