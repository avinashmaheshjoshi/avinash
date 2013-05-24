library(inline)
library(RcppArmadillo)
library(rbenchmark)
library(compiler)

src <- '
int nrows = as<int>(nRows);

//Create the Hilbert matrix
Rcpp::NumericMatrix M(nrows,nrows);

// Fill the elements of the Hilbert matrix
for (int i=0; i<nrows; i++) {
  for (int j=0; j<nrows; j++) {
    M(i,j) = 1/(i+j+1);
  }
}

//return the Hilbert matrix
return(M);
'

fun.Rcpp <- cxxfunction(body=src,
                        signature(nRows="numeric"),
                        plugin="RcppArmadillo",
                        verbose=FALSE)

#Create a Hilbert matrix in R
fun.R <- function(nRows){
  M <- rep(1:nRows, nRows);
  dim(M) <- c(nRows, nRows);
  M <- 1 / (t(M) + 0:(nRows-1))
  return(M)
}

fun.cmpR <- cmpfun(fun.R)
fun.cmpRcpp <- cmpfun(fun.Rcpp)

dimension <- 3000

benchmark(
   fun.Rcpp(dimension), 
   fun.R(dimension),
   fun.cmpR(dimension),
   fun.cmpRcpp(dimension),   
   replications=100,
   columns=c("test", "replications",
            "elapsed", "relative"),
   order='relative')

