library(inline)
library(RcppArmadillo)
library(rbenchmark)
library(compiler)

src <- '

int nrow = as<int>(nRow);
int ncol = as<int>(nCol);

//Create a random Armadillo matrix
arma::mat X = arma::randu<arma::mat>(nrow,ncol);

//Compute and return the Cholesky decomposition
return(Rcpp::wrap(arma::chol(trans(X)*X)));
'

fun.Rcpp <- cxxfunction(body=src,
                        signature(nCol="numeric",nRow="numeric"),
                        plugin="RcppArmadillo",
                        verbose=FALSE)

fun.R <- function(nRow,nCol){
  M=matrix(runif(nRow*nCol,0,1),nrow=nRow,ncol=nCol)
  #Compute and return the determinant
  return(chol(M,pivot=TRUE))
}

fun.cmpR <- cmpfun(fun.R)
fun.cmpRcpp <- cmpfun(fun.Rcpp)

dimension <- 3000

benchmark(
   fun.Rcpp(dimension,dimension), 
   fun.R(dimension,dimension),
   fun.cmpR(dimension,dimension),
   fun.cmpRcpp(dimension,dimension),   
   replications=100,
   columns=c("test", "replications",
            "elapsed", "relative"),
   order='relative')