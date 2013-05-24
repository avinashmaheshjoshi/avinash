library(inline)
library(RcppArmadillo)
library(rbenchmark)
library(compiler)

src <- '
int nrow = as<int>(nRow);
int ncol = as<int>(nCol);
//Create a random Armadillo matrix
arma::mat A = arma::randu<arma::mat>(nrow,ncol);

//Declare variables to hold eigen values and eigen vectors
arma::cx_vec eigval;
arma::cx_mat eigvec;

// Do an eigen decomposition and get eigenvalues and eigen vectors
arma::eig_gen(eigval, eigvec, A);

// return the eigen values
return(Rcpp::wrap(eigval));
'
fun.Rcpp <- cxxfunction(body=src,
                        signature(nCol="numeric",nRow="numeric"),
                        plugin="RcppArmadillo",
                        verbose=FALSE)

fun.R <- function(nrows,ncols){
  M=matrix(runif(nRow*nCol,0,1),nrow=nRow,ncol=nCol)
  #Compute and return the eigen values
  return(eigen(M)$values)
}

fun.cmpR <- cmpfun(fun.R)
fun.cmpRcpp <- cmpfun(fun.Rcpp)

dimension <- 640

benchmark(
   fun.Rcpp(dimension,dimension), 
   fun.R(dimension,dimension),
   fun.cmpR(dimension,dimension),
   fun.cmpRcpp(dimension,dimension),   
   replications=100,
   columns=c("test", "replications",
            "elapsed", "relative"),
   order='relative')