library(inline)
library(Rcpp)
library(rbenchmark)
library(compiler)

src <- '

int num_items = Rcpp::as<int>(nNumbers);
Eigen::FFT<double> fft;
std::vector<double> timevec(num_items);
std::generate_n(timevec.begin(), num_items,rand);
std::vector<std::complex<double> > freqvec;
fft.fwd(freqvec,timevec);
Rcpp::NumericVector xx( freqvec.begin(), freqvec.end() );
return(Rcpp::wrap(xx));
'
fun.RCpp <- cfunction(signature(nNumbers="integer"), body=src,
                      includes="#include <unsupported/Eigen/FFT>",
                      Rcpp=TRUE,
                      cppargs=c("-I/usr/include","-I/home/labs2/Desktop/RcppBenchmarks/eigen/"),
                      libargs="-lgsl -lgslcblas")

fun.R <- function(nNumbers){
  z=complex(real=runif(nNumbers),imaginary=runif(nNumbers))
  return(fft(z))
}

fun.cmpR <- cmpfun(fun.R)
fun.cmpRcpp <- cmpfun(fun.Rcpp)

nNumbers <- 1200

benchmark(
   fun.Rcpp(nNumbers), 
   fun.R(nNumbers),
   fun.cmpR(nNumbers),
   fun.cmpRcpp(nNumbers),   
   replications=10,
   columns=c("test", "replications",
            "elapsed",
            "relative"),
   order='relative')