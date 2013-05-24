package main
import Math._
import util._
/** 
 *	A simple fft implementation ( the array must have length = 2 ^ n for some n in N+ )
 * 		request Complx.scala ( a "Complex number" class ) 
 *
 *	author me := Gabriele Lami
 * 	koteth@gmail.com
 */

object fftShort {
  
  def fft ( x : Array[Complx] ): Array[Complx] = {
    
    val N = x.length
    if ( N == 1 ) return Array( x(0) )
    if ( (N % 2) == 1 ) throw new RuntimeException( "N must be a power of 2 !!" )

    // fft of even and odd terms
    val qArray = fft( x.indices.filter( _%2 == 0 ).map( x( _ ) ) )
    val rArray = fft( x.indices.filter( _%2 == 1 ).map( x( _ ) ) )
    
    x.indices.map( el => coeffCalc( N, el, qArray( el%(N/2) ), rArray( el%(N/2) ) ) )
  }
  
  val coeffCalc = ( N: Int , k: Int , qArrayEl : Complx , rArrayEl : Complx ) => {

    val kt = -2 * ( k%(N/2) ) * Pi / N 
    qArrayEl + ( new Complx ( kt ) * ( rArrayEl  * ( signum( N/2 - k - 0.5 ) ) ) ) 
    
  }	
  
  def ifft( x : Array[Complx] ): Array[Complx] = 
    fft( x.map( _.conjugate ) ).map( _  * ( 1.0 / x.length )  )
  
  def powerSpectrum( x: Array[Complx] ) : Array[Double] =  
    fft( x ).map( el => el .* ( el.conjugate ).real / ( 2 * Pi ) ) 

  def circularConvolution( x : Array[Complx], y : Array[Complx] ) : Array[Complx] = {
  
    if ( x.length !=  y.length ) throw new RuntimeException( "vectors lengths are different !!" )
    val	aArra = fft( x )
    val	bArra = fft( y )
    ifft ( aArra.indices.map( el => aArra( el ) * ( bArra( el ) ) ) )
        
  }
 
  def prntArray( title: String, x: Any ) = {
    printTitle( title )
    x match {
      case a: Array[Complx] => a.indices.foreach( el => println( (el + 1) + " - " + a( el ) ) )  
      case b: Array[Double] => b.indices.foreach( el => println( (el + 1) + " - real: " + b( el ) ) )
      case _ => println( "\nNothing to show...\n" )
    }
  }

  val printTitle = ( title: String ) =>
    println ( "-" * ( title.length + 2 ) +  "\n " + title  + "\n" + "-" * ( title.length + 2 ) ) 
   
  def main(args : Array[String]) : Unit = {
   
    var N = 64
    println ( " - beginning test - " )
    
    // test array 
    val arrayTest : Array[Complx] = new Array( N ).indices.map( el => new Complx( sin ( el ) , 2 * cos( el / 3 ) ) ) 
    val fftCal = fft ( arrayTest )
    
    val toShowMap = Map( 
      "0 - test generic case prntArray" -> "any" ,
      "1 - input array"  -> arrayTest , 
      "2 - fft Transform" -> fftCal ,
      "3 - ifft Transform" -> ifft( fftCal ) ,
      "4 - convolution: input * input" -> circularConvolution( arrayTest , arrayTest ) , 
      "5 - power spectrum" -> powerSpectrum( arrayTest )
    )
    
    toShowMap.keySet.toList.sort( _ < _ ).foreach( el => prntArray( el , toShowMap( el ) ) )
   
  }
}
