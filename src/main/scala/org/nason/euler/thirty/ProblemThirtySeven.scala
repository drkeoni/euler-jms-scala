package org.nason.euler.thirty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemThirtySeven {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  { 
    val (pTable, primes) = NumberUtils primes (10000000)
    // number must start and end with 2,3,5,7
    val PrimeDigits = Array(2,3,5,7) map (_.toString)
    
    val pow10: Int=>Int = {
      def pow10rec( f:Int=>Int )(n:Int) = if (n==0) 1; else 10*f(n-1)
      NumberUtils.Memoize.Y( pow10rec )
    }
    
    def candidate( n:Int ):Iterable[String] =
    {
      if ( n==0 )
      {
        for( i <- PrimeDigits; 
          j <- PrimeDigits )
          yield i + j
      }
      else
      {
        for( i <- PrimeDigits; 
          j <- PrimeDigits; 
          k <- ( ( 0 until pow10(n) ) map( ("%0" + n + "d").format(_) )) )
          yield i + k + j
      }
    }
    
    def suffix( a:String ) = a.substring(1)
    def prefix( a:String ) = a.substring(0,a.length-1)
    
    /** Returns list of booleans reflecting if each truncation of the number
     * represented by v is prime.  The formation of truncations is governed by
     * the recursive access pattern f.r
     */
    def truncatable( f: String=>String )( v:String ) : List[Boolean] =
    {
      val vi = Integer.parseInt(v)
      val vs = vi.toString
      if ( vs.length==1 )
        List( pTable(vi) )
      else
      {
        pTable(vi) :: truncatable(f)( f(v) )
      }
    }
    
    def leftTruncatable( v:String ) = truncatable(suffix)(v).forall(i=>i)
    def rightTruncatable( v:String ) = truncatable(prefix)(v).forall(i=>i)
    
    ( for( i <- 0 to 5; v <- candidate(i); if leftTruncatable(v) && rightTruncatable(v) )
      yield Integer.parseInt(v) )
      .reduceLeft(_+_)
  }
}