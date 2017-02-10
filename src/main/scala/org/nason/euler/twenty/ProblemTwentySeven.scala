package org.nason.euler.twenty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemTwentySeven {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    val MAX_PRIME = 20000
    val pTable = ( 0 to MAX_PRIME ).map( v => NumberUtils isPrime v )
    def isPrime( x:Int ) = if (x>0 && x<=MAX_PRIME) pTable(x); else NumberUtils isPrime x
    def numPrimes( a:Int, b:Int ) =
    {
      var i = 0
      def quad( i:Int ) = b + i * ( a + i )
      var y = quad(i)
      while( y>0 && isPrime(y) )
      {
        i += 1
        y = quad(i)
      }
      i
    }
    val p = ( for( a <- -999 to 999; b <- -999 to 999 )
        yield (a,b,numPrimes( a, b )) )
        .reduceLeft( (p1,p2) => if (p1._3>p2._3) p1; else p2 )
    p._1 * p._2
    //numPrimes( -79, 1601 )
  }

}