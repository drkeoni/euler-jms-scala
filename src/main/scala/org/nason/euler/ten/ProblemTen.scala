package org.nason.euler.ten

import scala.collection.mutable.HashMap

object ProblemTen {
  def main(args: Array[String]) {
    println( solution )
  }
  
  def solution =
  {
    val MAX = BigInt(2000000)
    def primes( max : BigInt ) =
    {
      var prime = new HashMap[BigInt,Boolean]
      for( i<-BigInt(2) to max )
        prime(i) = true
      val m = Math ceil (Math sqrt( max toDouble )) toInt
      var stride = BigInt(2)
      while( stride<=m )
      {
        var i = BigInt(2)*stride
        while( i<=max )
        {
          prime( i ) = false
          i += stride
        }
        stride += 1
        while( stride<=m && !prime(stride) )
        {
          stride += 1
        }
      }
      prime filter { a => a._2 } map { a => a._1 }
    }
    primes(MAX) reduceLeft { (a,b) => a+b }
    // for debugging
    //(primes(MAX) toList).sort( (a,b) => a < b )
  }
}