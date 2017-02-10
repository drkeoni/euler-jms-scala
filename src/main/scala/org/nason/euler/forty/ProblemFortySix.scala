package org.nason.euler.forty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils


object ProblemFortySix {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  /*
   * It's probably faster to use a search over 2*n^2 rather than a search
   * over primes...but this works...(if slow)...
   */
  def solution =
  {
    val MaxN = 100000
    val primeTable = NumberUtils.primes(MaxN)._2
    var maxPrimeIndex = 1
    def isInt( v:Double ) = math.floor(v)==v
    def isGoldbach( i:Int ) : Boolean =
    {
      for( j<-maxPrimeIndex to 1 by -1 )
      {
        val delta = i - primeTable(j)
        val t = delta / 2.0
        if (isInt(math.sqrt(t)))
          return true
      }
      return false
    }
    def isPrime( j:Int ) =
    {
      if (primeTable(maxPrimeIndex)==j)
      {
        maxPrimeIndex += 1
        true
      }
      else
      {
        false
      }
    }
    ( for( j<-1 to 48000; i=2*j+1; if !isPrime(i) && !isGoldbach(i) ) yield (i,isGoldbach(i)) )
      .map( v => "%d:%s".format(v._1,v._2) )
      .mkString(",")
  }
}