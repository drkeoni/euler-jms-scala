package org.nason.euler.fifty

import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemFifty {
  def main(args: Array[String])
  {
    timing( { println( solution3 ) } )
  }
  
  /**
   * Execute operation f and report the amount of time that it took
   */
  def timing( f: =>Unit ) =
  {
    val then = System.currentTimeMillis
    f
    val now = System.currentTimeMillis
    println( "Took %g sec".format( (now-then).toDouble/1000.0 ) )
  }
  
  def solution =
  {
    val ( isPrimesBig, primesBig ) = NumberUtils primes 10000000
    val PrimeMax = 1000000
    val primes = primesBig filter (_<PrimeMax)
    val sums = Array.fill(primes.length)(0)
    for( i <- 1 until primes.length ) sums(i) = sums(i-1) + primes(i-1)
    val a = ( for( i<-0 until sums.length; si=sums(i); j <- i+1 until sums.length; sj=sums(j);
         if (sj-si<PrimeMax) && isPrimesBig(sj-si) ) yield (primes(i+1),primes(j),j-i,sj-si) ) toArray
    val b = a.sortWith( _._3 > _._3 ).head
    "%d:%d:%d:%d".format(b._1,b._2,b._3,b._4)
  }
  
  def solution2 =
  {
    val ( isPrimesBig, primesBig ) = NumberUtils primes 1000000
    val PrimeMax = 1000000
    def maxSeq( start:Int ) =
    {
      ( ( (start until primesBig.length) map primesBig )
        .scanLeft(0)(_+_).tail )
        .takeWhile(_<PrimeMax)
        .zipWithIndex
        .filter( i => NumberUtils.fastPrime(i._1))
        .takeRight(1)(0)
    }
    def seqMax(i: Iterable[(Int,Int)]) = {
      if (i.isEmpty) None
      else Some(i.reduceLeft((a,b) => if (a._2 > b._2) a else b))
    }
    seqMax( ( 0 until PrimeMax ) map maxSeq )
  }
  
  def solution3 =
  {
    // slurp in file
    val DataPath = EulerUtils.DataRoot + "/" + "primes1000000.txt"
    val primes = io.Source.fromFile(DataPath).getLines().toArray map (Integer.parseInt(_))
    //primes takeWhile( _ < 1000 ) mkString ","
    val PrimeMax = 1000000
    val IMax = primes takeWhile( _<PrimeMax ) length
    def maxSeq( i:Int ) = 
    {
      var s = primes(i)
      var j = i+1
      var maxPrime = 0
      var maxLen = 1
      while( j<primes.length && s<PrimeMax )
      {
        s += primes(j)
        if ( s<PrimeMax && (NumberUtils isPrime s) )
        {
          maxPrime = s
          maxLen = j - i + 1
        }
        j += 1
      }
      Tuple2( maxLen, maxPrime )
    }
    def seqMax(i: Iterable[(Int,Int)]) = {
      if (i.isEmpty) None
      else Some(i.reduceLeft((a,b) => if (a._1 > b._1) a else b))
    }
    seqMax ( for( i <- 0 until IMax ) yield maxSeq(i) )
  }
}