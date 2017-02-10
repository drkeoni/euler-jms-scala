package org.nason.euler.zero

import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

object ProblemFive {
  def main(args: Array[String]) {
    println( solution )
  }
  
  // enumerate the factors of 1 ... 20
  // for each prime factor, record the maximum occurrence
  def solution =
  {
    def factor( a: BigInt ): List[BigInt] =
    {
      if ( a<BigInt(4) )
      {
        return List(a)
      }
      val art = BigInt( Math floor (Math sqrt( a toDouble )) toInt )
      var l = List[BigInt]()
      var i = art
      while( i >= 2 )
      {
        if ( a % i == 0 )
        {
          l = factor(i) ::: factor(a/i)
          i = 0
        }
        i -= 1
      }
      if ( l.length == 0 ) List(a)
      else l
    }
    def factorsToMap( factors : List[BigInt] ) =
    {
      val map = new HashMap[BigInt,Int]
      for( v <- factors )
      {
        if ( !map.contains(v) )
          map(v) = 1
        else
          map(v) += 1
      }
      map
    }
    def maxMap( maps : Iterable[Map[BigInt,Int]] ) =
    {
      val mp = new HashMap[BigInt,Int]
      for( m <- maps )
      {
        for( (k,v) <- m )
        {
          if ( !mp.contains(k) )
            mp(k) = v
          else
            if ( v>mp(k) )
              mp(k) = v
        }
      }
      mp
    }
    (for( (k,v) <- maxMap( (BigInt(1) to BigInt(20)) map factor map factorsToMap ) )
      yield k pow v) reduceLeft { (a,b) => a*b }
  }
}