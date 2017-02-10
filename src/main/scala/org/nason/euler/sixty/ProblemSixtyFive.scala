package org.nason.euler.sixty

import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemSixtyFive {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution ) }
  }
  
  def from(n: Int): Stream[Int] = Stream.cons( n, from(n+1) )
  
  def solution =
  {
    def series = (for( i <- from(1) ) yield List(1,2*i,1)).flatten
    val N = 100
    val series2 = series.take(N-1).toList.reverse
    def frac( i:Int, v:Tuple2[BigInt,BigInt] ) : Tuple2[BigInt,BigInt] =
      if ( i==series2.length )
        v
      else
        frac( i+1, ( v._2, v._2 * series2(i) + v._1 ) )
    val b = frac(0,(BigInt(0),BigInt(1)))
    val c = ( b._2 * 2 + b._1, b._2 )
    // sum of digits
    (c._1.toString map ( _ - '0' )).reduceLeft(_+_)
  }
}