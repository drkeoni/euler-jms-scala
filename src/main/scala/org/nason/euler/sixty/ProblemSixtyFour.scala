package org.nason.euler.sixty

import scala.collection.mutable.ListBuffer
import org.nason.euler.NumberUtils
import org.nason.euler.EulerUtils

object ProblemSixtyFour {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution2 ) }
  }
  
  def solution =
  {
    def p( a:Tuple4[Int,Int,Int,Int] ) = "(%d,%d,%d,%d)".format(a._1,a._2,a._3,a._4)
    def contFrac( i:Int ) : List[Int] =
    {
      val iSqrt = Math.floor(Math.sqrt(i))
      if ( iSqrt*iSqrt==i )
        return (Nil)
      def next( a:Tuple4[Int,Int,Int,Int] ) = Tuple3(a._2,a._3,a._4)
      def step( a:Tuple3[Int,Int,Int] ) =
      {
        val v1 = a._1 
        val q = i - (a._2*a._2)
        val (p1, q1) = NumberUtils reduce( a._3, q )
        val v2 = Math.floor( (-a._2+iSqrt) / q1.toDouble ).toInt
        val v4 = q1.toInt
        val v3 = (-v2 * v4 - a._2).toInt
        (v1,v2,v3,v4)
      }
      var u = step( ( 0,0,1 ) )
      u = step(next(u))
      u = step(next(u))
      val w = ListBuffer[Int]()
      for( j <- 0 until 2000 )
      {
        u = step(next(u))
        //System.err.println( p(u) )
        w += u._1
      }
      w.toList
    }
    def bool2int( a:Boolean ) = if (a) 1; else 0
    def autocorr( s:String ) =
      for( d <- 0 until s.length-1 )
        yield
        ( ( 0 until s.length-d )
          .map( i => bool2int(s(i)==s(i+d)) )
          .reduceLeft(_+_) ) / (s.length-d).toDouble
    def cycleLength( v:String ) = 
    {
      val seq = autocorr( v )
      if ( seq.length==0 )
        0
      else
      {    
        val seq2 = seq.tail
        if ( seq2.length==0 )
          0
        else
          ( NumberUtils argMax seq2 ) + 1
      }
    }
    def isOddCycle( a:Int ) = (a>0) && (a%2==1)
    //( ( 1 to 13 ).map(contFrac).map( _ mkString "," ) ) mkString "\n"
    ( 1 to 10000 ).map(contFrac).map(_ mkString "").map(cycleLength).count(isOddCycle)
  }
  
  def solution2 =
  {
    def p( a:Tuple4[Int,Int,Int,Int] ) = "(%d,%d,%d,%d)".format(a._1,a._2,a._3,a._4)
    def contFrac( i:Int ) : Int =
    {
      val iSqrt = Math.floor(Math.sqrt(i))
      if ( iSqrt*iSqrt==i )
        return 0
      def next( a:Tuple4[Int,Int,Int,Int] ) = Tuple3(a._2,a._3,a._4)
      def isEqual( a:Tuple4[Int,Int,Int,Int], b:Tuple4[Int,Int,Int,Int] ) =
        (a._1==b._1)&&(a._2==b._2)&&(a._3==b._3)&&(a._4==b._4)
      def step( a:Tuple3[Int,Int,Int] ) =
      {
        val v1 = a._1 
        val q = i - (a._2*a._2)
        val (p1, q1) = NumberUtils reduce( a._3, q )
        val v2 = Math.floor( (-a._2+iSqrt) / q1.toDouble ).toInt
        val v4 = q1.toInt
        val v3 = (-v2 * v4 - a._2).toInt
        (v1,v2,v3,v4)
      }
      var u = step( ( 0,0,1 ) )
      u = step(next(u))
      u = step(next(u))
      val u2 = step(next(u))
      u = step(next(u))
      for( j <- 0 until 2000 )
      {
        u = step(next(u)) 
        if ( isEqual(u,u2) ) 
          return ( j + 1 )
      }
      return (2001)
    }
    def isOddCycle( a:Int ) = (a>0) && (a%2==1)
    ( 1 to 10000 ).map(contFrac).count(isOddCycle)
  }
}