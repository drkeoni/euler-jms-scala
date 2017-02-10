package org.nason.euler.fifty

import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemFiftyTwo {
  def main(args: Array[String])
  {
    EulerUtils timing( { println( solution ) } )
  }
  
  def solution =
  {
    def sameDigits( i:Int, j:Int ) =
    {
      val is = i.toString.toList.sortWith(_<_)
      val js = j.toString.toList.sortWith(_<_)
      (is zip js).forall( v => v._1 ==v._2 )
    }
    def allSameDigits( v:Array[Int] ) =
    {
      (0 until v.length-1).forall( i => sameDigits( v(i), v(i+1) ) ) 
    }
    ( for( i <- 1 to 1000000 ) yield Array( i, 2*i, 3*i, 4*i, 5*i, 6*i ) )
      .filter(allSameDigits)
      .map( _(0) )
      .mkString( "," )
  }
}