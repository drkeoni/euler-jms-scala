package org.nason.euler.twenty

import java.math.{BigDecimal,MathContext,RoundingMode}


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemTwentySix {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  val One = new BigDecimal(1.0)
  val BIGDECIMAL = new MathContext( 2048, RoundingMode.HALF_UP )
    
  def solution =
  {
    def reciprocal( v:Int ) = One.divide(new BigDecimal(v),BIGDECIMAL)
    def bool2int( a:Boolean ) = if (a) 1; else 0
    def autocorr( s:String ) =
    {
      for( d <- 0 until s.length-1 )
        yield
        ( ( 0 until s.length-d )
          .map( i => bool2int(s.charAt(i) equals s.charAt(i+d)) )
          .reduceLeft(_+_) ) / (s.length-d).toDouble
    }
    def cycleLength( v:BigDecimal ) = 
    {
      val seq = autocorr( v.toString.substring(2) )
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
    
    ( for( d <- 2 until 1000 )
      yield "%d: %d".format( d, cycleLength( reciprocal(d) ) ) )
      .mkString("\n")
  }

}