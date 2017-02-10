package org.nason.euler.twenty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemTwentyOne {
  def main(args: Array[String]) 
  {
    println( solution )
  }
  
  def solution =
  {
    def d( i:Int ) =
    {
      NumberUtils.allFactors(i).reduceLeft(_+_) - i
    }
    def isAmicable( i:Int ) =
    {
      val a = d(i)
      d(a) == i && a!=i
    }
    ( (2 to 9999) filter isAmicable ) reduceLeft(_+_)
  }
}