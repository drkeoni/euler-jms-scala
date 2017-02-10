package org.nason.euler.zero


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemSeven {
  val STOP = 10001
    
  def main(args: Array[String]) {
    println( solution2 )
  }
  
  def solution =
  {
    var i = 0
    var j = 2
    while( i<STOP )
    {
      val f = NumberUtils.factor(j)
      if ( f.length == 1 )
        i += 1
      j += 1
    }
    j-1
  }
  
  def solution2 =
  {
    def isPrime( v:Int ) =
    {
      (NumberUtils factor v length) == 1
    }
    var i = 0
    var j = 2
    while( i<STOP )
    {
      if ( isPrime(j) )
        i += 1
      j += 1
    }
    j-1
  }
}