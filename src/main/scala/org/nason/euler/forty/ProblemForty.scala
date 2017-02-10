package org.nason.euler.forty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils


object ProblemForty {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    def d( n:Int ) =
    {
      var i = 0
      var l = 0
      while( l<n )
      {
        i += 1
        l += i.toString.length
      }
      val il = i.toString.length
      i.toString.substring( il - l + n - 1, il - l + n )
    }
    //((1 to 15) map d) mkString ","
    ( for( i <- List(1,10,100,1000,10000,100000,1000000) )
      yield Integer.parseInt(d(i)) )
      .reduceLeft(_*_)
  }
}