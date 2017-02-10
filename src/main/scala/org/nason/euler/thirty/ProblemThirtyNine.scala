package org.nason.euler.thirty

import scala.collection.mutable.ListBuffer


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemThirtyNine {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution = 
  {
    def pyTriples( p:Int ) : List[(Int,Int,Int)] =
    {
      val buf = ListBuffer[(Int,Int,Int)]()
      for( a <- 1 to p; b <- a to p; c = p - a - b )
      {
        if ( c>a && c>b && ( c*c - a*a - b*b == 0 ) )
          buf += (( a, b, c ))
      }
      buf.toList
    }
    1 + ( NumberUtils argMax ( for( p <- 1 to 1000 ) yield pyTriples(p).length ) )
  }
}