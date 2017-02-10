package org.nason.euler.thirty

object ProblemThirty {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    val fifths = (0 to 9) map ( i => i*i*i*i*i )
    def sumFifths( i:Int ) = i.toString map( c => fifths(c-'0') ) reduceLeft(_+_)
    def isSumFifths( i:Int ) = i == sumFifths(i)
    ( for( i <- 2 to 5000000; val j=isSumFifths(i); if j ) yield i ) reduceLeft(_+_)
  }
}