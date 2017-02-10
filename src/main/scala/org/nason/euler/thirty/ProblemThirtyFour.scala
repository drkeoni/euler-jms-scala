package org.nason.euler.thirty

object ProblemThirtyFour {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    def factorial( a:Int ):Int = if ( a==0 ) 1; else a * factorial(a-1)
    val fCache = (0 to 9) map (factorial) toArray
    def sumFact( v:Int ) = ( v.toString map( _ - '0' ) map(fCache) ).reduceLeft(_+_)
    ( for( i <- 3 to 10000000; if i==sumFact(i) )
      yield i )
      .reduceLeft(_+_)
  }
}