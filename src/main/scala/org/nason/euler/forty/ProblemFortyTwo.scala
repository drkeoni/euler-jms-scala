package org.nason.euler.forty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils


object ProblemFortyTwo {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  val DataPath = "/home/sorensjm/workspace/euler/data/problem42.txt"
  
  def solution =
  {
    def isTriangle( x:Int ) =
    {
      // 1/2 * n * (n+1) = x
      // n^2 + n - 2x = 0
      val discr = Math.sqrt( 1.0 + 8.0 * x )
      val numer = 0.5 * ( discr - 1 )
      Math.floor(numer)==numer
    }
    // slurp in file
    val lines = io.Source.fromFile(DataPath).getLines() toArray
    // split by ',' and trim quotes
    val values = (lines(0) split ",") map ( s => s.substring(1,s.length-1) )
    
    def wordToNumber( s:String ) = ( s map (c => c-'A'+1) ) reduceLeft(_+_)
    
    (for( v<-values; w=wordToNumber(v); if isTriangle(w) ) yield v).toArray.length
  }
}