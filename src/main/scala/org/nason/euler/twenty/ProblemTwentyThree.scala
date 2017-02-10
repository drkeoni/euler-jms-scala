package org.nason.euler.twenty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils


object ProblemTwentyThree {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    def isAbundant( v:Int ) = ((NumberUtils allFactors(v)) reduceLeft(_+_)) > 2*v
    def isAbundantSum( v:Int ) = ( 1 to v-1 ) exists ( i => isAbundant(i) && isAbundant(v-i) )
    ( 1 to 28123 )
        .filter( !isAbundantSum(_) )
        .map( _.toLong )
        .reduceLeft( _+_ )
  }
}