package org.nason.euler.sixty

import org.nason.euler.EulerUtils

object ProblemSixtyThree {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution ) }
  }
  
  def solution =
  {
    def nDigits( b:BigInt ) = b.toString.length
    def count2( n:Int ) =
    {
      val min = Math.ceil( Math.pow(10,(n-1)/n) ).toLong
      val max = Math.floor( 10.0 * Math.pow(10,1/n) ).toLong
      ( min to max ) map( BigInt(_).pow(n) ) count( nDigits(_)==n )
      //System.err.println( v mkString "," )
    }
    ( ( 1 to 21 ) map count2 ) reduceLeft(_+_)
  }
}