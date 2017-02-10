package org.nason.euler.ten

object ProblemSixteen {
  def main(args: Array[String]) {
    println( solution2 )
  }
  
  val MAX = 1000
  
  /** never fully realized **/
  def solution =
  {
    // from http://en.wikipedia.org/wiki/Modular_exponentiation
    def modularPow( base:Int, exponent:Int, modulus:Int ) =
    {
      var result = 1
      var e = exponent
      var b = base
      while( e > 0 )
      {
        if ( (e & 1) == 1)
           result = (result * b) % modulus
        e = e >> 1
        b = (b * b) % modulus
      }
      result
    }
    modularPow( 2, 1000, 10000 )
  }
  
  def solution2 =
  {
    val a = BigInt(2) pow MAX
    (a.toString map( _.toInt - '0' )) reduceLeft(_+_)
  }
}