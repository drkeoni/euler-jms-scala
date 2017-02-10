package org.nason.euler.twenty

object ProblemTwenty {
  def main(args: Array[String]) 
  {
    println( solution2 )
  }
  
  def solution =
  {
    def factorial( n:BigInt ) : BigInt =
    {
      if (n==1)
        BigInt(1)
      else
        n * factorial(n-1)
    }
    ( factorial(100).toString map ( _.toInt - '0' ) ) reduceLeft(_+_)
  }
  
  def solution2 =
  {
    def factorial( n:Int ) = (BigInt(1) to BigInt(n)) reduceLeft(_*_)
    ( factorial(100).toString map ( _.toInt - '0' ) ) reduceLeft(_+_)
  }
}