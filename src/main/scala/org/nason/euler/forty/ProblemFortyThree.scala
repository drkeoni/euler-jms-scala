package org.nason.euler.forty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils


object ProblemFortyThree {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    val pandigitals = NumberUtils permutations List(0,1,2,3,4,5,6,7,8,9)
    def fastCheck( v:Array[Byte] ) = v(3)%2==0 && v(5)%5==0
    val primes = NumberUtils.primes(18)._2
    def slowCheck( v:Array[Byte] ) =
    {
      def toInt( v:Array[Byte] ) = 100*v(0) + 10*v(1) + v(2)
      val divisible = (for( i<-1 to 7 ) yield (toInt( v.slice(i, i+3) )%primes(i-1)==0))
      divisible.forall(a=>a)
    }
    def bigToInt( v:Array[Byte] ) = BigInt( v mkString "" )
    (for( p<-pandigitals; if fastCheck(p) && slowCheck(p) ) yield bigToInt(p)).reduceLeft(_+_)
  }
}