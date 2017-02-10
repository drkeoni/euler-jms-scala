package org.nason.euler.forty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils


object ProblemFortyOne {
  def main(args: Array[String])
  {
    println( solution2 )
  }
  
  /** formally correct, too much memory **/
  def solution =
  {
    def isPanDigital( v:Int ) =
    {
      val digits = v.toString.map(_ - '0').toList
      val digitSet = digits.toSet.toList.sortWith(_<_)
      digitSet.length==digits.length && digitSet.head==1 && digitSet.takeRight(1)(0)==digits.length
    }
    //( for( i <- 1 to 100 ) yield "%d: %s".format(i,isPanDigital(i)) ) mkString ","
    val primes = NumberUtils.primes( 1000000000 )._2
    
    NumberUtils.seqMax( primes filter(isPanDigital) )
  }
  
  /** correct and quite fast **/
  def solution2 =
  {
    def byteArray2int( a:Array[Byte] ) = Integer.parseInt(a mkString "") 
    def pandigitalPrimes( n:Int ) = 
    {
      val p = NumberUtils.permutations( ( 1 to n ).toList )
      def isNotMultipleTwo( v:Array[Byte] ) = v(v.length-1)%2==1
      def isPrime( v:Array[Byte] ) = NumberUtils.fastPrime( byteArray2int(v) )
      p.filter(isNotMultipleTwo).filter(isPrime)
    }
    //for( i<-1 to 20000; if fastPrime(i) ) {println(i)}
    //pandigitalPrimes(4).map(_ mkString "") mkString ","
    //( for( i<-4 to 9; j<-pandigitalPrimes(i) ) yield j ).map(_ mkString "") mkString ","
    NumberUtils.seqMax( for( i<-4 to 9; j<-pandigitalPrimes(i) ) yield byteArray2int(j) )
  }
}