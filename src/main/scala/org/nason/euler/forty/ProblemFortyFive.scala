package org.nason.euler.forty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils


object ProblemFortyFive
{
  def main(args: Array[String])
  {
    println( solution2 )
  }
  
  // would work but inefficient
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
    def isPentagonal( p:Int ) =
    {
      val num = 1.0 + math.sqrt(24.0*p + 1.0)
      val root = num / 6.0
      root == math.floor(root)
    }
    def isHexagonal( h:Int ) =
    {
      val num = 1.0 + math.sqrt(8.0*h + 1.0)
      val root = num / 4.0
      root == math.floor(root)
    }
    def isSolution( i:Int ) = isTriangle(i) && isPentagonal(i) && isHexagonal(i)
    ( for( i <- 40756 to 200000000; if isSolution(i) ) yield i ) mkString ","
  }
  
  def solution2 =
  {
    def hexagonal( n:BigInt ) = n * ( n*2 - 1 )
    def isTriangle( x:BigInt ) =
    {
      // 1/2 * n * (n+1) = x
      // n^2 + n - 2x = 0
      val discr = Math.sqrt( (BigInt(1) + x*8) toDouble )
      val numer = 0.5 * ( discr - 1 )
      Math.floor(numer)==numer
    }
    def isPentagonal( p:BigInt ) =
    {
      val num = 1.0 + math.sqrt( (BigInt(1) + p*24) toDouble )
      val root = num / 6.0
      root == math.floor(root)
    }
    ( for( i<-1 to 50000; v=hexagonal(BigInt(i)); if isPentagonal(v)&&isTriangle(v) ) yield v ) mkString ","
  }

}