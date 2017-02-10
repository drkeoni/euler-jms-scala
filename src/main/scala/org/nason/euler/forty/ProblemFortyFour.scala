package org.nason.euler.forty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils


object ProblemFortyFour {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    val MaxN = 50000
    def pentagonal( n:BigInt ) = n*(BigInt(3)*n-1)/2
    val pentagonals = (1 to MaxN) map( i => pentagonal(BigInt(i)) )
    def isPentagonal( p:BigInt ) =
    {
      val num = 1.0 + math.sqrt( (BigInt(24)*p + BigInt(1)) toDouble )
      val root = num / 6.0
      root == math.floor(root)
    }
    def isPentagonalPair( i:BigInt, j:BigInt ) = isPentagonal(i+j) && isPentagonal( (i-j).abs )
    ( for( i<-1 until MaxN; j<-i until MaxN; 
        p1=pentagonals(i); p2=pentagonals(j); 
        if isPentagonalPair(p1,p2) ) yield (p1-p2).abs ) mkString ","
  }
}