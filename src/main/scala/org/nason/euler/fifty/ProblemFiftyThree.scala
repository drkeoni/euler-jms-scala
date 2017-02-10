package org.nason.euler.fifty

import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemFiftyThree {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution ) }
  }
  
  def solution =
  {
    // generate binomial coefficients using Pascal's triangle
    val v = Array.fill(101,101)(BigInt(0))
    v(2)(0) = 1
    v(2)(1) = 2
    v(2)(2) = 1
    val MaxBin = 100
    for( i <- 3 to MaxBin )
    {
      v(i)(0) = 1
      for( j<-1 to i-1)
      {
        v(i)(j) = v(i-1)(j-1) + v(i-1)(j)
      }
      v(i)(i) = 1
    }
    ( for( i<-0 to MaxBin; j<-0 to i; if v(i)(j)>1000000) 
      yield v(i)(j) )
      .length
      //yield "%d,%d,%d".format(i,j,v(i)(j)) )
      //.mkString("\n")
  }
}