package org.nason.euler.fifty

import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemFiftySix {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution ) }
  }
  
  def solution =
  {
    def sumDigits( v:BigInt ) = v.toString.map( i=> BigInt(i-'0') ).reduceLeft(_+_)
    NumberUtils seqMax ( for( i<-1 until 100; j<-1 until 100 ) 
      yield sumDigits(BigInt(i).pow(j)) )
  }
}