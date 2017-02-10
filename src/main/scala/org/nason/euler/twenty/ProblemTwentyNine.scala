package org.nason.euler.twenty

object ProblemTwentyNine {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    val Max = 100
    ( for( i <- 2 to Max; j <- 2 to Max ) yield BigInt(i).pow(j) )
      .distinct
      .length
  }
}