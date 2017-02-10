package org.nason.euler.forty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils


object ProblemFortyEight {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    def powpow( i:Int ) = BigInt(i).pow(i)
    val sum = ( ( 1 to 1000 ) map powpow ) reduceLeft(_+_)
    val sSum = sum.toString
    sSum.substring(sSum.length-10, sSum.length)
  }
}