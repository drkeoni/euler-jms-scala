package org.nason.euler.fifty

import scala.annotation.tailrec

import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemFiftyEight {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution ) }
  }
  
  def solution =
  {
    def ne( n:Long ) = n*n - 3*n + 3
    def sw( n:Long ) = n*n - n + 1
    def nw( n:Long ) = n*n - 2*n + 2
    def allThree( n:Long ) = List( ne(n), sw(n), nw(n) )
    def PrimeFrac = 0.1
    @tailrec def count( i:Long, nDiag:Long, nPrime:Long ) : Long =
    {
      if ( i>3 && nPrime.toDouble/nDiag.toDouble < PrimeFrac )
        i-2
      else
      {
        val nDiag2 = nDiag + 4
        val nPrime2 = nPrime + allThree(i).filter(NumberUtils.fastPrimeLong).length
        count( i+2, nDiag2, nPrime2 )
      }
    }
    count( 3, 1, 0 )
  }
}