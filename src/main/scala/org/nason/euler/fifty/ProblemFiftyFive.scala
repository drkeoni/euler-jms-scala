package org.nason.euler.fifty

import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemFiftyFive {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution ) }
  }
  
  def solution =
  {
    def isLychrel( v:Int ) =
    {
      def addReverse( x:BigInt ) = x + BigInt( x.toString.reverse )
      def _isLychrel( v:BigInt, i:Int ) : Boolean =
        if ( i>50 )
          true
        else if ( NumberUtils.isPalindrome(v.toString) )
          false
        else
          _isLychrel( addReverse(v), i+1 )
      _isLychrel( addReverse(v), 1)
    }
    ( for( i<-1 until 10000; if isLychrel(i) ) yield i ).length
  }
}