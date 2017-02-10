package org.nason.euler.thirty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemThirtyEight {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution = 
  {
    // take all 9-digit pandigitals
    // take p:prefix (less than 333), and s:suffix
    // is 2*p a prefix of s?
    // if yes, s <- remove "2*p"
    // is 3*p a prefix of s?
    // etc...
    // ( NumberUtils permutations (List(1,2,3)) map(_ mkString "") ) mkString ","
    val Digits = List(1,2,3,4,5,6,7,8,9)
    val permutations = (NumberUtils permutations Digits) map(_ mkString "")
    def isCatProduct( v:String ) =
    {
      def startswith( s:String, p:String ) = s.length>=p.length && s.indexOf(p)==0
      ( 1 to 4 ) exists ( i => {
        val ( h, t ) = v.splitAt(i)
        def iter( t2:String, d:Int, n:Int ):Boolean =
        {
          val a = (d*n).toString
          if ( startswith( t2, a ) )
          {
            if ( a.length==t2.length )
              true
            else
              iter( t2.substring(a.length), d, n+1 )
          }
          else
            false
        }
        iter( t, Integer.parseInt(h), 2 ) 
      } )
    }
    
    ( permutations filter isCatProduct ) mkString ","
  }
}