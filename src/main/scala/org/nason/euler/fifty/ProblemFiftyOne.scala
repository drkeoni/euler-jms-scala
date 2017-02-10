package org.nason.euler.fifty

import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemFiftyOne {
  def main(args: Array[String])
  {
    EulerUtils timing( { println( solution ) } )
  }
  
  def solution : Int =
  {
    def toIndices( pattern:Int ) = 
    {
      def toIndices2( pattern:Int, i:Int ) : List[Int] = pattern match
      {
        case 0 => Nil
        case 1 => List(i)
        case d if ( (d&1) == 1 ) => i :: toIndices2(pattern>>1,i+1)
        case _ => toIndices2(pattern>>1,i+1)
      }
      toIndices2(pattern,0)
    }
    val patterns = ((0 until 64) map toIndices ) toArray
    // returns the integer formed by substituting digit into value
    // at all locations in pattern
    def subDigit( value:Int, pattern:Int, digit:Int ) =
    {
      val chars = value.toString.toArray
      val d = (digit+'0').toChar
      patterns(pattern).foreach( chars(_)=d )
      val i = Integer.parseInt( chars mkString "" )
      if ( i.toString.length<chars.length )
        -1
      else
        i
    }
    // returns true if value has the same digit at all of the locations in pattern
    def matchesPattern( value:Int, pattern:Int ) = 
    {
      val p = patterns(pattern)
      val chars = value.toString.toArray
      val ch = chars(p.head)
      p.forall( chars(_)==ch )
    }
    val ( isPrimesBig, primesBig ) = NumberUtils primes 1000000
    for( p<-primesBig )
    {
      val numDigits = p.toString.length
      val maxPattern = (1 << (numDigits-1)) - 1
      if (maxPattern>0)
      {
        // find maximum number of equivalent primes by 
        // scanning across possible substitution patterns
        // and possible digits to substitute
        val maxEquivalent = NumberUtils seqMax (
          for( j<-1 to maxPattern; if matchesPattern(p,j) )
            yield ( (0 to 9)
                .map(subDigit(p,j,_))
                .filter( i => i>0 && isPrimesBig(i) )
                .length)
        )
        if ( maxEquivalent.isDefined && maxEquivalent.get==8 )
        {
          //println( "%d:%d".format(p,maxEquivalent.get) )
          return( p )
        }
      }
    }
    return( 0 )
  }
}