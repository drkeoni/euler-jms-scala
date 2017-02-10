package org.nason.euler.fifty

import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemFiftySeven {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution ) }
  }
  
  def solution =
  {
    // This isn't used in the current solution, because it turns out you
    // don't need to reduce the fractions
    def reduce( p:BigInt, q:BigInt ) =
    {
      val max = (NumberUtils max( Math.sqrt(p.toDouble), Math.sqrt(q.toDouble) )).toInt
      def _reduce( a:BigInt, b:BigInt, pr:Int ) : Tuple2[BigInt,BigInt] =
        if ( pr>=max )
          Tuple2(a,b)
        else
          if ( NumberUtils.fastPrime(pr) )
          {
            if ( a%pr==0 && b%pr==0 )
              _reduce( a/pr, b/pr, pr )
            else
              _reduce( a, b, pr+1 )
          }
          else
            _reduce( a, b, pr+1 )
      _reduce( p, q, 2 )
    }
    val Max = 1000
    val Two = BigInt(2)
    //def nextIter( v:Tuple2[BigInt,BigInt] ) = reduce(v._1+Two*v._2,v._1+v._2)
    def nextIter( v:Tuple2[BigInt,BigInt] ) = Tuple2(v._1+Two*v._2,v._1+v._2)
    def continuation( v: List[Tuple2[BigInt,BigInt]], i:Int ) : List[Tuple2[BigInt,BigInt]] = (v,i) match
    {
      case (_,j) if j>=Max => v
      case (h::t,j) => continuation( nextIter(h)::v, j+1 )
      case (_,_) => Nil
    }
    def moreDigits( p:Any, q:Any ) = p.toString.length > q.toString.length
    ( continuation( List(Tuple2(3,2)), 1 ) filter( v => moreDigits(v._1,v._2) ) ).length
    //continuation( List(Tuple2(3,2)), 1 )
  }
}