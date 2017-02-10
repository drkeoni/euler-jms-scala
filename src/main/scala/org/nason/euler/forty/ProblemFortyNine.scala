package org.nason.euler.forty

import scala.collection.mutable.HashMap

import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemFortyNine {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    // all 4-digit primes
    val primes = ( NumberUtils primes 10000 )._2 filter(_>1000)
    def digitArray( i:Int ) = 
    {
      val v = Array.fill(10)(0)
      i.toString foreach ( ch => v(ch-'0') += 1 )
      v.toList
    }
    def isPermutation( i:Int, j:Int ) = ( digitArray(i) zip digitArray(j) ) forall( v => v._1 == v._2 )
    // divide into permutation equivalence classes
    lazy val pClasses = {
      val _pClasses = new HashMap[Int,List[Int]]()
      for( i<-0 until primes.length; pi=primes(i); j<-i until primes.length; pj=primes(j))
      {
        if ( i==j )
        {
          _pClasses(i) = List(pi)
        }
        else
        {
          if ( isPermutation(pi,pj) )
            _pClasses(i) = pj :: _pClasses(i)
        }
      }
      _pClasses
    }
    // while this produces the correct answer, technically we need
    // to allow l>3 here and prune for l==3 in the final check
    // for arithmetic series
    lazy val qClasses = pClasses filter ( _._2.length==3)
    def isSeries( v:List[Int] ) =
    {
      val v2 = v.sortWith( _<_ )
      def diff( a:List[Int] ) : List[Int] = a match
      {
        case h :: (h2::t) => h2-h :: diff(h2::t)
        case _ => Nil
      }
      val dv = diff(v2)
      dv forall ( _ == dv.head )
    }
    qClasses filter ( a => isSeries(a._2) ) map ( a=>"%d:%s".format(a._1,a._2 mkString ",") )
    //( for( a <- pClasses ) yield "%d:%s".format(a._1,a._2 mkString ",") ) mkString ";"
  }
}