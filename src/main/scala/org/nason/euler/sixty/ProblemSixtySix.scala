package org.nason.euler.sixty

import scala.collection.mutable.ListBuffer
import org.nason.euler.NumberUtils
import org.nason.euler.EulerUtils

object ProblemSixtySix {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution2 ) }
  }
  
  /**
   * formally correct, but won't work due to speed and representation limitations
   */
  def solution =
  {
    def isInt( d:Double ) = Math.floor(d)==d
    def isPerfectSquare( i:BigInt ) = isInt(Math.sqrt(i.toDouble))
    def findMin( d:Int ) : BigInt =
    {
      if (isPerfectSquare(d))
        return 0
      val ans = NumberUtils.from(BigInt(2))(i=>i+1)
        .filter( i => (i+1)%d==0 || (i-1)%d==0 )
        .map( i => i*i-1 )
        .filter( a => a%d==0 && isPerfectSquare( a / d ) )
        .head
      BigInt(Math.sqrt(ans.toDouble+1).toLong)
    }
    //(for(i<-1 to 20) yield "%d:%d".format(i,findMin(i))) mkString "\n"
    for( i<-1 to 100 )
    {
      System.err.println( "%d:%d".format(i,findMin(i)) )
    }
  }
  
  /**
   * http://en.wikipedia.org/wiki/Pell%27s_equation
   * Fundamental solution via continued fractions
     Let  denote the sequence of convergents to the continued fraction for . 
     Then the pair (x1,y1) solving Pell's equation and 
     minimizing x satisfies x1 = hi and y1 = ki for some i. 
     This pair is called the fundamental solution. Thus, the fundamental 
     solution may be found by performing the continued fraction expansion and 
     testing each successive convergent until a solution to Pell's equation is found.
     As Lenstra (2002) describes, the time for finding the 
     fundamental solution using the continued 
     fraction method, with the aid of the Schönhage–Strassen 
     algorithm for fast integer multiplication, is within a logarithmic 
     factor of the solution size, the number of digits in the pair (x1,y1). 
     However, this is not a polynomial time algorithm because the 
     number of digits in the solution may be as large as √n, far 
     larger than a polynomial in the number of digits in the input value n (Lenstra 2002).
   */
  def solution2 =
  {
    /** provides continued fraction expansion for sqrt(i) **/
    def contFrac( i:Int ) : List[Int] =
    {
      val iSqrt = Math.floor(Math.sqrt(i))
      if ( iSqrt*iSqrt==i )
        return (Nil)
      def next( a:Tuple4[Int,Int,Int,Int] ) = Tuple3(a._2,a._3,a._4)
      def step( a:Tuple3[Int,Int,Int] ) =
      {
        val v1 = a._1 
        val q = i - (a._2*a._2)
        val (p1, q1) = NumberUtils reduce( a._3, q )
        val v2 = Math.floor( (-a._2+iSqrt) / q1.toDouble ).toInt
        val v4 = q1.toInt
        val v3 = (-v2 * v4 - a._2).toInt
        (v1,v2,v3,v4)
      }
      var u = step( ( 0,0,1 ) )
      u = step(next(u))
      u = step(next(u))
      val w = ListBuffer[Int]()
      for( j <- 0 until 2000 )
      {
        u = step(next(u))
        w += u._1
      }
      w.toList
    }
    /** Calculates rational fraction for mth convergent of sqrt(n) **/
    def mthConvergent( m:Int, n:Int, series:List[Int] ) =
    {
      val iSqrt = Math.floor(Math.sqrt(n)).toInt
      val series2 = series.take(m-1).toList.reverse
      def frac( i:Int, v:Tuple2[BigInt,BigInt] ) : Tuple2[BigInt,BigInt] =
        if ( i==series2.length )
          v
        else
          frac( i+1, ( v._2, v._2 * series2(i) + v._1 ) )
      val b = frac(0,(BigInt(0),BigInt(1)))
      ( b._2 * iSqrt + b._1, b._2 )
    }
    /** True if sol (x,y) is a solution to Pell's equation (x^2-n*y^2=1) **/
    def isPell( sol:Tuple2[BigInt,BigInt], n:Int ) = sol._1*sol._1==sol._2*sol._2*n + 1
    def pellSolution(n:Int) = {
      val series = contFrac(n)
      (1 to 2000).iterator.map( mthConvergent(_,n,series) ).find( isPell(_,n) )
    }
    val N = 1000
    1 + NumberUtils.argMax( (1 to N) map( x => { val s=pellSolution(x); if (s.isDefined) s.get._1; else BigInt(0)} ) )
  }
}