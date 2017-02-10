package org.nason.euler.sixty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemSixtyOne {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution ) }
  }
  
  def solution =
  {
    val perms = NumberUtils permutations( List( 3, 4, 5, 6, 7, 8 ) )
    def fig3( n:Int ) = n*(n+1)/2
    def fig4( n:Int ) = n*n
    def fig5( n:Int ) = n*(3*n-1)/2
    def fig6( n:Int ) = n*(2*n-1)
    def fig7( n:Int ) = n*(5*n-3)/2
    def fig8( n:Int ) = n*(3*n-2)
    def tabulate( f:Int=>Int ) =
    {
      val nums = (( 0 to 200 ) map f) dropWhile(_<1000) takeWhile(_<10000)
      val table = Array.fill(10000)(false)
      nums.foreach( n=>table(n)=true )
      ( table, nums )
    }
    def nums = List(fig3 _,fig4 _,fig5 _,fig6 _,fig7 _,fig8 _) map tabulate
    def isCyclicPair( i:Int, j:Int ) = i.toString.substring(2,4) == j.toString.substring(0,2)
    for( p <- perms )
    {
      val p2 = p map (_-3)
      //System.err.println( "considering " + (p2 mkString ",") )
      def findSet( s:List[Int], i:Int ) : Unit =
      {
        if ( i==6 )
        {
          if ( isCyclicPair(s(5),s(0)) )
            System.err.println( s mkString "," )
        }
        else
        {
          for( v <- nums(p2(i))._2 )
          {
            if ( s.length==0 )
            {
              findSet( List(v), i+1 )
            }
            else
            {
              if ( isCyclicPair(v,s.head) )
                findSet( v :: s, i+1 )
            }
          }
        }
      }
      findSet( Nil, 0 )
    }
  }
}