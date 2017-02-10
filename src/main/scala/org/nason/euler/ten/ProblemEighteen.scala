package org.nason.euler.ten


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemEighteen {
  def main(args: Array[String]) 
  {
    println( solution )
  }
  
  val NUM_ROWS = 15
  
  def solution =
  {
    // unmarshal triangle string into ragged int array
    val values = (data2 split " ") map( Integer.parseInt(_) )
    val triangle = (0 to NUM_ROWS-1)
      .map( i => (i,i*(i+1)/2) )
      .map( i => (i._2 to i._2 + i._1) map (values(_)) toArray )
      .toArray
    // initialize f to copy of triangle
    val f = 
      ( for( i<-0 to NUM_ROWS-1 ) 
        yield (for( j<-0 to i ) yield triangle(i)(j)).toArray )
      .toArray
    // returns list of adjacent nodes ( row,col tuples )
    def adj( r:Int, c:Int ) = (r,c) match
    {
      case (_,0) => List( (r-1,0) )
      case _ => 
      {
        if ( r==c )
          List( (r-1,r-1) )
        else
          List( ( r-1, c-1 ), ( r-1, c ) )
      }
    }
    def max( v:Iterable[Int] ) = v reduceLeft((a,b) => if (a > b) a else b)
    for( i<-1 to NUM_ROWS-1; c<-0 to i )
    {
      // add the max value from the adjacent nodes
      f(i)(c) += max( adj(i,c) map ( p => f(p._1)(p._2) ) )
    }
    // answer is max value of the bottom row
    NumberUtils seqMax ( ( 0 to NUM_ROWS-1 ) map ( f(NUM_ROWS-1)(_) ) )
  }
  
  // 4 rows
  val data1 =
     "3" +
    " 7 4" +
    " 2 4 6" +
    " 8 5 9 3"
  
  // 15 rows
  val data2 =
     "75" +
    " 95 64" +
    " 17 47 82" +
    " 18 35 87 10" +
    " 20 04 82 47 65" +
    " 19 01 23 75 03 34" +
    " 88 02 77 73 07 63 67" +
    " 99 65 04 28 06 16 70 92" +
    " 41 41 26 56 83 40 80 70 33" +
    " 41 48 72 33 47 32 37 16 94 29" +
    " 53 71 44 65 25 43 91 52 97 51 14" +
    " 70 11 33 28 77 73 17 78 39 68 17 57" +
    " 91 71 52 38 17 14 91 43 58 50 27 29 48" +
    " 63 66 04 68 89 53 67 30 73 16 69 87 40 31" +
    " 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
}