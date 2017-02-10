package org.nason.euler.twenty

//import scala.util.continuations._

import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemTwentyFour 
{
  def main(args: Array[String])
  {
    println( solution3 )
  }
  
  // permutations
  // formally correct, too much memory
  def solution =
  {
    def permutations( v:List[Int] ) : Stream[Stream[Int]] =
      v match
    {
      case Nil => Stream.cons( Stream.Empty, Stream.Empty )
      case _ => ( for( i<-v; t<-permutations( v filter (_!=i) ) ) yield i #:: t ) toStream
    }
    //val DIGITS = List(0,1,2)
    //val N = 1
    val DIGITS = List(0,1,2,3,4,5,6,7,8,9)
    val N = 10
    ( permutations(DIGITS).zipWithIndex dropWhile(v => v._2<N-1) take(1) )
      .toList
      .map( i => NumberUtils.strJoin(",",(i._1).map(_.toString)) )
  }
  
  // permutations
  def solution2 =
  {
    def permutations( v:Array[Byte] ) : List[List[Byte]] =
    {
      if ( v.length==1 )
        List(List(v(0)))
      else
        for( i<-v.toList; t<-permutations( v filter (_!=i) ) ) yield i :: t
    }
    //val DIGITS = List(0,1,2).map(_.toByte) toArray
    //val N = 1
    val DIGITS = List(0,1,2,3,4,5,6,7,8,9).map(_.toByte) toArray
    val N = 10
    //( permutations(DIGITS).zipWithIndex dropWhile(v => v._2<N-1) take(1) )
    //  .toList
    //  .map( i => i._1 mkString "," )
    ( permutations(DIGITS).view(N-1,N) )
      .toList
      .map( i => i mkString "," )
  }
  
  // permutations
  def solution3 =
  {
    val MAX_PERM = 5000000
    val heap = Array.fill(MAX_PERM,10)(0.toByte)
    //val DIGITS = List(0,1,2).map(_.toByte)
    //val N = 1
    val DIGITS = List(0,1,2,3,4,5,6,7,8,9).map(_.toByte)
    val N = 1000000
    
    def factorial( a:Int ) : Int = if (a==0) 1; else a * factorial(a-1)
    val fCache = (0 to 10).map(factorial).toArray
    
    def permutations( v:List[Byte], i:Int, offset:Int ) : Unit =
    {
      if (v.length>0)
      {
        val nperm = fCache(v.length-1)
        v.zipWithIndex.foreach ( j=> {
          ( 0 until nperm ) foreach ( k => heap( k+j._2*nperm+offset )(i) = j._1 )
          // ( j._2*nperm until (j._2+1)*nperm ) foreach ( k => heap(k+offset)(i)=j._1 )
          permutations( v filter (_!=j._1), i+1, j._2*nperm + offset )
        } )
      }
    }
    
    permutations(DIGITS,0,0)
    
    heap(N-1).take(DIGITS.length) mkString ","
  }
}