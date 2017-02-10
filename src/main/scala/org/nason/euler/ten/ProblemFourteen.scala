package org.nason.euler.ten

import scala.collection.mutable.HashMap


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemFourteen {
  def main(args: Array[String]) {
    println( solution4 )
  }
  
  /*
   * The first three solutions appear formally correct but all result in
   * stack overflows
   */
  
  def solution =
  {
    def f( n:Int ) : List[Int] =
    {
      if (n==1)
        List(1)
      else if (n%2==0)
        n :: f(n/2)
      else
        n :: f(3*n+1)
    }
    f(500)
    //NumberUtils seqMax( (1 to 999999) map ( f(_).length ) )
  }
  
  def solution2 =
  {
    val fLengths = new HashMap[Int,Int]()
    fLengths(1) = 1
    def f( n:Int ) : Int =
    {
      if ( fLengths contains n )
        fLengths(n)
      else if (n%2==0)
      {
        val v = f(n/2)
        fLengths(n/2) = v
        1 + v
      }
      else
      {
        val v = f(3*n+1)
        fLengths(3*n+1) = v
        1 + v
      }
    }
    NumberUtils seqMax( (1 to 999999) map ( f(_) ) )
  }
  
  def solution3 =
  {
    def f( n:Int ) : Int =
    {
      if ( n==1 )
        1
      else if (n%2==0)
      {
        1 + f(n/2)
      }
      else
      {
        1 + f(3*n+1)
      }
    }
    //f(13)
    NumberUtils seqMax( (1 to 999999) map ( f(_) ) )
  }
  
  /** iterative rewrite **/
  def solution4 =
  {
    // memoize function lookup
    val fLengths = new HashMap[BigInt,Int]()
    fLengths(1) = 1
    def f( n:BigInt ) =
    {
      var g = n
      var l = 1
      while( g!=1 )
      {
        if ( fLengths contains g )
        {
          l += fLengths(g) - 1
          g = 1
        }
        else
        {
          (g%2).toInt match
          {
            case 0 => g = g/2
            case 1 => g = BigInt(3)*g + BigInt(1)
          }
          l += 1
        }
      }
      fLengths(n) = l
      l
    }
    NumberUtils seqMax( (1 to 999999) map ( i => (f(i),i) ) )
  }
}