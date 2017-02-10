package org.nason.euler.thirty

import scala.collection.mutable.ListBuffer
object ProblemThirtyThree {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    def filterFirst[A]( v:Iterable[A], f:A=>Boolean ) =
    {
      val ( h, t ) = v span f
      if ( t.isEmpty )
        h
      else if ( h.isEmpty )
        t.tail
      else
        h ++ t.tail
    }
    
    def improperCancel( n:Int, d:Int ) =
    {
      def str2digit( a:String ) = if (a.isEmpty) 1 else Integer.parseInt(a)
      def hasDigit( a:String, digit:Int ) = a.indexWhere( _ == digit+'0' ) >= 0
      def cancel( a:String, digit:Int ) = 
        filterFirst( a, {c:Char => c-'0' != digit} ) mkString ""
      def cancel2( a:(String,String), digit:Int ) =
        if ( hasDigit(a._1,digit) && hasDigit(a._2,digit) )
          ( cancel(a._1,digit), cancel(a._2, digit) )
        else
          a
      val (ns,ds) = ( 1 to 9 ).foldLeft( (n.toString,d.toString) )( 
          (s, d) => cancel2( s, d ) )
      (str2digit(ns),str2digit(ds))
    }
    
    def isCurious( i:Int, j:Int ) =
    {
      val f = i.toFloat / j.toFloat
      val (n,d) = improperCancel( i, j )
      val f2 = n.toFloat / d.toFloat
      (i!=n) && (j!=d) && (f==f2)
    }
    
    ( for( i <- 10 to 99; j <- i+1 to 99; if isCurious(i,j) ) yield (i,j) )
      .reduceLeft( (v,x) => ( v._1*x._1, v._2*x._2 ) )
  }
}