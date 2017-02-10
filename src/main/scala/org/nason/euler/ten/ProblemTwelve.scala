package org.nason.euler.ten

import scala.collection.mutable.HashSet

object ProblemTwelve {
  def main(args: Array[String]) {
    println( solution3 )
  }
  
  val MAX = 500
  
  def allFactors( v:Int ) =
  {
    val max = Math.floor(Math sqrt( v )).toInt
    ( for ( i<-1 to max; if v%i==0 ) yield (i, v/i) )
      .foldLeft(List[Int]())( (a,b) => {
        if ( b._1 == b._2 )
          b._1 :: a
        else
          b._1 :: b._2 :: a 
        } )
  }
  
  def unique[A]( v:Iterable[A] ) =
  {
    val all = new HashSet[A]()
    v foreach ( all += _ )
    all.toList
  }
  
  def solution =
  {
    val i = ( 1 to 100000 ) map ( i => i*(i+1)/2 ) map (allFactors(_).length) takeWhile( _ <= MAX )
    val n = i.length + 1
    n * (n+1) / 2
  }
  
  /** horribly slow: don't use **/
  def solution2 =
  {
    def factorTriangle( n:Int ) = n%2 match
    {
      case 0 => unique( for (i<-allFactors(n/2); j<-allFactors(n+1)) yield i*j )
      case 1 => unique( for (i<-allFactors((n+1)/2); j<-allFactors(n)) yield i*j )
    }
    // factorTriangle(5)
    val i = ( 1 to 100000 ) map( factorTriangle(_).length ) takeWhile( _ <= MAX )
    val n = i.length + 1
    n * (n+1) / 2
  }
  
  /** Much faster, use this one **/
  def solution3 =
  {
    def nFactorsTriangle( n:Int ) = n%2 match
    {
      case 0 => allFactors(n/2).length * allFactors(n+1).length
      case 1 => allFactors((n+1)/2).length * allFactors(n).length
    }
    val i = ( 1 to 100000 ) map( nFactorsTriangle(_) ) takeWhile( _ <= MAX )
    val n = i.length + 1
    n * (n+1) / 2
  }
}