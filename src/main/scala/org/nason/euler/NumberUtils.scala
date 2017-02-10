package org.nason.euler

import scala.concurrent.Future
import scala.concurrent.Future._

object NumberUtils
{
  /** returns list of prime factors **/
  def factor( a: BigInt ): List[BigInt] =
  {
    if ( a<BigInt(4) )
    {
      return List(a)
    }
    val art = BigInt( Math floor (Math sqrt( a toDouble )) toInt )
    var l = List[BigInt]()
    var i = art
    while( i >= 2 )
    {
      if ( a % i == 0 )
      {
        l = factor(i) ::: factor(a/i)
        i = 0
      }
      i -= 1
    }
    if ( l.length == 0 ) List(a)
    else l
  }
  
  /** returns list of all factors **/
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
  
  def allBigFactors( v:BigInt ) =
  {
    
    val max = BigInt( Math.floor(Math sqrt( v.toDouble )).toLong )
    ( for ( i<-BigInt(1) to max; if v%i==0 ) yield (i, v/i) )
      .foldLeft(List[BigInt]())( (a,b) => {
        if ( b._1 == b._2 )
          b._1 :: a
        else
          b._1 :: b._2 :: a 
        } )
  }
  
  // from http://stackoverflow.com/questions/1870736/scala-whats-the-best-way-to-do-numeric-operations-in-generic-classes
  def seqMax[T <% Ordered[T]](i: Iterable[T]) = {
    if (i.isEmpty) None
    else Some(i.reduceLeft((a,b) => if (a > b) a else b))
  }
  
  def max[A <% Ordered[A]](a:A,b:A) = if (a > b) a else b
  
  def isPrime( v:Int ) = (NumberUtils factor v length) == 1
  def isPrimeLong( v:Long ) = (NumberUtils factor v length) == 1
  
  /** implements python's string.join() ... whoops mkString is the way to go!! **/
  def strJoin( delimiter:String, values:Iterable[String] ) =
  {
    values.foldLeft(""){ (a,b) => 
      a match { case "" => b; case _ => a + delimiter + b } }
  }
  
  /** returns position of maximum element in ds **/
  def argMax[A <% Ordered[A]](ds: Iterable[A]): Int = 
    ds.zipWithIndex
      .reduceLeft((p1,p2) => if(p2._1 > p1._1) p2 else p1) 
      ._2
      
  /** Returns tuple (boolean table from 0 until maxPrime, sequence of primes) **/
  def primes( maxPrime:Int ) =
  {
    val heap = Array.fill(maxPrime)(true)
    // implements sieve of Erastothenes
    def sieve() =
    {
      val MaxFactor = math.sqrt(heap.length).toInt
      
      def cancel( f:Int ) : Int =
      {
        // start at the first prime equal to or larger than f
        //val i = f + heap.view(f,heap.length).takeWhile( b => b==false ).length
        var i = f
        while( !heap(i) && i<heap.length ) { i+=1 }
        // strike out all of the multiples
        /*
        val f1 = future { for( j <- i+i until maxPrime by 2*i ) { heap(j) = false } }
        val f2 = future { for( j <- i+i+i until maxPrime by 2*i ) { heap(j) = false } }
        f1()
        f2()
        */
        for( j <- i+i until maxPrime by i ) { heap(j) = false }
        i+1
      }
      
      def cancelAll( n:Int ) : Unit = if ( n<=MaxFactor ) cancelAll( cancel(n) )
      cancelAll(2)
    }
    
    sieve()
    ( heap, heap.view(2,heap.length).zipWithIndex.filter(_._1).map(_._2 + 2) )
  }
  
  /** Returns Array[Int][Byte] of all n! permutations **/
  def permutations( digits:List[Int] ) =
  {
    def factorial( a:Int ) : Int = if (a==0) 1; else a * factorial(a-1)
    val fCache = (0 to 10).map(factorial).toArray
    val MAX_PERM = fCache(digits.length)
    val heap = Array.fill(MAX_PERM,digits.length)(0.toByte)
    val DIGITS = digits.map(_.toByte)
    
    def perms( v:List[Byte], i:Int, offset:Int ) : Unit =
    {
      if ( !v.isEmpty )
      {
        val nperm = fCache(v.length-1)
        v.zipWithIndex.foreach ( j=> {
          ( 0 until nperm ) foreach ( k => heap( k+j._2*nperm+offset )(i) = j._1 )
          perms( v filter (_!=j._1), i+1, j._2*nperm + offset )
        } )
      }
    }
    
    perms(DIGITS,0,0)
    heap
  }
  
  class Memoize1[-T, +R](f: T => R) extends (T => R) 
  {
    import scala.collection.mutable
    private[this] val vals = mutable.Map.empty[T, R]

    def apply(x: T): R = vals getOrElseUpdate( x, f(x) )
  }

  object Memoize {
    /**
    * Memoize a unary (single-argument) function.
    *
    * @param f the unary function to memoize
    */
    def memoize[T, R](f: T => R): (T => R) = new Memoize1(f)
   
    /**
    * Fixed-point combinator (for memoizing recursive functions).
    */
    def Y[T, R](f: (T => R) => T => R): (T => R) = {
      lazy val yf: (T => R) = memoize(f(yf)(_))
      yf
    }
  }
  
  // all primes less than 20000
  lazy val PrimeTable = NumberUtils.primes(20000)._2
  lazy val BigPrimeTable =
  {
    val PrimeFile = "/home/sorensjm/workspace/euler/data/primes1000000.txt"
    (io.Source.fromFile(PrimeFile).getLines() map Integer.parseInt) toArray
  }
  /** relatively fast prime checker */
  def fastPrime( v:Int ):Boolean =
  {
    val maxp = Math.sqrt(v.toDouble).toInt
    for( p<-(BigPrimeTable.iterator takeWhile(_<maxp)); if v%p==0 )
    {
      if (v==p)
        return true
      else
        return false
    }
    isPrime(v)
  }
  
  lazy val MaxPrime = BigPrimeTable.last
  
  def fastPrimeLong( v:Long ):Boolean =
  {
    // for all primes p, p mod 6 = 1 or 5
    val v6 = v%6
    if (v6!=1 && v6!=5) return false
    val maxp = Math.sqrt(v.toDouble).toInt
    for( p<-(BigPrimeTable.iterator takeWhile(_ <= maxp)); if v%p==0 )
    {
      if (v==p)
        return true
      else
        return false
    }
    // only need to check numbers between max(BigPrimeTable) and maxp
    for( i<-MaxPrime+1 to maxp; i6=i%6; if i6==1 || i6==5 )
    {
      if (v%i==0)
        return false
    }
    true
  }
  
  /** Infinite generator of primes equal to or larger than minPrime **/
  def infinitePrimes( minPrime:Long ) = new Iterator[Long]
  {
    val primes = BigPrimeTable.filter(_>=minPrime)
    var i = 0
    var j = primes.last+1
    def hasNext = true
    def next =
    {
      if ( i<primes.length )
      {
        i += 1
        primes(i-1)
      } 
      else
      {
        while( !fastPrimeLong(j) ) { j += 1 }
        j += 1
        j-1
      }
    }
  }
  
  /**
   * Wraps an iterable and returns an iterable over sliding windows from 
   * the wrapped sequence.
   */
  class ChunkIterator( i:Iterable[Int], n:Int ) extends Iterable[Array[Int]]
  {
    var j = 0
    val ii = i.iterator
    var _hasNext = true
    val curr = Array.fill(n)(0)
    
    load()
    
    private def load() =
    {
      if (j==n)
      {
        if (!ii.hasNext)
        {
          _hasNext = false
        }
        else
        {
          for( k<-0 until n-1 ) { curr(k) = curr(k+1) }
          curr(n-1) = ii.next
          if (!ii.hasNext)
            _hasNext = false
        }
      }
      else
      {
        while(j<n && ii.hasNext)
        {
          curr(j) = ii.next
          j += 1
        } 
        if ( j<n ) _hasNext=false
      }
    }
    
    def iterator = new Iterator[Array[Int]] 
    {
      def hasNext = _hasNext
      def next = { load(); curr }
    }
  }
  
  /** returns true if s is a palindrome */
  def isPalindrome( s:String ) =
  {
    val sl = s.length
    ( 0 to sl/2 ) map( i => s(i)==s(sl-i-1) ) forall( i => i )
  }
  
  def toSet[T](list: List[T]) = {
    def traverse(list: List[T])(set: Set[T]): Set[T] = list match {
      case hd :: tail => traverse(tail)(set + hd)   // create a new Set, adding hd
      case Nil => set
    }
 
    traverse(list)(Set[T]())
  }
  
  def from[A](n:A)(incr:A=>A) : Stream[A] = Stream.cons( n, from(incr(n))(incr) )
  
  /** Reduces a fraction p/q to it's lowest representation. **/
  def reduce( p:BigInt, q:BigInt ) : Tuple2[BigInt,BigInt] =
  {
    if ( q % p == 0 )
    {
      return ( BigInt(1), q / p )
    }
    val _max = max( Math.sqrt(p.toDouble), Math.sqrt(q.toDouble) ).toInt
    def _reduce( a:BigInt, b:BigInt, pr:Int ) : Tuple2[BigInt,BigInt] =
      if ( pr>=_max )
        Tuple2(a,b)
      else
        if ( fastPrimeLong(pr) )
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
}