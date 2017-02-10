package org.nason.euler.sixty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemSixty {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution3 ) }
  }
  
  def solution =
  {
    //val seed = Array( 3l, 7l, 109l, 673l )
    //val seed = Array( 3l, 7l, 109l, 29059l )
    //val seed = Array( 3l, 7l, 109l )
    //val seed = Array( 3l, 7l, 61l )
    //val seed = Array( 3l, 7l, 61l, 29059l )
    //val seed = Array( 3l, 7l )
    //val seed = Array( 3l )
    //val seed = Array( 3l, 11l )
    //val seed = Array( 3l, 11l, 23l )
    //val seed = Array( 3l, 11l, 23l, 8747l )
    //val seed = Array( 3l, 11l, 23l, 164663l )
    //val seed = Array( 7l )
    //val seed = Array( 7l, 19l )
    //val seed = Array( 7l, 19l, 97l )
    //val seed = Array( 7l, 19l, 433l )
    //val seed = Array( 7l, 19l, 97l, 3727l )
    //val seed = Array( 7l, 19l, 433l, 71143l )
    //val seed = Array( 7l, 19l, 433l, 82219l )
    //val seed = Array( 7l, 19l, 433l )
    //val seed = Array( 3l, 11l, 701l )
    //val seed = Array( 3l, 11l, 701l, 8747l )
    //val seed = Array( 3l, 11l, 701l, 53777l )
    //val seed = Array( 7l, 61l )
    //val seed = Array( 7l, 61l, 487l )
    //val seed = Array( 7l, 61l, 487l, 8941l )
    val seed = Array( 7l, 61l, 487l, 63601l )
    val MinPrime = (NumberUtils seqMax(seed)).get
    def isPrimeSet( v:Array[Long] ) =
    {
      val vs = v map (_.toString)
      def concatIsPrime( p:String, q:String ) = NumberUtils fastPrimeLong(BigInt(p+q).toLong)
      val l = ( for( i<-0 until v.length; j<-i+1 until v.length )
        yield concatIsPrime(vs(i),vs(j)) && concatIsPrime(vs(j),vs(i)) )
        .takeWhile(i=>i)
        .length
      l == (v.length * (v.length-1)) / 2
    }
    // only checks concatIsPrime for pairs containing the last element
    // (for use when the seed is a prime set)
    def isPrimeSet2( v:Array[Long] ) =
    {
      val vs = v map (_.toString)
      def concatIsPrime( p:String, q:String ) = NumberUtils fastPrimeLong(BigInt(p+q).toLong)
      val js = vs(vs.length-1)
      val l = ( for( i<-0 until (v.length-1) )
        yield concatIsPrime(vs(i),js) && concatIsPrime(js,vs(i)) )
        .takeWhile(i=>i)
        .length
      l == (v.length-1)
    }
    // infinite iterator that generates primes
    def primeGenerator : Iterator[Long] = new Iterator[Long]
    {
      val primes = NumberUtils.BigPrimeTable.filter(_>=MinPrime)
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
          while( !NumberUtils.fastPrimeLong(j) ) { j += 1 }
          j += 1
          j-1
        }
      }
    }
    // infinite iterator that generates mutually prime sets having seed s
    def primeSets( s:Array[Long] ) : Iterator[Array[Long]] = new Iterator[Array[Long]]
    {
      var j = 0
      val trial = ( s.iterator ++ Array(0l).iterator ).toArray
      val primes = primeGenerator
      //System.err.println( "trial %s".format(trial mkString ",") )
      def hasNext = true
      def next = 
      {
        // returns index of prime that produced a prime set and the prime set
        def _primeSets( v:Int ) : Tuple2[Int,Array[Long]] =
        {
          //System.err.println( "v = %d".format(v) )
          //System.err.println( "p0 = %d".format(Primes(v)) )
          trial(s.length) = primes.next
          System.err.println( "trial2 %s".format(trial mkString ",") )
          if ( isPrimeSet2(trial) )
            (v,trial)
          else
            _primeSets( v+1 )
        }
        val p = _primeSets(j)
        j = p._1 + 1
        p._2
      }
    }
    primeSets( seed ).take(1).map( _ mkString "," ) mkString ","
    //primeGenerator.take(10) mkString ","
  }
  
  // branch-and-bound solution
  def solution2 =
  {
    var best = 10000000l
    // only checks concatIsPrime for pairs containing the last element
    // (for use when the seed is a prime set)
    def isPrimeSet2( v:Array[Long] ) =
    {
      val vs = v map (_.toString)
      def concatIsPrime( p:String, q:String ) = NumberUtils fastPrimeLong(BigInt(p+q).toLong)
      val js = vs(vs.length-1)
      val l = ( for( i<-0 until (v.length-1) )
        yield concatIsPrime(vs(i),js) && concatIsPrime(js,vs(i)) )
        .takeWhile(i=>i)
        .length
      l == (v.length-1)
    }
    // infinite iterator that generates mutually prime sets having seed s
    def primeSets( s:Array[Long], maxSum:Long ) : Iterator[Array[Long]] = new Iterator[Array[Long]]
    {
      var j = 0
      val trial = ( s.iterator ++ Array(0l).iterator ).toArray
      var curSum = 0l
      val curValue = Array.fill(s.length+1)(0l)
      val primes = new Iterator[Long]
      {
        val minPrime = (NumberUtils seqMax(s)).get
        val primes = NumberUtils.BigPrimeTable.filter(_>=minPrime)
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
            while( !NumberUtils.fastPrimeLong(j) ) { j += 1 }
            j += 1
            j-1
          }
        }
      }
      
      //System.err.println( "trial %s".format(trial mkString ",") )
      def hasNext = true
      def next = 
      {
        // returns index of prime that produced a prime set and the prime set
        def _primeSets( v:Int ) : Tuple2[Int,Array[Long]] =
        {
          //System.err.println( "v = %d".format(v) )
          //System.err.println( "p0 = %d".format(Primes(v)) )
          trial(s.length) = primes.next
          System.err.println( "trial2 %s".format(trial mkString ",") )
          val sum = trial.reduceLeft(_+_)
          if ( isPrimeSet2(trial) )
            (v,trial)
          else
            _primeSets( v+1 )
        }
        val p = _primeSets(j)
        j = p._1 + 1
        curSum = p._2.reduceLeft(_+_)
        p._2
      }
    }
    // need to rewrite this to couple the prime set trial generation with
    // the b-and-b procedure
    def bb( s:Array[Long], best:Long, n:Int ) : Long =
    {
      if ( s.length==n )
      {
        val sum = s.reduceLeft(_+_)
        if ( sum < best )
          sum
        else
          best
      }
      else
      {
        var best2 = best
        for( ps <- primeSets(s,best).takeWhile( v => v.reduceLeft(_+_) < best ) )
        {
          val best3 = bb( ps, best2, n )
          System.err.println( "now best3=%d".format(best3) )
          System.exit(1)
          if ( best3 < best2 )
            best2 = best3
        }
        best2
      }
    }
    bb( Array(3l), 1000000l, 3 )
    /*
    def pv(v:Iterator[Array[Long]]) = v map( _ mkString "," ) mkString ";"
    for( p <- List( 3l, 7l, 11l ) )
    {
      println( pv( primeSets(Array(p)).take(3) ) )
    }
    */
  }
  
  def infinitePrimes( minPrime:Long ) = new Iterator[Long]
  {
    val primes = NumberUtils.BigPrimeTable.filter(_>=minPrime)
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
        while( !NumberUtils.fastPrimeLong(j) ) { j += 1 }
        j += 1
        j-1
      }
    }
  }
  
  def solution3 =
  {
    def isPrimeSet2( v:Array[Long] ) =
    {
      val vs = v map (_.toString)
      /*
      def concatIsPrime( p:String, q:String ) = {
        System.err.println( "testing p=%s and q=%s".format(p,q) )
        val isp = NumberUtils fastPrimeLong(BigInt(p+q).toLong)
        System.err.println( "found isp=%s".format(isp) )
        isp
      }
      */
      def concatIsPrime( p:String, q:String ) =
        NumberUtils fastPrimeLong(BigInt(p+q).toLong)
      val js = vs(vs.length-1)
      val l = ( for( i<-0 until (v.length-1) )
        yield concatIsPrime(vs(i),js) && concatIsPrime(js,vs(i)) )
        .takeWhile(i=>i)
        .length
      l == (v.length-1)
    }
    val n = 5
    def ps(s:Array[Long]) = s mkString ","
    def bb( s:Array[Long], best:Long ) : Option[Long] =
    {
      val sum = s.reduceLeft(_+_)
      val x = n - s.length
      val minBound = sum + x * ( s.last + x + 1 )
      System.err.println( ps(s) )
      System.err.println( minBound )
      if ( minBound >= best )
        return None
      if ( x==0 )
        return Some(sum)
      val s2 = Array.fill(s.length+1)(0l)
      Array.copy( s, 0, s2, 0, s.length )
      var newBest = best
      System.err.println( newBest )
      val l = infinitePrimes(s.last+1).takeWhile( p =>
        {
          //System.err.println( p )
          s2(s2.length-1) = p
          if ( isPrimeSet2(s2) )
          {
            System.err.println( "trying s2=" + ps(s2) )
            val newBest2 = bb( s2, newBest )
            if ( !newBest2.isDefined )
              false
            else
            {
              newBest = newBest2.get
              true
            }
          }
          else
          {
            val sum2 = sum + p
            if ( sum2>newBest )
              false
            else
              true
          }
        }
      )
      if ( l.length > 0 )
        Some(newBest)
      else
        None
    }
    //bb( List(7l).toArray, 76501 )
    //bb( List(11l).toArray, 34427 )
    //bb( List(13l).toArray, 34427 )
    //bb( List(17l).toArray, 26033 )
    bb( List(19l).toArray, 26033 )
    //isPrimeSet2( List(3l,5l).toArray )
    //NumberUtils.fastPrimeLong(35l)
  }
}