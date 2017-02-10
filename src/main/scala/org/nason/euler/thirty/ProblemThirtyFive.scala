package org.nason.euler.thirty

object ProblemThirtyFive {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    val MaxPrime = 1000000
    //val MaxPrime = 100
    val heap = Array.fill(MaxPrime)(true)
    def sieve() =
    {
      val MaxFactor = math.sqrt(heap.length).toInt
      
      def cancel( f:Int ) : Int =
      {
        // start at the first prime equal to or larger than f
        val i = f + heap.view(f,heap.length).takeWhile( b => b==false ).length
        // strike out all of the multiples
        for( j <- i+i until MaxPrime by i ) { heap(j) = false }
        i+1
      }
      
      def cancelAll( n:Int ) : Unit =
      {
        if ( n<=MaxFactor )
        {
          cancelAll( cancel(n) )
        }
      }
      
      cancelAll(2)
    }
    
    sieve()
    
    def rotations( v:Int ) =
    {
      def rotInt( a:Int ) = {
        val as = a.toString
        val al = as.length
        Integer.parseInt( as.substring(al-1,al) + as.substring(0,al-1) )
      }
      def rot( v:Int, i:Int ): List[Int] = i match
      {
        case 0 => Nil
        case _ => v :: rot(rotInt(v),i-1)
      }
      rot( v, v.toString.length )
    }
    
    val allPrimes = heap.view(2,heap.length).zipWithIndex.filter(_._1).map(_._2 + 2)
    
    // for each prime p, if all of the circular permutations are prime, yield p and count how many
    ( for( p <- allPrimes; if rotations(p).forall(heap) )
      yield p )
      .foldLeft(0)( (s,v) => s + 1 )
    
    //heap.take(30) mkString ","
    //heap.take(30).zipWithIndex.filter(_._1).map(_._2) mkString ","
  }
}