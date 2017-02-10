package org.nason.euler.thirty

object ProblemThirtyTwo {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    val MAX_PERM = 400000
    val heap = Array.fill(MAX_PERM,9)(0.toByte)
    val DIGITS = List(1,2,3,4,5,6,7,8,9).map(_.toByte)
    
    def factorial( a:Int ) : Int = if (a==0) 1; else a * factorial(a-1)
    val fCache = (0 to 10).map(factorial).toArray
    
    def permutations( v:List[Byte], i:Int, offset:Int ) : Unit =
    {
      if ( !v.isEmpty )
      {
        val nperm = fCache(v.length-1)
        v.zipWithIndex.foreach ( j=> {
          ( 0 until nperm ) foreach ( k => heap( k+j._2*nperm+offset )(i) = j._1 )
          permutations( v filter (_!=j._1), i+1, j._2*nperm + offset )
        } )
      }
    }
    
    permutations(DIGITS,0,0)
    
    // logic dictates that the product has exactly 4 digits
    def makeTriple( v:Array[Byte], i:Int ) =
    {
      def bytes2int( a:Array[Byte] ) = Integer.parseInt(a mkString "")
      val product = bytes2int( v.takeRight(4) )
      val a = v.dropRight(4).splitAt(i)
      ( bytes2int(a._1), bytes2int(a._2), product )
    }
    
    def isProduct( a:(Int,Int,Int) ) = a._1 * a._2 == a._3
    
    def ordered( a:(Int,Int,Int) ) = a._1 <= a._2
    
    // loop over all permutations of {1..9} and all partitions of 5 digits on the LHS
    ( for( i <- 1 to 4; j <- 0 until fCache(9); t=makeTriple(heap(j),i); if isProduct(t) && ordered(t) )
        yield t._3 )
        .distinct
        .reduceLeft(_+_)
  }
}