package org.nason.euler.twenty

object ProblemTwentyFive {
  
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    val MAX_DIGIT = 1000
    val heap = Array.fill(10000)(0) map( i => BigInt(i) )
    heap(1) = 1
    heap(2) = 1
    var i = 2
    def digitLength( v:BigInt ) = v.toString.length
    while( digitLength(heap(i))<MAX_DIGIT )
    {
      heap(i+1) = heap(i) + heap(i-1)
      i += 1
    }
    i
  }
}