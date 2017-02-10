package org.nason.euler.zero

object ProblemTwo {
  def main(args: Array[String]) {
    println( solution2 )
  }
  
  def solution =
  {
    def fib( i: Int ): Int =
    {
      if (i==0)
      {
        1
      }
      else if (i==1)
      {
        2
      }
      else
      {
        fib( i-1 ) + fib( i-2 )
      }
    }
    def even( i: Int) = i%2 == 0
    val sum = ( 1 to 37 ) map ( v => fib(v) ) filter ( v=>v<4000000 && even(v) ) reduceLeft( _+_ )
    sum
  }
  
  def solution2 =
  {
    def fib( i: Int ): Int = i match
    {
      case 0 => 1
      case 1 => 2
      case _ => fib( i-1 ) + fib( i-2 )
    }
    def even( i: Int) = i%2 == 0
    val sum = ( 0 to 37 ) map ( v => fib(v) ) filter ( v=>v<4000000 && even(v) ) reduceLeft( _+_ )
    sum
  }
}