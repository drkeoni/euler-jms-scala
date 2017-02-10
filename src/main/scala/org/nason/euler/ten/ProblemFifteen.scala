package org.nason.euler.ten

object ProblemFifteen {
  def main(args: Array[String]) {
    println( solution2 )
  }
  
  /** formally correct, too much memory **/
  def solution =
  {
    def g( r:Int, c:Int ) : Int = (r,c) match
    {
      case (0,c) => 1
      case (r,0) => 1
      case (r,c) => g(r-1,c) + g(r,c-1)
    }
    g(20,20)
  }
  
  def solution2 =
  {
    val f = Array.fill(21,21)(1l)
    for ( i <- 1 to 20; j <- 1 to 20 )
    {
      f(i)(j) = f(i-1)(j) + f(i)(j-1)
    }
    f(20)(20)
  }
}