package org.nason.euler.zero

object ProblemNine {
  def main(args: Array[String]) {
    println( solution )
  }
  
  def solution =
  {
    def isTriple( a:Int, b:Int, c:Int ) =
    {
      c * c - a * a - b * b == 0
    }
    ( for(
        a <- 1 to 1000;
        b <- a to 1000;
        c <- b to 1000;
        if isTriple(a,b,c) && a+b+c==1000
        )
      yield a*b*c
    )
  }
}