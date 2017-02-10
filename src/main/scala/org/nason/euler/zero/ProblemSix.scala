package org.nason.euler.zero

object ProblemSix
{
  def main(args: Array[String]) {
    println( solution )
  }
  
  def solution =
  {
    ( (for ( i<-1 to 100; j<-1 to 100; if (i!=j) ) yield i*j) 
        reduceLeft {(a,b)=>a+b} )
  }
}