package org.nason.euler.zero

object ProblemFour {
  def main(args: Array[String]) {
    println( solution2 )
  }
  
  def solution =
  {
    def isPalindromic( v: Int ): Boolean =
    {
      val s = v.toString
      for( i <- 0 to s.length-1 )
      {
        if (s.charAt(i)!=s.charAt(s.length - i - 1))
        {
          return false
        }
      }
      return true
    }
    var max = 0
    for( i <- 0 to 999 )
    {
      for( j <- 0 to 999 )
      {
        val prod = i * j
        if (isPalindromic(prod))
        {
          if (prod>max)
          {
            max = prod
          }
        }
      }
    }
    max
  }
  
  def solution2 =
  {
    def isPalindromic( v: Int ): Boolean =
    {
      val s = v.toString
      for( i <- 0 to s.length-1 )
      {
        if (s.charAt(i)!=s.charAt(s.length - i - 1))
        {
          return false
        }
      }
      return true
    }
    // from http://stackoverflow.com/questions/1870736/scala-whats-the-best-way-to-do-numeric-operations-in-generic-classes
    def max[T <% Ordered[T]](i: Iterable[T]) = {
      if (i.isEmpty) None
      else Some(i.reduceLeft((a,b) => if (a > b) a else b))
    }
    val x = ( for( i<-(1 to 999); j<-(i to 999) ) yield i*j ) filter isPalindromic
    max(x)
  }
}