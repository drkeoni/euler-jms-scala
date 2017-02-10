package org.nason.euler.zero

object ProblemOne {
  def main(args: Array[String]) {
    println( better1 )
  }
  
  def javaLike =
  {
    def multipleThree( v: Int ) = v % 3 == 0
    def multipleFive( v: Int ) = v % 5 == 0
    //println("4%3" + multipleThree(4) )
    var sum = 0
    for( i <- 1 to 999 )
    {
      if ( multipleThree(i) || multipleFive(i) )
      {
        sum += i
      }
    }
    sum
  }
  
  def better1 =
  {
    def multiple( v: Int, k: Int ) = v % k == 0
    def multipleThree( v: Int ) = multiple(v,3)
    def multipleFive( v: Int ) = multiple(v,5)
    val sum = (1 to 999) filter 
      (v => (multipleThree(v) || multipleFive(v))) reduceLeft( _+_ )
    sum
  }
}