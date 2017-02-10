package org.nason.euler.zero

object ProblemThree {
  def main(args: Array[String]) {
    solution foreach println
  }
  
  def solution =
  {
    def factor( a: BigInt ): List[BigInt] =
    {
      if ( a<BigInt(4) )
      {
        return List(a)
      }
      val art = BigInt( Math floor (Math sqrt( a toDouble )) toInt )
      //println( art )
      var l = List[BigInt]()
      var i = art
      while( i >= 2 )
      {
        if ( a % i == 0 )
        {
          l = factor(i) ::: factor(a/i)
          i = 0
        }
        i -= 1
      }
      if ( l.length == 0 )
      {
        List(a)
      }
      else
      {
        l
      }
    }
    //factor( BigInt("13195") )
    factor( BigInt("600851475143") )
    //factor( BigInt( "10000" ) )
  }
}