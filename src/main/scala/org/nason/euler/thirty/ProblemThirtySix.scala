package org.nason.euler.thirty

object ProblemThirtySix {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    val Max = 1000000
    
    def isPalindrome( s:String ) =
    {
      val sl = s.length
      ( 0 to sl/2 ) map( i => s(i)==s(sl-i-1) ) forall( i => i )
    }
    
    def binary( i:Int ) =
    {
      def bit( v:Int ) = if ( (v & 1) == 1 ) '1' else '0'
      def bits( i:Int ) : List[Char] = i match
      {
        case 0 => Nil
        case _ => bit(i) :: bits( i>>1 )
      }
      bits(i).reverse mkString ""
    }
    
    ( for( i <- 1 to Max; if isPalindrome(i.toString) && isPalindrome(binary(i)) )
      yield i )
      .reduceLeft(_+_)
    
    //( for( i <- 0 to 100 ) yield (i,isPalindrome(i.toString)) ) mkString " "
  }
}