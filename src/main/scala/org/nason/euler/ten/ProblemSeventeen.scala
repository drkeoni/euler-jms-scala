package org.nason.euler.ten

object ProblemSeventeen {
  def main(args: Array[String]) 
  {
    println( solution )
  }
  
  def solution =
  {
    def intToEnglish( v:Int ) : String =
    {
      if (v<10)
        ONES(v)
      else if (v<20)
        TEENS(v-10)
      else
      {
        val one = v % 10
        val ten = ((v-one)/10) % 10
        if (v<100)
          TENS(ten) + " " + ONES(one)
        else if ( v<1000 )
        {
          val hundred = ((v - 10*ten - one) / 100) % 10
          if ( ten==0 && one==0 )
            ONES(hundred) + " hundred"
          else
            ONES(hundred) + " hundred and " + intToEnglish( 10*ten + one )
        }
        else if ( v==1000 )
          "one thousand"
        else
          ""
      }
    }
    //List( 2, 17, 18, 22, 53, 115, 215, 234, 301, 342, 500, 1000 ) map( intToEnglish(_) )
    def letterCount( s:String ) = s.foldLeft(0)( (a,b) => if (Character isLetter b) a+1; else a )
    ( 1 to 1000 )
      .map ( intToEnglish(_) )
      .map ( letterCount(_) )
      .reduceLeft( _+_ )
  }
  
  val ONES = Map ( 
        0 -> "",
        1 -> "one",
        2 -> "two",
        3 -> "three",
        4 -> "four",
        5 -> "five",
        6 -> "six",
        7 -> "seven",
        8 -> "eight",
        9 -> "nine"
        )
        
  val TEENS = Map (
        0 -> "ten",
        1 -> "eleven",
        2 -> "twelve",
        3 -> "thirteen",
        4 -> "fourteen",
        5 -> "fifteen",
        6 -> "sixteen",
        7 -> "seventeen",
        8 -> "eighteen",
        9 -> "nineteen"
        )
        
  val TENS = Map (
        2 -> "twenty",
        3 -> "thirty",
        4 -> "forty",
        5 -> "fifty",
        6 -> "sixty",
        7 -> "seventy",
        8 -> "eighty",
        9 -> "ninety",
        10 -> "hundred"
        )
}