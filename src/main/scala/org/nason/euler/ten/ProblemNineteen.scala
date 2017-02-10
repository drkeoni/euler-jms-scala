package org.nason.euler.ten

object ProblemNineteen {
  def main(args: Array[String]) 
  {
    println( solution )
  }
  
  def solution =
  {
    def daysInMonth( m:Int, y:Int ) =
    {
      if (m==1)
      {
        if (y%100==0)
        {
          if (y%400==0)
            29
          else
            28
        }
        else if (y%4==0)
          29
        else
          29
      }
      // April, June, Sept, Nov
      else if (m==3 || m==5 || m==8 || m==10)
        30
      else
        31
    }
    
    var d = 0
    var nSundays = 0
    for( y <- 1900 to 2000; m<-0 to 11 )
    {
      d = d + daysInMonth(m,y)
      if ( d%7==6 && y>=1901 && !(y==2000 && m==11))
        nSundays += 1
    }
    nSundays
    
    //( for( y <- 1900 to 2000; m<-0 to 11 ) yield daysInMonth(m,y) ) toList
  }
}