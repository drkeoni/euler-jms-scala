package org.nason.euler.twenty

object ProblemTwentyEight {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    val MaxDim = 1003
    val Dim = 1001
    val Directions = Array( (0,1), (1,0), (0,-1), (-1,0) )
    val grid = Array.fill(MaxDim,MaxDim)(BigInt(0))
    
    class Ant( x:Int, y:Int )
    {
      def nEmpty = Directions map ( d => if (grid(x+d._1)(y+d._2)==0) 1; else 0 ) reduceLeft(_+_)
      
      def mark( i:Int ) = grid(x)(y) = i
      
      def move( dir:(Int,Int) ) = new Ant(x+dir._1,y+dir._2)
    }
    
    var ant = new Ant( (MaxDim-1)/2, (MaxDim-1)/2 )
    var dir = 3
    
    def turnRight( dir:Int ) = ( dir + 1 ) % 4
    
    for( i<-1 to Dim*Dim )
    {
      ant.mark(i)
      if ( ant.nEmpty>2 )
        dir = turnRight(dir)
      ant = ant.move( Directions(dir) )
    }
    
    def sumDiagonal( f: Int=>BigInt ) = ( 0 until MaxDim ) map ( f ) reduceLeft(_+_)
    
    sumDiagonal(i => grid(i)(i)) + sumDiagonal(i => grid(i)(MaxDim-i-1)) - 1
    
    /*
    def pad( a:Array[Int] ) = a map ( "%02d".format(_) )
    
    grid map ( v => pad(v) mkString " " ) mkString "\n"
    */
  }
}