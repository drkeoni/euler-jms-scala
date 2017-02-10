package org.nason.euler.thirty

object ProblemThirtyOne {
  def main(args: Array[String])
  {
    println( solution2 )
  }
 
  class Memoize1[-T, +R](f: T => R) extends (T => R) 
  {
    import scala.collection.mutable
    private[this] val vals = mutable.Map.empty[T, R]

    def apply(x: T): R = vals getOrElseUpdate( x, f(x) )
  }

  object Memoize {
    /**
    * Memoize a unary (single-argument) function.
    *
    * @param f the unary function to memoize
    */
    def memoize[T, R](f: T => R): (T => R) = new Memoize1(f)
   
    /**
    * Fixed-point combinator (for memoizing recursive functions).
    */
    def Y[T, R](f: (T => R) => T => R): (T => R) = {
      lazy val yf: (T => R) = memoize(f(yf)(_))
      yf
    }
  }
  
  class MemoizeInt1[+R](f: Int => R, initMax:Int ) extends (Int => R) 
  {
    private[this] val vals = (0 to initMax) map f

    def apply(x: Int): R = if ( x <= initMax ) vals(x); else f(x)
  }
  
  object MemoizeInt1 {
    def apply[R]( f:(Int=>R), initMax:Int ) = new MemoizeInt1(f,initMax)
    
    // doesn't work yet
    def Y[R](f: (Int, Int => R) => R, initMax:Int ) = {
      def yf: (Int=>R) = MemoizeInt1( f(_, yf(_)), initMax )
      yf
    }
  }
  
  /*
   * correct answer would have 2:2 3:2 4:3 5:4 6:5 7:6 8:7 9:8 10:11 11:12
   */
  // only correct up to 10
  def solution =
  {
    val Coins = Array( 1, 2, 5, 10, 20, 50, 100, 200 )
    
    val isCoin = Memoize.memoize( (c:Int) => Coins.contains(c) )
    val isPure = Memoize.memoize( (v:Int) => if (isCoin(v)) 1 else 0 )
    def areCoins( c1:Int, c2:Int ) = if ( isCoin(c1) && isCoin(c2) ) 1 else 0
    
    val numPureComb: Int=>BigInt = {
      def numPureCombRec( f:Int=>BigInt )( v:Int ) : BigInt = 
      {
        (Coins.tail takeWhile( _ <= v/2 ) map ( i => BigInt(areCoins(i,v-i)) + f( v-i ) )).foldLeft(BigInt(0))(_+_)
      }
      Memoize.Y(numPureCombRec)
    }
    
    def change( target:Int ) : BigInt = target match
    {
      case 0 => 0
      case 1 => 1
      case _ => 
      {
        BigInt(isPure(target)) + numPureComb(target) + change(target-1)
      }
    }
    
    for( i <- 2 to 200 ) { numPureComb(i) }
    
    //( for( i <- 2 to 11 ) yield "%d:%d".format(i,numPureComb(i)) ) mkString " "
    ( for( i <- 2 to 11 ) yield "%d:%d".format(i,change(i)) ) mkString " "
    //change(200)
  }
  
  def solution2 =
  {
    val Coins = Array( 1, 2, 5, 10, 20, 50, 100, 200 )
    
    def change( v:Int, maxCoin:Int ):BigInt = (v,maxCoin) match
    {
      case (i,_) if i<0 => 0
      case (0,_) => 1
      case (_,1) => 1
      case _ => (Coins takeWhile ( _ <= maxCoin ) map ( c => change(v-c,c) )).foldLeft(BigInt(0))(_+_)    
    }
    
    //( for( i <- 2 to 11 ) yield "%d:%d".format(i,change(i,i)) ) mkString " "
    
    change(200,200)
  }
}