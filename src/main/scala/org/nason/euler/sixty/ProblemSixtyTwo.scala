package org.nason.euler.sixty

import org.nason.euler.NumberUtils
import org.nason.euler.EulerUtils

object ProblemSixtyTwo {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution ) }
  }
  
  class Fingerprint( b:BigInt )
  {
    val value = b
    val digits =
    {
      val d = Array.fill(10)(0)
      b.toString.foreach( i => d(i-'0') += 1 )
      d
    }
    override def equals( o:Any ) = o match
    {
      case that:Fingerprint => ( this.digits zip that.digits ).forall( v => v._1 ==v._2 )
      case _ => false
    }
    override def toString = value.toString
  }
  
  def solution =
  {
    def nDigitCubes( n:Int ) = 
    {
      def min = Math.ceil(Math.pow(Math.pow(10,n-1),1.0/3.0)).toLong
      def max = Math.floor(Math.pow(Math.pow(10,n)-1.0,1.0/3.0)).toLong
      for( i<-min to max; b=BigInt(i) ) yield b*b*b
    }
    val TargetLength = 5
    def longestChain( n:Int ) =
    {
      val fingers = (nDigitCubes(n) map( new Fingerprint(_) )).toArray
      def chainLength( i:Int ) : Int = 
      {
        val fingeri = fingers(i)
        val length = ( ( i+1 until fingers.length ) map (fingers(_)) ).filter( fingeri==_ ).length + 1
        if ( length==TargetLength )
        {
          val s = ( ( i+1 until fingers.length ) map (fingers(_)) ).filter( fingeri==_ ).mkString(",")
          System.err.println( fingers(i).toString + "," + s )
        }
        length
      }
      NumberUtils.seqMax( (0 until fingers.length) map chainLength ).get
    }
    Iterator.from(5).takeWhile(longestChain(_)!=TargetLength) mkString ","
    //nDigitCubes(6) mkString ","
    /*
    val a = new Fingerprint( BigInt( 32242 ) )
    val b = new Fingerprint( BigInt( 24223 ) )
    a == b
    */
  }
}