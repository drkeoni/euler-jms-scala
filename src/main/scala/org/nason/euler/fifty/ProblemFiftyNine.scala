package org.nason.euler.fifty

import java.io.File

import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemFiftyNine {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution ) }
  }
  
  val dataPath = "/home/sorensjm/workspace/euler/data/problem59.txt"
  val outPath = "/home/sorensjm/workspace/euler/data/out59.txt"
    
  def solution =
  {
    val lines = io.Source.fromFile(dataPath).getLines() toArray
    // split comma-separated values and convert to Int
    val values = lines(0) split "," map Integer.parseInt
    val Min = 'a'.toInt
    val Max = 'z'.toInt
    // cycles through the values of v indefinitely
    def cyclic[A]( v:Array[A] ) : Iterator[A] = new Iterator[A]
    {
      var i = 0
      def hasNext = true
      def next =
        if (i<v.length)
        {
          i = i + 1
          v(i-1)
        }
        else
        {
          i = 1
          v(0)
        }
    }
    def decode( v:Array[Int], key:Array[Int] ) =
    {
      new String( ( ( v.iterator zip cyclic(key) ) map ( t2 => (t2._1 ^ t2._2).toByte ) ).toArray )
    }
    /*
    val s =
      ( for( ci<-Min to Max; cj<-Min to Max; ck<-Min to Max )
        yield "(%d,%d,%d) %s".format(ci,cj,ck,decode(values, Array(ci,cj,ck)) ) )
        .mkString("\n")
    EulerUtils.writeFile( new File(outPath), s)
    // by inspecting the resulting output it can be seen that the key is 103,111,100
    */
    val key = Array(103,111,100)
    decode( values, key ) map( _.toInt ) reduceLeft(_+_)
  }
}