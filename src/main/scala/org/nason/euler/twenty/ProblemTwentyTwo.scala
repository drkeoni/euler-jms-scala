package org.nason.euler.twenty

import scala.util.Sorting
object ProblemTwentyTwo {
  
  def main(args: Array[String]) {
    println( solution )
  }
  
  def solution =
  {
    val lines = io.Source.fromFile(dataPath).getLines() toArray
    // split comma-separated values and strip "
    val values = (lines(0) split ",") map ( s => s.substring(1,s.length-1) )
    Sorting.quickSort(values)
    def nameToValue( s:String ) = { s map (_.toInt - 'A' + 1) reduceLeft(_+_) }
    (values map ( nameToValue(_) ) zipWithIndex)
       .map( i => ((i._2+1) * i._1).toLong )
       .reduceLeft( _+_ )
  }
  
  val dataPath = "/home/sorensjm/workspace/euler/data/problem22.txt"
}