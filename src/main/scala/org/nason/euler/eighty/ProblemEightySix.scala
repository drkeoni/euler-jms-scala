package org.nason.euler.eighty

import org.nason.euler.EulerUtils

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by jon on 2/13/17.
  */
object ProblemEightySix {

  def main(args: Array[String]): Unit = {
    EulerUtils timing { println(solution) }
  }

  def solution = {

    /*
    Looking for integer solutions to cuboid shortest path
    using the sides.
    */

    def isInt(a:Double) = Math.abs(Math.floor(a)-a)<1.0e-8

    def dist( a:Double, b:Double, c:Double ) = {
      val x = (a+b)*(a+b)+c*c
      Math.sqrt(x)
    }

    def hasIntegerDist( h:Int, w:Int, d:Int ) = {
      val (hd,wd,dd) = (h.toDouble,w.toDouble,d.toDouble)
      val d1 = dist(hd,wd,dd)
      val d2 = dist(wd,hd,dd)
      val d3 = dist(wd,dd,hd)
      val dmin = Seq(d1,d2,d3).min
      isInt(dmin)
    }

    //println( "hasIntegerDist(3,5,6)=%s".format(hasIntegerDist(3,5,6)))

    val MaxIter = 10000
    val StopNumber = 1000000

    val numSolutions = Array.fill[Int](MaxIter+1)(0)
    var maxNum = 0
    var x = 1

    while( maxNum < StopNumber ) {
      var n = numSolutions(x - 1)
      for (j <- 1 to x; k <- 1 to j) {
        if (hasIntegerDist(x, j, k))
          n += 1
      }
      numSolutions(x) = n
      x += 1
      maxNum = n
    }

    x-1
  }

}
