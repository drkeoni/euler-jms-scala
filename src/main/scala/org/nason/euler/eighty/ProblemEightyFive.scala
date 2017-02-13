package org.nason.euler.eighty

import org.nason.euler.EulerUtils

/**
  * Created by jon on 2/13/17.
  */
object ProblemEightyFive {


  def main(args: Array[String]): Unit = {
    EulerUtils timing { println(solution) }
  }

  def solution = {
    // find closest integer solution (h,w) to
    // h(h+1)w(w+1) = 4 * 2000000

    var best = (100000,0,0)

    for( h <- 1 until 2000000 ) {
      val h2 = h * (h+1)
      val c = -4.0 * 2000000.0 / h2
      val b = 1.0
      val disc = b*b - 4.0*c
      var best0 = (100000,0)
      if ( disc > 0.0 ) {
        val w = 0.5 * (-b + Math.sqrt(disc))
        val w1 = Math.floor(w)
        val w2 = Math.ceil(w)
        val delta1 = Math.abs(h2 * w1 * (w1+1) - 4 * 2000000).toInt
        val delta2 = Math.abs(h2 * w2 * (w2+1) - 4 * 2000000).toInt
        if ( delta1 < delta2 ) {
          best0 = (delta1,w1.toInt)
        } else {
          best0 = (delta2,w2.toInt)
        }
        if (best0._1<best._1) {
          best = (best0._1,h,best0._2)
        }
      }

    }
    println( "best is (%d,%d,%d)".format(best._1,best._2,best._3) )
    ( best._2*best._3 ).toString
  }
}
