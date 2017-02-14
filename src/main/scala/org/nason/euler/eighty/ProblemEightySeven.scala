package org.nason.euler.eighty

import org.nason.euler.{EulerUtils, NumberUtils}

import scala.collection.mutable

/**
  * Created by jon on 2/13/17.
  */
object ProblemEightySeven {
  def main(args: Array[String]): Unit = {
    EulerUtils timing { println(solution) }
  }

  def solution = {

    val Max = 50000000
    val MaxPrime = Math.ceil( Math.pow(Max,0.25) ).toInt

    val triples = new mutable.HashSet[Int]
    val (_,primes) = NumberUtils.primes(MaxPrime)

    println( "Searching over " + (primes mkString ","))

    for( i <- primes ) {
      val max2 = Max - i*i*i*i
      if (max2>=0) {
        val max2prime = Math.ceil(Math.pow(max2, 0.33333333)).toInt
        val (_,jPrimes) = NumberUtils.primes(max2prime)
        for (j <- jPrimes) {
          val max3 = Max - i * i * i * i - j * j * j
          if (max3 >= 0) {
            val max3prime = Math.ceil(Math.sqrt(max3)).toInt
            val (_,kPrimes) = NumberUtils.primes(max3prime)
            for (k <- kPrimes) {
              val p = k * k + j * j * j + i * i * i * i
              if (p < Max)
                triples.add(p)
            }
          }
        }
      }
    }
    triples.size
  }
}
