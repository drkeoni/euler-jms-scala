package org.nason.euler.forty


import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils


object ProblemFortySeven {
  def main(args: Array[String])
  {
    println( solution )
  }
  
  def solution =
  {
    val Window = 4
    def nDistinct[A]( v:List[A] ) = v.toSet.size
    val j = new NumberUtils.ChunkIterator( (1 to 1000000), Window )
    def nFactors( v:Int ) = nDistinct(NumberUtils factor v)
    def allFactors( v:Iterable[Int], n:Int ) = v.forall(nFactors(_)==n)
    ( j find ( allFactors(_,Window) )).head mkString ","
  }
}