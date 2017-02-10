package org.nason.euler

object PrimeBenchmarking 
{
  def main(args: Array[String])
  {
    solution(isPrime)
    solution(NumberUtils.fastPrimeLong)
    val funcs = List[Long=>Boolean]( isPrime, NumberUtils.fastPrimeLong, fastPrime )
    for( i<-0 until 2; pc<-funcs )
    {
      EulerUtils timing { println( solution(pc) ) }
    }
  }
  
  def isPrime( v:Long ) =
  {
    true
  }
  
  lazy val MaxPrime = NumberUtils.BigPrimeTable.last
  
  def fastPrime( v:Long ):Boolean =
  {
    // for all primes p, p mod 6 = 1 or 5
    val v6 = v%6
    if (v6!=1 && v6!=5) return false
    val maxp = Math.sqrt(v.toDouble).toInt
    for( p<-(NumberUtils.BigPrimeTable.iterator takeWhile(_<maxp)); if v%p==0 )
    {
      if (v==p)
        return true
      else
        return false
    }
    // only need to check numbers between max(BigPrimeTable) and maxp
    for( i<-MaxPrime+1 to maxp; i6=i%6; if i6==1 || i6==5 )
    {
      if (v%i==0)
        return false
    }
    true
  }
  
  def solution( primeCheck : Long=>Boolean ) =
  {
    val min = 1000000l
    val max = min + 10000
    //(for( i <- min to max ) yield "%d: %s".format(i,primeCheck(i))) mkString "\n"
    ( (min to max) filter primeCheck ) length
  }
}