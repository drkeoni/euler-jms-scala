package org.nason.euler.fifty

import org.nason.euler.EulerUtils
import org.nason.euler.NumberUtils

object ProblemFiftyFour {
  def main(args: Array[String])
  {
    EulerUtils timing { println( solution ) }
  }
  
  val DataPath = "/home/sorensjm/workspace/euler/data/problem54.txt"
  //                        11111
  //                2345678901234
  val FaceValues = "23456789TJQKA"  
    
  val HighCard = 1
  val OnePair = 2
  val TwoPair = 3
  val ThreeOfKind = 4
  val Straight = 5
  val Flush = 6
  val FullHouse = 7
  val FourOfKind = 8
  val StraightFlush = 9
  val RoyalFlush = 10
    
  class Card( repr:String ) extends Ordered[Card]
  {
    val value = faceToValue(repr.substring(0,1))
    val suit = repr.substring(1,2)
    
    private def faceToValue( a:String ) = FaceValues.indexOf(a)+2
    override def toString = FaceValues(value-2) + suit
    def compare(that:Card) = this.value - that.value
  }
  
  class HandRank( private val v : List[Int] ) extends Ordered[HandRank]
  {
    def compare(that:HandRank) =
    {
      val a = (this.v zip that.v) map { case (i,j) => i-j } find(_!=0)
      if (a.isDefined)
        a.get
      else
        0
    }
    
    override def toString = this.v mkString ","
  }
  
  object HandRank
  {
    def apply( xs:Int* ) = new HandRank(xs.toList)
  }
  
  class Hand( cardsList:List[Card] )
  {
    val cards = cardsList.sortWith(_<_).toArray
    override def toString = cards mkString " "
    
    private lazy val isStraight =
      ( (1 until cards.length) map ( i => cards(i).value - cards(i-1).value ) ).forall(_==1)
    
    private lazy val isFlush =
      (1 until cards.length) forall( cards(_).suit==cards(0).suit )
      
    private lazy val highCard = cards(cards.length-1)
      
    private lazy val runs = 
    {
      val counts = Array.fill(15)(0)
      cards foreach { i => counts(i.value) += 1 }
      def tCompare( a:Tuple2[Int,Int], b:Tuple2[Int,Int] ) =
        if (a._1==b._1)
          a._2>b._2
        else
          a._1>b._1
      counts.zipWithIndex.filter( _._1>0 ).sortWith( tCompare )
    }
    
    def rank =
    {
      if ( this.isStraight && this.isFlush )
        if ( this.highCard.value==14 )
          HandRank( RoyalFlush )
        else
          HandRank( StraightFlush, this.highCard.value )
      else if ( this.runs(0)._1 == 4 )
        HandRank( FourOfKind, this.runs(0)._2, this.runs(1)._2 )
      else if ( this.runs(0)._1 == 3 && this.runs(1)._1 == 2 )
        HandRank( FullHouse, this.runs(0)._2, this.runs(1)._2 )
      else if ( this.isFlush )
        HandRank( Flush, this.highCard.value )
      else if ( this.isStraight )
        HandRank( Straight, this.highCard.value )
      else if ( this.runs(0)._1 == 3 )
        HandRank( ThreeOfKind, this.runs(0)._2, this.runs(1)._2, this.runs(2)._2 )
      else if ( this.runs(0)._1 == 2 && this.runs(1)._1 == 2 )
        HandRank( TwoPair, this.runs(0)._2, this.runs(1)._2, this.runs(2)._2 )
      else if ( this.runs(0)._1 == 2 )
        HandRank( OnePair, this.runs(0)._2, this.runs(1)._2, this.runs(2)._2, this.runs(3)._2 )
      else
        HandRank( HighCard, this.runs(0)._2, this.runs(1)._2, this.runs(2)._2, this.runs(3)._2, this.runs(4)._2 )
    }
  }
  
  object Hand
  {
    def apply( x:String ) = new Hand( ((x split " ") map(new Card(_))).toList )
  }
  
  def solution =
  {
    // slurp in file
    val lines = io.Source.fromFile(DataPath).getLines() toArray
    val h1 = lines map ( s => s.substring( 0, 15) ) map ( Hand(_) )
    val h2 = lines map ( s => s.substring( 15 ) ) map ( Hand(_) )
    def comp( a:HandRank, b:HandRank ) =  if ( a>b ) 1; else 0
    (( h1 zip h2 ) map { case (a,b) => comp(a.rank,b.rank) }) reduceLeft(_+_)
  }
  
  def test =
  {
    val h = Array( 
        Hand( "2H 3H 4H 5H 6H" ),
        Hand( "TH JH QH KH AH" ),
        Hand( "2H 2S 3D 3H 4D" ),
        Hand( "2H 2D 2S 2C 3C" ),
        Hand( "5S 5D 5C 3S 3D" ),
        Hand( "5S 7S 9S JS QS" ),
        Hand( "8D 9S TD JH QC" ),
        Hand( "5D 5C 5S QH JS" ),
        Hand( "9D 9S 8H 8S TD" ),
        Hand( "3D 3S 8H 9S TD" ),
        Hand( "KS QH 7S 8D 9D" )
        )
    ( for( h2 <- h ) yield "%s: %s".format( h2.rank, h2 ) ) mkString "\n"
  }
  
  def test2 =
  {
    val p1 = Array(
        Hand( "5H 5C 6S 7S KD" ),
        Hand( "5D 8C 9S JS AC" ),
        Hand( "2D 9C AS AH AC" ),
        Hand( "4D 6S 9H QH QC" ),
        Hand( "2H 2D 4C 4D 4S" )
        )
    val p2 = Array(
        Hand( "2C 3S 8S 8D TD" ),
        Hand( "2C 5C 7D 8S QH" ),
        Hand( "3D 6D 7D TD QD" ),
        Hand( "3D 6D 7H QD QS" ),
        Hand( "3C 3D 3S 9S 9D" )
        )
    def comp( a:HandRank, b:HandRank ) =  if ( a>b ) "player1"; else "player2"
    ( p1 zip p2 ) map { case (a,b) => comp(a.rank,b.rank) }
  }
}