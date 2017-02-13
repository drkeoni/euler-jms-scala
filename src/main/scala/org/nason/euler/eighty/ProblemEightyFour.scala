package org.nason.euler.eighty

import org.nason.euler.EulerUtils

import scala.collection.mutable

/**
  * Created by jon on 2/13/17.
  */
object ProblemEightyFour {

  def main(args: Array[String]): Unit =
  {
    EulerUtils timing { println(solution) }
  }

  val squares: mutable.Map[String,Square] = new mutable.HashMap[String,Square]()

  val RNG = scala.util.Random

  object Square {
    def make(id:Int,label:String) : Square = {
      if (label.startsWith("CC"))
        new CommunityChest(id,label)
      else if (label.startsWith("CH"))
        new Chance(id,label)
      else if (label=="G2J")
        new GoToJail(id,label)
      else
        new Property(id,label)
    }
  }

  abstract class Square( val id:Int, val label:String ) {
    var visitCount = 0
    def advance(board:Board):Square = this
  }

  class Property(id:Int,label:String) extends Square(id,label)

  class CommunityChest(id:Int,label:String) extends Square(id,label) {
    override def advance(board:Board):Square = {
      val next = ccPile.getCard

      next match {
        case "S" => this
        case s => squares(s)
      }
    }
  }

  class Chance(id:Int,label:String) extends Square(id,label) {
    override def advance(board:Board):Square = {
      val next = chancePile.getCard

      next match {
        case "S" => this
        case "-3" => board.spaces(board.positionPlusDelta(-3))
        case "+R" => board.findNext("R")
        case "+U" => board.findNext("U")
        case s => squares(s)
      }
    }
  }

  class GoToJail(id:Int,label:String) extends Square(id,label) {
    override def advance(board:Board):Square = squares("JAIL")
  }

  abstract class CardPile {
    val cards:Array[String]
    private var cardPos:Int = 0
    def getCard:String = {
      val card = cards(cardPos)
      cardPos += 1
      if ( cardPos >= cards.length ) {
        cardPos = 0
      }
      card
    }
    override def toString:String = cards mkString ","
    def shuffle:Unit = {
      for( i<-0 until 10000 ) {
        val x = RNG.nextInt(cards.length)
        val y = RNG.nextInt(cards.length)
        val t = cards(x)
        cards(x) = cards(y)
        cards(y) = t
      }
    }
  }

  class CommunityChestPile extends CardPile {
    val cards:Array[String] = ("S,S,S,S,S,S,S,S,S,S,"+
        "S,S,S,S,GO,JAIL").split(",")

    {
      shuffle
    }
  }

  class ChancePile extends CardPile {
    val cards:Array[String] = ("S,S,S,S,S,S,GO,JAIL,"+
        "C1,E3,H2,R1,+R,+R,+U,-3").split(",")

    {
      shuffle
    }
  }

  val ccPile = new CommunityChestPile
  val chancePile = new ChancePile

  class Board {

    val SpacesString =
      "GO,A1,CC1,A2,T1,R1,B1,CH1,B2,B3,JAIL,"+
      "C1,U1,C2,C3,R2,D1,CC2,D2,D3,FP,"+
      "E1,CH2,E2,E3,R3,F1,F2,U2,F3,G2J,"+
      "G1,G2,CC3,G3,R4,CH3,H1,T2,H2"

    val spaces:Array[Square] = {
      for( (label,id) <- SpacesString.split(",").zipWithIndex ) {
        val sq = Square.make(id,label)
        squares.put(label,sq)
      }
      SpacesString.split(",").map( label => squares(label) )
    }

    var position = 0

    def positionPlusDelta(delta:Int) = plusDelta(position,delta)
    def plusDelta(position:Int,delta:Int) = (position + delta + spaces.length) % spaces.length

    def findNext(prefix:String) = {
      //println("finding next %s".format(prefix))
      var p = positionPlusDelta(1)
      var space = spaces(p)
      while( !space.label.startsWith(prefix) ) {
        p = plusDelta(p,1)
        space = spaces(p)
      }
      space
    }

    val NumSides = 4

    def roll:(Int,Boolean) = {
      val die1 = RNG.nextInt(NumSides)+1
      val die2 = RNG.nextInt(NumSides)+1
      (die1+die2,die1==die2)
    }

    var numTurns = 0
    var doublesCount = 0
    def advance = {
      spaces(position).visitCount += 1
      numTurns += 1

      val (delta,isDoubles) = roll

      if ( isDoubles )
        doublesCount += 1
      else
        doublesCount = 0

      if ( doublesCount==3 ) {
        //println("3 doubles, go to Jail!")
        position = squares("JAIL").id
        doublesCount = 0
      } else {
        val position0 = position
        position = positionPlusDelta(delta)
        var lastPosition:Int = -1
        //
        // it's not clear from the instructions whether to iterate
        // this happens when you hit CH3 and then go back three spaces to CC3
        // do you then pick another card?
        // it doesn't matter in the final statistics
        //do {
          lastPosition = position
          position = spaces(position).advance(this).id
        //} while(position!=lastPosition)
        //
        // catch bug
        //
        if ( spaces(position).label=="G2J" ) {
          println("landing on G2J??")
          println("Moved from %s to %s to %s".format(
            spaces(position0).label,spaces(lastPosition).label,spaces(position).label
          ))
        }
      }
    }

    def dumpCounts = {
      for( space <- spaces ) {
        val freq = space.visitCount.toDouble / numTurns.toDouble * 100.0
        println("%4s: %06d (%.2f%%)".format(space.label,space.visitCount,freq))
      }
    }

    def restart = {
      ccPile.shuffle
      chancePile.shuffle
      position = 0
      doublesCount = 0
    }

    def modalString = {
      val counts = spaces.map( _.visitCount ).sortWith( _ > _ )
      val best0 = spaces.find( _.visitCount==counts(0) ).get
      val best1 = spaces.find( _.visitCount==counts(1) ).get
      val best2 = spaces.find( _.visitCount==counts(2) ).get
      "%02d%02d%02d".format(best0.id,best1.id,best2.id)
    }

    override def toString = "Board(%d,%d,%s)".format(numTurns,position,spaces(position).label)
  }


  def solution = {

    println( "Community Chest is %s".format(ccPile.toString))
    println( "Chance is %s".format(chancePile.toString))

    val board = new Board

    for( j <- 0 until 50000 ) {

      for (i <- 0 until 2000) {
        board.advance

        //println( board )
      }

      board.restart
    }

    board.dumpCounts
    board.modalString
  }
}
