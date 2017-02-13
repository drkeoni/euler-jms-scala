package org.nason.euler.eighty

import org.nason.euler.NumberUtils
import org.nason.euler.EulerUtils

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by jon on 2/10/17.
  */
object ProblemEightyThree {

  def main(args: Array[String]): Unit =
  {
    EulerUtils timing { println(solution2) }
  }

  val Data: Array[Array[Int]] = EulerUtils.readDataFile("p083_matrix.txt")
    .toArray
    .map(l => l.split(",").map(_.toInt))

  val Size:Int = Data.length

  val nodes : mutable.Map[(Int,Int),Node] = new mutable.HashMap[(Int,Int),Node]()

  class Node( val x:Int, val y:Int, val weight:Int ) {
    val id = x + y*Size
    var gScore = Int.MaxValue
    var fScore = Int.MaxValue
    var cameFrom:Node = null

    lazy val neighbors : List[Node] = {
      val n = ListBuffer[Node]()
      if ( x-1 >= 0 )
        n += nodes( (x-1,y) )
      if ( y-1 >= 0 )
        n += nodes( (x,y-1) )
      if ( x+1 < Size )
        n += nodes( (x+1,y) )
      if ( y+1 < Size )
        n += nodes( (x, y+1) )
      n.toList
    }

    override def toString = "Node(%d,%d,%d)".format(x,y,weight)
  }


  /**
    * After implementing a solution that didn't scale well, fell back to reading up on A* algorithm
    * and implementing this.
    * @return
    */
  def solution2 = {
    for( i <- 0 until Size; j <- 0 until Size ) {
      nodes.put( (i,j), new Node(i,j,Data(i)(j)) )
    }
    println("Created %d nodes for %dx%d matrix".format(nodes.size,Size,Size))

    val dataMinimum:Int = (for( i <- 0 until Size; j <- 0 until Size ) yield Data(i)(j)).min
    def heuristic( start:Node, goal:Node ) : Int = {
      val dist = goal.y - start.y + goal.x - start.x
      dist * dataMinimum
    }

    // following pseudocode at https://en.wikipedia.org/wiki/A*_search_algorithm
    val closedSet = mutable.Set[Node]()
    val openSet = mutable.Set[Node]()
    val Start = nodes((0,0))
    val Goal = nodes((Size-1,Size-1))
    openSet.add( Start )

    Start.gScore = Start.weight
    Start.fScore = heuristic(Start,Goal)

    var done = false
    var n = 0
    do {

      val minScore = openSet.map( _.fScore ).min
      val minOpen = openSet.find( _.fScore==minScore ).get

      if (minOpen.id==Goal.id) {
        done = true
      }
      else {

        openSet.remove(minOpen)
        closedSet.add(minOpen)

        for( node <- minOpen.neighbors ) {

          if (!closedSet.contains(node)) {

            val gScore0 = minOpen.gScore + node.weight

            if ( !openSet.contains(node) || gScore0 < node.gScore ) {
              openSet.add(node)

              node.cameFrom = minOpen
              node.gScore = gScore0
              node.fScore = node.gScore + heuristic(node,Goal)
            }
          }
        }

      }

      n += 1
      if ( n>100 ) {
        println("...still working, best fScore is %d".format(minScore))
        n = 0
      }

    } while(openSet.size>0 && !done)

    if ( done )
      Goal.gScore.toString
    else
      "-1"
  }

  val walkers : ListBuffer[Walker] = new mutable.ListBuffer[Walker]
  var bestMinimum = 900000000

  class Walker( weight0:Int, val atNode:Node, visited:mutable.Set[Int] ) {
    var weight:Int = weight0
    var currentNode:Node = atNode
    var marked:Boolean = false

    {
      visited += currentNode.id
    }

    def clone(node:Node) = {
      val v = visited.clone()
      v += node.id
      new Walker( weight + node.weight, node, v )
    }

    def spawnAndWalk = {
      val neigh = currentNode.neighbors
      var weightToAdd = 0
      for( node <- neigh ) {
        if ( !visited.contains(node.id) ) {
          if ( weightToAdd==0 ) {
            weightToAdd = node.weight
            currentNode = node
            //println( "walker %s moved to %s".format(this.toString,node.toString) )
          } else {
            val newWalker = clone(node)
            walkers += newWalker
            //println( "new walker %s at %s".format(newWalker.toString,node.toString) )
          }
        }
      }
      if ( weightToAdd==0 )
        marked = true
      else
        weight += weightToAdd
    }

    override def toString = "Walker(%d,%d,%d)".format(currentNode.x,currentNode.y,weight)
  }

  /*
  This solution works but is ultimately too efficient to solve the 80x80 matrix in a timely fashion
   */
  def solution = {
    for( i <- 0 until Size; j <- 0 until Size ) {
      nodes.put( (i,j), new Node(i,j,Data(i)(j)) )
    }
    println("Created %d nodes for %d x %d matrix".format(nodes.size,Size,Size))

    //val walker = new Walker(nodes((0,0)).weight, nodes((0,0)), new mutable.HashSet[Int])
    //val winnerId = nodes( (Size-1,Size-1) ).id
    val walker = new Walker(nodes((Size-1,Size-1)).weight, nodes((Size-1,Size-1)), new mutable.HashSet[Int])
    val winnerId = nodes( (0,0) ).id
    walkers += walker

    val topMin:Int = (0 until Size).map( j => Data(0)(j) ).sum +
                     (1 until Size).map( i => Data(i)(Size-1) ).sum
    val bottomMin:Int = (0 until Size).map( i => Data(i)(0) ).sum +
      (1 until Size).map( j => Data(Size-1)(j) ).sum

    println("top minimum is %d; bottom minimum is %d".format(topMin,bottomMin))
    var bestMinimum = if ( topMin<bottomMin ) topMin else bottomMin

    for( n <- 0 until 2000 ) {
      val walkers0 = walkers.toList
      for (walker <- walkers0) {
        walker.spawnAndWalk
      }

      val winners = walkers.filter( w => w.currentNode.id == winnerId )
      if ( winners.length>0 )
        println( "%d winners".format(winners.length))
      winners.foreach( w => w.marked = true )

      val thisMinimum: Int = if (winners.length>0) winners.map(_.weight).min else 50000000
      bestMinimum = if ( thisMinimum<bestMinimum ) thisMinimum else bestMinimum
      walkers.foreach( w => { if (w.weight > bestMinimum ) w.marked=true } )

      // mark the top 2/3s for destruction
      if (walkers.length>2000000) {
        val deathCut = 0.67
        val weights = walkers.map(_.weight).sortWith(_ > _)
        println("weights range = [%d,%d]".format(weights(weights.length - 1), weights(0)))
        val cutoff = weights((weights.length * deathCut).toInt)
        walkers.foreach(w => if (w.weight >= cutoff) w.marked = true)
      }

      val numMarked = walkers.count(_.marked)
      println("%d: now %d walkers and minimum path weight is %d; %d marked".format(
        n, walkers.length, bestMinimum, numMarked
      ))

      // cull
      val survivors = walkers.filter( w => !w.marked )
      walkers.clear()
      survivors.foreach( w => walkers += w )
    }
    bestMinimum.toString
  }

  def OLD = {
    def toRect(i: Int, j: Int): (Int, Int) = (i - j, j)

    def toGraph(x: Int, y: Int): (Int, Int) = (x + y, y)

    def readDataInGraph(i: Int, j: Int): Int =
      if (i - j < Data.length && j < Data.length) {
        Data(i - j)(j)
      } else {
        0
      }

    def solution: String = {
      val SIZE = Data.length

      def minPath(i: Int, j: Int, k: Int, l: Int, path: Seq[(Int, Int)]): (Int, Seq[(Int, Int)]) = {
        val (x, y) = toRect(i, j)
        if (x >= SIZE || y >= SIZE) {
          (0, path)
        } else if (i == k) {
          (readDataInGraph(k, l), path ++ List((k, l)))
        } else {
          val patha = minPath(i + 1, j, k, l, path ++ List((i + 1, j)))
          val pathb = minPath(i + 1, j + 1, k, l, path ++ List((i + 1, j + 1)))
          val a = readDataInGraph(i + 1, j) + patha._1
          val b = readDataInGraph(i + 1, j + 1) + pathb._1
          if (a < b) (patha._1 + a, patha._2) else (pathb._1 + b, pathb._2)
        }
      }

      val ans = minPath(0, 0, 4, 4, List((0, 0)))
      ans.toString
    }
  }
}
