package org.nason.euler

import java.io.{File, FileWriter}

object EulerUtils
{
  val DataRoot = "/home/sorensjm/workspace/euler/data"
  val DataRoot2 = "/Users/jon/Documents/IdeaProjects/euler-jms-scala/data"

  def writeFile(file: File, content: String) =
    if (file.exists() && !file.canWrite())
      System.err.println("File " + file + " is not writable")
    else {
      val writer = new FileWriter(file, false)
      writer.write(content)
      writer.close()
    }

  def readDataFile( fileName:String ) : Iterator[String] = {
    val file = new File(DataRoot2,fileName)
    if (!file.exists()) {
      System.err.println("Can not find file %s/%s".format(DataRoot2,fileName))
      Array[String]().iterator
    } else {
      io.Source.fromFile(file).getLines()
    }
  }

  /**
   * Execute operation f and report the amount of time that it took
   */
  def timing[F]( f: =>F ) =
  {
    val then = System.nanoTime
    val ans = f
    val now = System.nanoTime
    println( "Took %g sec".format( (now-then).toDouble*1e-9 ) )
    ans
  }
}