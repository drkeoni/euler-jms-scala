package org.nason.euler

import java.io.{File, FileWriter}

object EulerUtils
{
  val DataRoot = "/home/sorensjm/workspace/euler/data"
    
  def writeFile(file: File, content: String) =
    if (file.exists() && !file.canWrite())
      System.err.println("File " + file + " is not writable")
    else {
      val writer = new FileWriter(file, false)
      writer.write(content)
      writer.close()
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