package whiteboxhh

import org.mitlware.problem.tsp._

///////////////////////////////////

object WhiteboxHH {

  def optional2option[T](o: java.util.Optional[T]): Option[T] = 
    if( o.isPresent ) Some( o.get() ) else None
  
  def getListOfFiles(dir: String):List[java.io.File] = {
    val d = new java.io.File(dir)
    if( d.exists && d.isDirectory ) 
      d.listFiles.filter(_.isFile).toList
    else
      List.empty[java.io.File]
  }
  
  /////////////////////////////////
  
  def solveTSP(tsp: TSP): Option[Double] = {  

    optional2option( tsp.toTSPLibFormat() ).flatMap { fmtStr =>

      val dir = System.getProperty( "user.dir" ) + "/resources/bin"
      
      import java.io.PrintWriter
      val tspFilename = "fromxcsp.tsp"      
      new PrintWriter( s"$dir/$tspFilename" ) { write(fmtStr); close }
    
      /////////////////////////////
    
      import scala.sys.process._
    
      val cmd = s"$dir/linkern.exe $dir/$tspFilename"
      val output = cmd.!! 
    
      val searchStr = "Final Cycle: "
      val startIndex = output.indexOf(searchStr)
      if( startIndex == -1 )
          None
      else { 
        val endIndex = output.indexOf("\n", startIndex )
        if( endIndex == -1 )
          None
        else
          Some( output.substring( startIndex + searchStr.length, endIndex ).toDouble ) 
      }
    }
  }

  /////////////////////////////////
  
  def solve(xcspFile: java.io.File, timeoutInSeconds: Int): Option[Double] = {  

    // if we recognize the problem as being isomorphic to the TSP, 
    // then run a dedicated solver (Concorde's 'linkern')
    // else use the generic Choco solver:
    
    val xmlStr = scala.io.Source.fromFile(xcspFile).getLines.mkString( "\n" )
    
    MatchTSPConstraints.matchTSP(new java.io.ByteArrayInputStream(xmlStr.getBytes("UTF-8"))) match { 
      case Some(tsp) => solveTSP(tsp)
      case None =>   try {
        runChocoSolver(xcspFile,timeoutInSeconds)
      } catch {
        case t: Throwable =>
          println( "Warning: caught exception " + t.getStackTrace.mkString("\n") ) 
          jeep.lang.Diag.println( t )          
          None        
      }
    } 
  }
  
  /////////////////////////////////

  def run(directories: List[java.io.File], solverTimeoutInSeconds: Int): List[(String,Option[Double])] = { 
    directories.map { dir =>
      println( s"Processing directory: $dir ..." )
      
      getListOfFiles( dir.toString ).map { f =>
        f.getName -> solve( f, solverTimeoutInSeconds )  
      }       
    }.flatten
  }
  
  /////////////////////////////////  
  
  def main(args: Array[String]): Unit = {

    // Multiple problem domains as input:
    val dirs = List(
      "Sat-m1-various", 
      "BinPacking-mdd-ft",        
      "Vrp-zinc-s1",
      "Scheduling-os-taillard",
      "TravellingSalesman-m1-n15"        
    )
    
    val xcspProblemDirectories = dirs.map { dir => 
      val root = System.getProperty("user.dir")
      new java.io.File( s"$root/resources/XCSP/$dir" )
    }

    val results = run( xcspProblemDirectories, solverTimeoutInSeconds=60 )
    println( "results:\n" + results.mkString("\n") )
    println( "All done." )
    
    ///////////////////////////////
    
    // Clean up any threads from subordinate processes:
    System.exit( 0 ) 
  }
}

// End ///////////////////////////////////////////////////////////////
