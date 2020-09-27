package whiteboxhh

///////////////////////////////////

object runChocoSolver { 
  
  def apply(xcspFile: java.io.File, timeoutInSeconds: Int): Option[Double] = { 

    val root = System.getProperty("user.dir")
    val jarFilePath = s"${root}/resources/bin"
    val jarFile = s"$jarFilePath/choco-parsers-4.10.4-jar-with-dependencies.jar"
    
    ///////////////////////////////    

    val hours: Int = timeoutInSeconds / 3600
    val minutes: Int = timeoutInSeconds / 60
    val seconds: Int = timeoutInSeconds % 60
    
    ///////////////////////////////    
    
    val cmd = s"java -cp $jarFile org.chocosolver.parser.xcsp.ChocoXCSP -limit ${hours}h${minutes}m${seconds}s $xcspFile"
    val lines = scala.sys.process.Process( cmd ).lines_!.toList
    
    val successConditions = List( "s SATISFIABLE", "s UNSATISFIABLE", "s OPTIMUM FOUND" )
    val index = lines.indexWhere { s => successConditions.contains( s ) }
    
    if( index == -1 ) {
      None
    } else {
      if( lines( index - 1).split(" ")(0) == "o" ) { // objective value
        Some( lines( index - 1).split(" ")(1).toDouble )
      }
      else { 
        lines( index ) match { 
          case "s SATISFIABLE" => Some( 1.0 )
          case "s UNSATISFIABLE" => Some( 0.0 )
          case _ => Some( Double.NaN ) // unrecognised
        }
      }
    }
  }

  /////////////////////////////////
  
  def main(args: Array[String]) { 
    val root = System.getProperty("user.dir") + "/resources/XCSP/TravellingSalesman-m1-n15"
    val xcspFile = new java.io.File( s"$root/TravellingSalesman-15-30-00.xml" )
    jeep.lang.Diag.println( runChocoSolver(xcspFile, timeoutInSeconds=10 ) )
  }
}

// End ///////////////////////////////////////////////////////////////
