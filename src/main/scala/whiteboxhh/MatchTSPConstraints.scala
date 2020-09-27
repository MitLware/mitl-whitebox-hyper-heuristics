package whiteboxhh

import org.mitlware.problem.tsp._

///////////////////////////////////

object MatchTSPConstraints {

  import org.xcsp.parser._

  /////////////////////////////////

  private def validObjectives(objectives: Seq[XObjectives.OEntry]): Boolean = {
    objectives.length == 1 && 
      objectives.head.getType == XEnums.TypeObjective.SUM && 
        objectives.head.minimize
  }

  /////////////////////////////////
  
  private def parseArrayElem(str: String): Option[(String,Int)] = {
    val s = str.trim
    val lbracket = s.indexOf('[')
    val rbracket = s.indexOf(']')
    if( lbracket == -1 || rbracket == -1 || lbracket == 0 ) {
      None
    } else {      
      val name = s.substring( 0, lbracket )
      scala.util.Try { s.substring( lbracket + 1, rbracket ).toInt }.map {
        number => (name,number) 
      }.toOption
    }
  }
  
  private def arraySize(v: XVariables.VEntry): Option[Int] = {
    val s = v.attributes.get( XEnums.TypeAtt.size )
    if( s == null ) 
      None
    else if( s.head == '[' && s.last == ']' )
      Some(s.substring(1, s.length()-1).toInt)
    else
      None          
  }
  
  /////////////////////////////////
  
  private def allDifferentVarId(c: XConstraints.CEntry): Option[String] = {
    if( c.isInstanceOf[XConstraints.XCtr] ) {
      val asXCtr = c.asInstanceOf[XConstraints.XCtr]
      if( asXCtr.getType() == org.xcsp.parser.XEnums.TypeCtr.allDifferent ) {
        if( asXCtr.childs.length == 1 ) {
          val child = asXCtr.childs.head 
          if( child.getType == XEnums.TypeChild.list 
              && child.value.isInstanceOf[Array[XVariables.XVarInteger]] ) {

            val asArrayOfVarInteger = child.value.asInstanceOf[Array[XVariables.XVarInteger]]
            val (names,indices) = asArrayOfVarInteger.map { v => parseArrayElem(v.id) }.flatten.unzip
            if( names.toSet.size == 1 && indices.toSeq == ( 0 until indices.size ).toSeq ) {
              return Some(names.head )
            }
          }            
        }
      }
    }
    None
  }
  
  /////////////////////////////////
  
  case class XCSPInfo(permVar: String,distVar: String, distFn: (Int,Int) => Int)

  private def validVars(vars: Seq[XVariables.VEntry], info: XCSPInfo): Option[Int] = {

    def permAndDistVars(vars: (XVariables.VEntry, XVariables.VEntry)) = { 
      if( vars._1.id == info.permVar && vars._2.id == info.distVar ) 
        Some(vars) 
      else if( vars._2.id == info.permVar && vars._1.id == info.distVar ) 
        Some(vars.swap) 
      else 
        None
    }

    def validTypes(perm: XVariables.VEntry,dist: XVariables.VEntry) =
      perm.getType == XVariables.TypeVar.integer &&
      ( dist.getType == XVariables.TypeVar.integer || dist.getType == XVariables.TypeVar.real )
    
    for( v1 <- if( vars.length == 2 ) Some((vars.head,vars.last)) else None; 
      sz1 <- arraySize(v1._1); 
      sz2 <- arraySize(v1._2);
      v2 <- if( sz1 > 0 && sz1 == sz2 ) Some(v1) else None;
      v3 <- if( v2._1.id != v2._2.id ) Some(v2) else None;      
      (perm,dist) <- permAndDistVars(v3) if validTypes(perm,dist)      
    ) yield {
      sz1
    }
  }

  /////////////////////////////////
  
  private def argsDefineEdges(args: Array[Array[Object]], permVarId: String): Option[String] = {
    val asTuples = args.map { entry =>
      if( entry.length == 3 ) {
        for( e0 <- parseArrayElem(entry.head.toString) if e0._1 == permVarId;
          e1 <- parseArrayElem(entry(1).toString) if e1._1 == permVarId;
          e2 <- parseArrayElem(entry.last.toString) if e2._2 == e0._2 ) yield {
            (e0._2,e1._2,e2._1) 
          }
      }
      else
        None
    }.flatten
 
    val (e0,e1,w) = asTuples.unzip3
    if( w.toSet.size == 1 && 
      e0.toSeq == ( 0 until args.length ).toSeq &&  
      e1.toSeq == ( 1 until args.length ).toSeq :+ 0 )
      Some( w.head )
    else 
      None        
  }

  private def distanceFnFromSupports(group: XConstraints.XGroup, permVarId: String): Option[(Int,Int) => Int] = {
    val reifiable = group.template
    if( reifiable.isInstanceOf[XConstraints.XCtr]) {
      val asCtr = reifiable.asInstanceOf[XConstraints.XCtr]
        
      if( asCtr.childs.length == 2 && 
        asCtr.childs.head.getType == XEnums.TypeChild.list && 
        asCtr.childs.last.getType == XEnums.TypeChild.supports ) {

        val listValue = asCtr.childs.head.value 
        if( listValue.isInstanceOf[Array[XConstraints.XParameter]]) {
          val asArray = listValue.asInstanceOf[Array[XConstraints.XParameter]]
          if( asArray.length == 3 && asArray.zipWithIndex.forall { case (p,i) => p.number == i } ) {
              
            val supportsValue = asCtr.childs.last.value
 
            // FIXME: what about other array types?
            if( supportsValue.isInstanceOf[Array[Array[Byte]]] ) {
              val asByteArray = supportsValue.asInstanceOf[Array[Array[Byte]]]
              if( asByteArray.forall { _.length == 3 } ) {
                if( asByteArray.forall { x => x(2) >= 0 } ) {
                    val asMap = Map.empty[(Int,Int),Int] ++ asByteArray.map { x => (x(0).toInt,x(1).toInt) -> x(2).toInt }
                    return Some((i: Int, j: Int) => asMap((i,j)) )
                  }
                }
              }
            }
          }
        }
      }
    None
  }
  
  private def extensionGroup(constraint: XConstraints.CEntry, permVarId: String): Option[XCSPInfo] = {
    if( constraint.isInstanceOf[XConstraints.XGroup]) {
      val asGroup = constraint.asInstanceOf[XConstraints.XGroup]
      for( distVar <- argsDefineEdges(asGroup.argss,permVarId);
        distFn <- distanceFnFromSupports(asGroup, permVarId) ) yield {
          XCSPInfo(permVar=permVarId,distVar=distVar,distFn)
        }
    }
    else
        None
  }
  
  private def validConstraints(constraints: Seq[XConstraints.CEntry]): Option[XCSPInfo] = {
    for( permVarId <- allDifferentVarId(constraints.head) if constraints.length == 2;
      result <- extensionGroup(constraints.last,permVarId) ) yield {
      result
    }
  }
  
  /////////////////////////////////
  
  def matchTSP(is: java.io.InputStream): Option[TSP] = {
    
		val parser = new XParser( is )

    val vars = parser.vEntries
    val constraints = parser.cEntries
    val objectives = parser.oEntries

    type JavaDistFn = java.util.function.BiFunction[java.lang.Integer, java.lang.Integer, java.lang.Double]
		
    def mkJavaDistFn(distFn: (Int,Int) => Int) : JavaDistFn = new JavaDistFn {
      override def apply(i: java.lang.Integer,j: java.lang.Integer): java.lang.Double = distFn(i,j)
		}

    def isSymmetric(numCities: Int,distFn: (Int,Int) => Int): Boolean = {
      for( i <- 0 until numCities; j <- i + 1 until numCities ) {
        if( distFn(i,j) != distFn(j,i) )
          return false
      }
      true
    }
		
    class XCSPTSP(nCities: Int,distFn: JavaDistFn, isSymm: Boolean) extends TSP {

      override def numCities(): Int = nCities
      override def isSymmetric(): Boolean = isSymm
      override def getDistanceFn(): JavaDistFn = distFn
      
      override def toTSPLibFormat(): java.util.Optional[String] = {

        val tspType = "TSP"    
        // val tspType = if(tsp.isSymmetric()) "TSP" else "ATSP"
        // ^ can't do this: linkern.exe trivially fails if type isn't explicitly "TSP"    
      
        val distanceMatrix = Array.tabulate( numCities(), numCities() ) { (i,j) =>
          if( i == j ) 9999 else getDistanceFn()(i,j).toInt
        }
      
        val distanceMatrixStr = distanceMatrix.map { arr => arr.mkString( "\t", "\t", "" ) }.mkString( "\n" )       
      
        val lines = List(
          "NAME: autogenerated",
          "TYPE: TSP",
          "COMMENT: na",      
          s"DIMENSION: ${numCities()}",
          "EDGE_WEIGHT_TYPE: EXPLICIT",
          "EDGE WEIGHT FORMAT: FULL_MATRIX",
          "EDGE_WEIGHT_SECTION",
          s"${distanceMatrixStr}",
          "EOF"
        )
        java.util.Optional.of( lines.mkString("\n") )
		  }
    }
    
		///////////////////////////////      
      
    import scala.collection.JavaConversions._
    
		for { 
		  xcspInfo <- validConstraints(constraints) if validObjectives(objectives)
		  numCities <- validVars(vars, xcspInfo ) 
    } yield
		  new XCSPTSP(numCities,mkJavaDistFn(xcspInfo.distFn),isSymmetric(numCities,xcspInfo.distFn))
  }

  /////////////////////////////////

  def main(args: Array[String]): Unit = {
    
    val path = System.getProperty( "user.dir" ) + "/resources/TravellingSalesman-m1-n15/TravellingSalesman-15-30-00.xml"
		jeep.lang.Diag.println( matchTSP( new java.io.FileInputStream(path) ) )
  }
}

// End ///////////////////////////////////////////////////////////////
