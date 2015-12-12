import scala.io.Source

object Main {

  def initGraphe(file : String) : Array[Array[Int]]= {
    val lines = Source.fromFile(file).getLines().toList

    val info = lines(0).split(" ")
    val head = Array("0") ++ lines(1).split(" ")
    val coordx = lines(2).split(" ")
    val coordy = lines(3).split(" ")
    val succ = Array("0") ++ lines(4).split(" ") ++ Array("0")
    val distance = Array("0") ++ lines(5).split(" ") ++ Array("0")
    val prospectus = Array("0") ++  lines(6).split(" ") ++ Array("0")
    Array(info.map(x => x.toInt),head.map(x => x.toInt),succ.map(x => x.toInt),distance.map(x => x.toInt),prospectus.map(x => x.toInt),coordx.map(x => x.toInt),coordy.map(x => x.toInt))
  }
  def main(args: Array[String]){
   val graphInfo = initGraphe("input.txt")
    println(graphInfo(1).length+" : "+graphInfo(2).length+" : "+graphInfo(3).length+" :" +graphInfo(4).length)
    val graph = new Graphe(graphInfo(1),graphInfo(2),graphInfo(3),graphInfo(4))
    graph.fullDijkstraUpdate() ; dijkstraTest(graph.D)
    println("Arrêtes : " ,Methaeuristic.computeEdges(graph)) ;println("Compacity of the Graph : ", Methaeuristic.compacity(graph))
    var cc = graph.createCC((1,2))
    cc.addEdge((2,6)) ; cc.addEdge((6,10)) ; cc.addEdge((1,5)) ; cc.addEdge((5,6))
    cc.fullDijkstraUpdate() ;dijkstraTest(cc.D)
    cc.succ.foreach(i => print(i))
    println() ;println("Arrêtes de la composante connexe : "+ Methaeuristic.computeEdges(cc))
    cc.subEdge((6,10)) ;cc.subEdge((2,6))
    cc.fullDijkstraUpdate()
    println("Retrait de l'arrête (6,10)")
    cc.fullDijkstraUpdate() ;dijkstraTest(cc.D)
    cc.succ.foreach(i => print(i)) ; println()
    println("Compacity of the CC : " + Methaeuristic.compacity(cc))
    println("Edge possibilities : " + cc.computeEdgePossibilities())
    var CCs = Methaeuristic.GRASP.compute(3, graph, 1000)
    CCs.foreach(cc => println(Methaeuristic.computeEdges(cc)))
    //CCs.foreach(cc => println("Prospectus : "+ cc.prospectusCovered + ", distance : " + cc.distCovered + ", compacity : "+ Methaeuristic.compacity(cc)))
  }

  def dijkstraTest(D: Array[Array[Int]]): Unit ={
    for(i<- 1 to D.length -1) {
      println("Plus courts chemins du sommet ", i, " à : ")
      for (j <- 1 to D(i).length - 1)
        if(D(i)(j) != Int.MaxValue)
          println("---> ", j, " = ", D(i)(j))
    }
  }

}
