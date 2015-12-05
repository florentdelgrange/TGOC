object Main {

  def main(args: Array[String]){
    val graph = new Graphe(Array(0,1,4,5,7,11,14,17,18,19), Array(0,2,4,6,3,4,8,2,5,6,7,3,7,8,4,5,7,8,7,0), Array(0,1,5,7,3,0,8,1,1,3,9,1,4,7,2,2,4,2,1,0), Array())
    graph.fullDijkstraUpdate()
    dijkstraTest(graph.D)
    println("Arrêtes : " ,Methaeuristic.computeEdges(graph))
    println("Compacity of the Graph : ", Methaeuristic.compacity(graph))
  }

  def dijkstraTest(D: Array[Array[Int]]): Unit ={
    for(i<- 1 to D.length -1) {
      println("Plus courts chemis du sommet ", i, " à : ")
      for (j <- 1 to D(i).length - 1)
        println("---> ", j, " = ", D(i)(j))
    }
  }

}
