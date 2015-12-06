object Methaeuristic {

  def computeEdges(CC : Graphe): List[Tuple2[Int,Int]] = {
    var t = List[Tuple2[Int, Int]]()
    for (i <- 1 to (CC.head.length - 2))
      for (j <- CC.head(i) to (CC.head(i + 1) - 1))
        if(CC.succ(j) != 0)
          t = (i, CC.succ(j)) :: t
    t
  }

  /**
    * Compute the minimal distance in the Connectes Component between the edge a1 and a2. You have to apply a full Dijkstra update before apply this method.
    * @param CC The Connected component
    * @param a1 The first edge
    * @param a2 The twice edge
    * @return the minimal distance between a1 and a2
    */
  def distance(CC: Graphe, a1: Tuple2[Int,Int], a2: Tuple2[Int,Int]): Int = {
    List(CC.D(a1._1)(a2._1), CC.D(a1._1)(a2._2), CC.D(a1._2)(a2._1), CC.D(a1._2)(a2._2)).min
  }

  def edgeEquality(a1: Tuple2[Int,Int], a2: Tuple2[Int,Int]): Boolean = {
    a1 == a2 || (a1._1 == a2._2 && a1._2 == a2._1)
  }

  def compacity(CC: Graphe): Int = {
    CC.fullDijkstraUpdate()
    val edges = computeEdges(CC)
    var max = Int.MinValue
    edges.foreach(i => edges.foreach(j => if(!edgeEquality(i, j) && distance(CC, i, j) > max) max = distance(CC, i, j)))
    max
  }

  /**
    *
    * @param alpha randomize rate
    * @param graph the graph to analyze
    * @param n the graph will be divided in n connected component
    */
  def glouton_proba(alpha: Double, graph: Graphe, n: Int): Array[Graphe] = {
    val CCs = Array.fill[graph.CC](n)(null)
    def cost(edge: Tuple2[Int, Int]): Double = {
      for (i <- graph.head(edge._1) to graph.head(edge._1 + 1) - 1)
        if (graph.succ(i) == edge._2)
          return Int.int2double(graph.prospectus(i)) / Int.int2double(graph.dist(i))
      Double.MaxValue
    }
    var edges = computeEdges(graph)
    def find_min_edge(edges : List[Tuple2[Int,Int]], min_cost: Double, min: Tuple2[Int,Int]): Tuple2[Int,Int] = edges match{
      case Nil => min
      case head :: tail => if(cost(head) <= cost(min))
        find_min_edge(tail, cost(head), head)
        else find_min_edge(tail, min_cost, min)
    }
    var S = Set[Tuple2[Int, Int]]()
    for(i<-0 to n-1){
      var min = find_min_edge(edges, cost(edges.head), edges.head)
      S = S + min + min.swap
      CCs(i) = graph.createCC(min)
    }

    def cost(CC: graph.CC): Double ={
      var prospectusSum = 0.
      var distSum = 0.
      var compacitySum = 0.
      CCs.foreach({CC => prospectusSum += CC.prospectusCovered; distSum += CC.distCovered; compacitySum += compacity(CC)})
      if(CCs.contains(CC))
        Math.abs(CC.prospectusCovered/prospectusSum -1) + Math.abs(CC.distCovered/distSum -1) + Math.abs(compacity(CC)/compacitySum)
      else
        Math.abs(CC.prospectusCovered/(prospectusSum+CC.prospectusCovered) -1) + Math.abs(CC.distCovered/(distSum+CC.distCovered) -1) + Math.abs(compacity(CC)/(compacitySum + compacity(CC))
    }

    while(S.size != edges.length){

    }
    CCs
  }

}
