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

  object GRASP {

    /**
      * @param graph a Graph
      * @param edge the edge that the cost will be computed
      * @return the cost of the edge in the graph
      */
    def cost(graph: Graphe, edge: (Int,Int)): Double = {
        for (i <- graph.head(edge._1) to graph.head(edge._1 + 1) - 1)
          if (graph.succ(i) == edge._2)
            return Int.int2double(graph.prospectus(i)) / Int.int2double(graph.dist(i))
        Double.MaxValue
      }

    def find_min_edge(graph: Graphe, edges: List[Tuple2[Int, Int]], min_cost: Double, min: Tuple2[Int, Int]): Tuple2[Int, Int] = edges match {
        case Nil => min
        case head :: tail => if (cost(graph, head) <= cost(graph, min))
          find_min_edge(graph, tail, cost(graph, head), head)
        else find_min_edge(graph, tail, min_cost, min)
      }

    /**
      *
      * @param alpha randomize rate
      * @param graph the graph to analyze
      * @param n the graph will be divided in n connected component
      */
    def glouton_proba(alpha: Double, graph: Graphe, n: Int): Array[graph.CC] = {
      val CCs = Array.fill[graph.CC](n)(null)
      val edges = computeEdges(graph)
      var S = Set[Tuple2[Int, Int]]()
      var min = (0,0)
      for (i <- 0 to n - 1) {
        min = find_min_edge(graph, edges, cost(graph, edges.head), edges.head)
        S = S + min + min.swap
        CCs(i) = graph.createCC(min)
      }

      def graphCost(CC: graph.CC): Double ={
        var prospectusSum = 0
        var distSum = 0
        var compacitySum = 0
        CCs.foreach({CC => prospectusSum += CC.prospectusCovered; distSum += CC.distCovered; compacitySum += compacity(CC)})
        Math.abs(CC.prospectusCovered/(prospectusSum/n) -1) + Math.abs(CC.distCovered/(distSum/n) -1) + Math.abs(compacity(CC)/(compacitySum/n))
      }
      def computePossibilities()
      def probaComputing(alpha: Double, S: Set[Tuple2[Int,Int]], cc: graph.CC): Tuple2[Int,Int] = {

      }
      while(S.size != edges.length){
        var cc = CCs.minBy(x => graphCost(x))
      }
      CCs
    }

  }
}
