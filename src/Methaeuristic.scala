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
  def glouton_proba(alpha: Double, graph: Graphe, n: Int): Array[List[Tuple2[Int, Int]]] = {
    val CC = Array.fill[List[Tuple2[Int,Int]]](n)(List())
    //val edges = computeEdges()
    CC
  }

}
