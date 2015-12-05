object Methaeuristic {

  def computeEdges(CC : Graphe): List[Tuple2[Int,Int]] = {
    var t = List[Tuple2[Int, Int]]()
    for (i <- 1 to (CC.head.length - 2))
      for (j <- CC.head(i) to (CC.head(i + 1) - 1))
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
    def distrec(list: List[Tuple2[Int, Int]], min: Int): Int = list match {
      case Nil => min
      case head :: tail =>
        if (head._1 != head._2 && CC.D(head._1)(head._2) < min)
          distrec(tail, CC.D(head._1)(head._2))
        else distrec(tail, min)
    }
    distrec(List((a1._1, a2._1), (a1._1, a2._2), (a1._2, a2._1), (a1._2, a2._2)), Int.MaxValue)
  }

  def edgeEquality(a1: Tuple2[Int,Int], a2: Tuple2[Int,Int]): Boolean = {
    a1 == a2 || (a1._1 == a2._2 && a1._2 == a2._1)
  }

  def compacity(CC: Graphe): Int = {
    CC.fullDijkstraUpdate()
    val edges = computeEdges(CC)
    var min = Int.MaxValue
    edges.foreach(i => edges.foreach(j => if(!edgeEquality(i, j) && distance(CC, i, j) < min) min = distance(CC, i, j)))
    min
  }

}
