import scala.util.Random

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
    edges.foreach(i => edges.foreach({j => if(!edgeEquality(i, j) && distance(CC, i, j) > max) max = distance(CC, i, j);
      if(max > 10000) println("overflow on" + i +" to "+j +
        CC.D(i._1)(j._1) +" "+  CC.D(i._1)(j._2) +" "+ CC.D(i._2)(j._1) +" "+CC.D(i._2)(j._2)+" max = "+max)}))
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
            return Int.int2double(graph.prospectus(i)) + Int.int2double(graph.dist(i))
        Double.MaxValue
      }

    /**
      * Find the edge with the min cost in the graph
      */
    def find_min_edge(graph: Graphe, edges: List[Tuple2[Int, Int]], min_cost: Double, min: Tuple2[Int, Int], S: Set[(Int,Int)]): Tuple2[Int, Int] = edges match {
        case Nil => min
        case head :: tail => if (cost(graph, head) <= cost(graph, min) && !S.contains(head))
          find_min_edge(graph, tail, cost(graph, head), head, S)
        else find_min_edge(graph, tail, min_cost, min, S)
      }

    /**
      * Apply the GRASP algorithm on the graph to divide it to n connected component
      * @param n the graph will be divided to n connected component
      * @param graph the graph to divide
      * @param maxIter maximal iterations
      * @return the connected components
      */
    def compute(n: Int, graph: Graphe, maxIter: Int): Array[graph.CC] = {
      def graphCost(CC: graph.CC, CCs: Array[graph.CC]): Double ={
        var sumDist = 0D; var sumProspectus = 0D; var sumCompacity = 0D
        for(i<-0 to CCs.length -1) {
          sumDist += Math.abs(CC.distCovered - CCs(i).distCovered)
          sumProspectus += Math.abs(CC.prospectusCovered - CCs(i).prospectusCovered)
          val compacitycc = compacity(CC); val compacitycci = compacity(CCs(i))
          if(compacitycc > 0 && compacitycc<Int.MaxValue && compacitycci>0 && compacitycci < Int.MaxValue)
            sumCompacity += Math.abs(compacitycc - compacitycci)
        }
        sumDist + sumProspectus + sumCompacity
      }

      def graphScore(CC: graph.CC, CCs: Array[graph.CC]): Double ={
        var sumDist = 0D; var sumProspectus = 0D; var sumCompacity = 0D; val compacitycc = compacity(CC)
        for(i<-0 to CCs.length -1) {
          sumDist += CCs(i).distCovered
          sumProspectus += CC.prospectusCovered - CCs(i).prospectusCovered
          val compacitycci = compacity(CCs(i));
          if(compacitycc>0 && compacitycc<Int.MaxValue && compacitycci>0 && compacitycci < Int.MaxValue)
            sumCompacity += compacitycc - compacitycci
        }
        var comp = 0D
        if(compacitycc>0 && compacitycc < Int.MaxValue && sumCompacity>0 && sumCompacity < Int.MaxValue)
          comp = n*compacitycc /sumCompacity
        (n*CC.distCovered/sumDist) + (n*CC.prospectusCovered/sumProspectus) + comp
      }

      def probaComputing(alpha: Double, S: Set[Tuple2[Int,Int]], cc: graph.CC, CCs: Array[graph.CC]): Tuple2[Int,Int] = {
        var possibilities = cc.computeEdgePossibilities()
        var costList = List[((Int,Int),Double)]()
        possibilities.foreach( edge =>
          if(!S.contains(edge)) {
            cc.addEdge(edge)
            costList = (edge, graphCost(cc, CCs)) :: costList
            cc.subEdge(edge)
          })
        val min = costList.minBy(tuple => tuple._2)._2
        val max = costList.maxBy(tuple => tuple._2)._2
        var RCL = List[(Int,Int)]()
        costList.foreach({ tuple =>
          if(tuple._2 <= min + alpha * (max-min)) RCL = tuple._1 :: RCL
        })
        RCL.toVector(new Random().nextInt(RCL.length))
      }

      def greedy_proba(alpha: Double): Array[graph.CC] = {
        val CCs = Array.fill[graph.CC](n)(null)
        val edges = computeEdges(graph)
        var S = Set[Tuple2[Int, Int]]()
        var min = (0,0)
        for (i <- 0 to n - 1) {
          min = find_min_edge(graph, edges, cost(graph, edges.head), edges.head, S)
          S = S + min + min.swap
          CCs(i) = graph.createCC(min)
        }
        while(S.size != edges.length){
          val cc = CCs.minBy(x =>
            if (S.intersect(x.computeEdgePossibilities().toSet) != x.computeEdgePossibilities().toSet)
              graphScore(x, CCs)

            else
              Double.PositiveInfinity
            )
          val e = probaComputing(alpha, S, cc, CCs)
          cc.addEdge(e)
          S = (S + e) + e.swap
        }
        CCs
      }

      def score(CCs: Array[graph.CC]): Double ={
        var sum:Double = 0
        CCs.foreach({cci => CCs.foreach(ccj => sum += Math.abs(graphCost(cci, CCs)- graphCost(ccj, CCs))); sum+=graphCost(cci, CCs)})
        sum
      }

      val step: Double = 1D/maxIter
      var CCsScore = Double.MaxValue
      var CCs = Array[graph.CC]()
      for(i<- 1 to maxIter){
        var array = greedy_proba(step * i)
        println("Essai " + i + " alpha : " + (step*i))
        array.foreach(cc => println("CC : " + computeEdges(cc)))
        array.foreach(cc => println("Prospectus : "+ cc.prospectusCovered + ", distance : " + cc.distCovered + ", compacity : "+ Methaeuristic.compacity(cc) + " Error : " + graphCost(cc, array)))
        if(score(array) <= CCsScore) {
          CCsScore = score(array)
          CCs = array
        }
      }
      println("Choisi ! ")
      CCs.foreach(cc => println("Prospectus : "+ cc.prospectusCovered + ", distance : " + cc.distCovered + ", compacity : "+ Methaeuristic.compacity(cc) + " Error : " + graphCost(cc, CCs)))
      CCs
    }
  }
}
