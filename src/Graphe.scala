class Graphe(val head: Array[Int], val succ: Array[Int], val dist: Array[Int], val prospectus: Array[Int], var size: Int){

  def this(head: Array[Int], succ: Array[Int], dist: Array[Int], prospectus: Array[Int]){
    this(head, succ, dist, prospectus, head.length -1 )
  }

  val D = Array.fill[Array[Int]](head.length -1)(Array.fill[Int](head.length -1)(Int.MaxValue))
  var G = (head.indices.toSet - 0) - size

  def relax(s: Int, u: Int, i: Int){
    if( ( D(s)(u) + dist(i) ) >= 0 && D(s)(succ(i)) > (D(s)(u) + dist(i)) ) {
      //First condition to avoid the overflow in the case of D(s)(u) or dist(i) = infinity
      D(s)(succ(i)) = D(s)(u) + dist(i)
    }
  }

  def dijkstra(s: Int){
    D(s)(s) = 0
    var N = Set(0)
    while(N.size < (size)){
      var u = 0
      for( i <- D(s).indices.filter({x: Int => !(N.contains(x)) && G.contains(x)}) )
        if(D(s)(u) >= D(s)(i))
          u = i
      N = N+u
      for( i <- head(u) to (head(u+1) -1) )
        if(succ(i) != 0)
          relax(s, u, i)
    }
  }

  def fullDijkstraUpdate(){
    G.foreach(i => dijkstra(i))
  }

  def createCC(edge: Tuple2[Int,Int]): CC ={
    val cc = new CC(this)
    cc.G = Set()
    cc.addEdge(edge)
    cc
  }

  class CC(father: Graphe) extends Graphe(head, Array.fill[Int](succ.length)(0), dist, prospectus, 1){

    def addEdge(edge: Tuple2[Int,Int]): Unit ={
      if(!G.contains(edge._1)){
        G = G + edge._1
        size = size +1
      }
      if(!G.contains(edge._2)){
        G = G + edge._2
        size = size +1
      }
      for( i<- father.head(edge._1) to father.head(edge._1+1) -1 ) {
        if (father.succ(i) == edge._2)
          succ(i) = edge._2
      }
      for( i<- father.head(edge._2) to father.head(edge._2+1) -1 )
        if(father.succ(i) == edge._1)
          succ(i) = edge._1
    }

  }
}