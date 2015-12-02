class Graphe(val head: Array[Int], val succ: Array[Int], val dist: Array[Int], val prospectus: Array[Int]){

  val D = Array.fill[Array[Int]](head.length -1 )(Array.fill[Int](head.length -1 )(Int.MaxValue))

  def relax(s: Int, u: Int, i: Int){
    if( ( D(s)(u) + dist(i) ) >= 0 && D(s)(succ(i)) > (D(s)(u) + dist(i)) )
      //First condition to avoid the overflow in case of D(s)(u) or dist(i) = infinity
      D(s)(succ(i)) = D(s)(u) + dist(i)
  }

  def dijkstra(s: Int){
    D(s)(s) = 0
    var N = Set(0, (head.length-1))
    while(N.size < head.length){
      var u = Int.MaxValue
      for( i <- D(s).indices.filter({x: Int => !(N.contains(x))}) )
        if(u >= D(s)(i))
          u = i
      N = N+u
      for( i <- head(u) to (head(u+1) -1) )
        relax(s, u, i)
    }
  }

  def fullDijkstraUpdate(){
    for( i <- 1 to (head.length -2) )
      dijkstra(i)
  }

}