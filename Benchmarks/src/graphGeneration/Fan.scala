package graphGeneration

//The degree of fanning (i.e. amount of intermediate nodes) will be size - 2 (source and sink nodes)
class Fan(override val size : Int, override val sources : Int) extends Topology(size,sources) {
  
  def generate() : Graph = {
    val sink   : Graph.NodeId        = graph.newNode()
    for(i <- (sources + 1) to size){
      val interNode : Graph.NodeId = graph.newNode()
      srcs.foreach { src : Graph.NodeId =>
        graph.addEdge(src, interNode)  
      }
      graph.addEdge(interNode, sink)
    }
    graph
  }
  
}