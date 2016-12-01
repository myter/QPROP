package graphGeneration

class Chain(override val size : Int, override val sources : Int) extends Topology(size,sources) {
  
  def generate() : Graph = {
    var prevNode  : Graph.NodeId        = -1
    for(i <- sources to size) {
      val newNode : Graph.NodeId = graph.newNode()
      if(prevNode < 0){
        srcs.foreach { src : Graph.NodeId => 
          graph.addEdge(src, newNode)
        }
      }
      else{
        graph.addEdge(prevNode, newNode)
      }
      prevNode = newNode
    }
    graph
  }
}