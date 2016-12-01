package graphGeneration

abstract class Topology(val size : Int, val sources : Int) {
  val graph : Graph               = new Graph()
  val srcs  : List[Graph.NodeId]  = List.fill(sources)(graph.newNode()) 
  
  def generate() : Graph
}