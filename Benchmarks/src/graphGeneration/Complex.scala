package graphGeneration

class Complex(val fanRange : Range, override val size : Int, override val sources : Int) extends Topology(size,sources) {
  
  def generateNextLayer(currentLayer : List[Graph.NodeId], fanDegrees : List[Int]) : List[Graph.NodeId] = {
    val r         : scala.util.Random  = scala.util.Random
    var nextLayer : List[Graph.NodeId] = List[Graph.NodeId]()
    (currentLayer.zipWithIndex).foreach { case(existingNode : Graph.NodeId, index : Int) => 
      val fanDegree : Int = fanDegrees(index)
      for(i <- 1 to fanDegree){
        val newNode : Graph.NodeId = graph.newNode()
        graph.addEdge(existingNode, newNode)
        nextLayer = newNode +: nextLayer
        //Determine if new node should be shared with other node in same layer (1 chance out of 3)
        val picked : Int  = r.nextInt(3)
        if(picked == 1){
          val parentNode : Graph.NodeId = currentLayer(r.nextInt(currentLayer.length))
          graph.addEdge(parentNode,newNode)
        }
      }
    }
    nextLayer
  }
  
  //Depending on the randomly picked degree of fanning for each node , this graph might slightly surpass the indicated size
  def generate() : Graph = {
    val r            : scala.util.Random  = scala.util.Random
    val sink         : Graph.NodeId       = graph.newNode()
    var currentSize  : Int                = sources + 1
    var prevLayer    : List[Graph.NodeId] = srcs
    //Avoid having source nodes without children
    var firstLayer   : Boolean            = true
    while(currentSize < size){
      var fanDegrees : List[Int] = List[Int]()
      for(i <- 0 to (prevLayer.length - 1)){
        if(firstLayer){
          fanDegrees = (r.nextInt(fanRange.end) + 1) +: fanDegrees
        }
        else{
          fanDegrees = r.nextInt(fanRange.end) +: fanDegrees
        }

      }
      prevLayer                  = generateNextLayer(prevLayer,fanDegrees)
      currentSize               += prevLayer.length  
    }
    val tempSinks      : List[Graph.NodeId]  = graph.getSinks()
      tempSinks.foreach { tempSink : Graph.NodeId => 
      graph.addEdge(tempSink, sink)
    }
    graph
  }
  
  
}