package translators
import graphGeneration._
import QPROPS.algorithm.API
import scala.collection.mutable.Map
import scala.util.Random

class QPROPTranslator(override val intermediateLambda : List[_] => _, override val sourceLambda : List[_] => _, override val sinkLambda : List[_] => _) extends Translator[API.NodeId](intermediateLambda,sourceLambda,sinkLambda) {
  val idMappings : Map[Graph.NodeId,API.NodeId] = Map[Graph.NodeId,API.NodeId]()
  var graph : Graph = null
  var fromIndex : API.NodeId = 20
  var toIndex : API.NodeId = 50
  var addedEdges : List[Edge] = List[Edge]()
  
  //Default values are set to zero since result of propagation does not matter for benchmark
  def translate(graph : Graph) : (List[API.NodeId],List[_]) => _ = {
    this.graph = graph
    graph.nodes.foreach{ case (nodeId : Graph.NodeId, _) =>
      var apiId : API.NodeId = null
      if(graph.getSources().contains(nodeId)){
        apiId = API.newNode(sourceLambda, 0)
      }
      if(graph.getSinks().contains(nodeId)){
        apiId = API.newNode(sinkLambda, 0)
      }
      else if(!(graph.getSinks().contains(nodeId) || graph.getSources().contains(nodeId))){
        apiId = API.newNode(intermediateLambda, 0)
      }
      idMappings += (nodeId -> apiId)
    }
    graph.edges.foreach { case Edge(from : Graph.NodeId, to : Graph.NodeId) => 
      val fromAPIId : API.NodeId  = idMappings.get(from).get
      val toAPIId   : API.NodeId  = idMappings.get(to).get
      API.->(fromAPIId, toAPIId)
    }
    API.<>()
  }
  
  def mapNode(graphNode : Graph.NodeId) : API.NodeId = {
    idMappings.get(graphNode).get
  }
  
  def dynEdge() : Unit = {
    if(graph.legalEdge(fromIndex, toIndex) && !(graph.hasEdge(fromIndex,toIndex))){
      graph.addEdge(fromIndex, toIndex)
      addedEdges = new Edge(fromIndex,toIndex) +: addedEdges
       API.*->(idMappings.get(fromIndex).get,idMappings.get(toIndex).get)
       fromIndex += 1
       toIndex += 1
    }
    else{
      fromIndex += 1
       toIndex += 1
      dynEdge()
    }
  }
  
  def reverse(oldEdges : List[Edge]) : Unit = {
    oldEdges.foreach { edge => graph.removeEdge(edge) }
  }
  
  def cleanUp() : Unit = {
    API.cleanUp()
  }

}