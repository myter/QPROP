package translators

import SIDUP.algorithm._
import graphGeneration._
import scala.collection.mutable.Map
import scala.util.Random

class SIDUPTranslator(admitterCallback : () => _, override val intermediateLambda : List[_] => _, override val sourceLambda : List[_] => _, override val sinkLambda : List[_] => _) extends Translator[API.NodeId](intermediateLambda,sourceLambda,sinkLambda) {
  //Keeps mapping from graph nodeId to API nodeId
  val idMapping : Map[Graph.NodeId,API.NodeId] = Map[Graph.NodeId,API.NodeId]()
  var graph : Graph = null
  var fromIndex : API.NodeId = 20
  var toIndex : API.NodeId = 50
  var addedEdges : List[Edge] = List[Edge]()
  
  def translate(graph : Graph) : (List[API.NodeId],List[_]) => _ = {
    this.graph = graph
    graph.nodes.foreach{ case (nodeId : Graph.NodeId, _) =>
      var apiId : API.NodeId  = null
      if(graph.getSources().contains(nodeId)){
        apiId = API.newSource(sourceLambda)
      }
      if(graph.getSinks().contains(nodeId)){
        apiId = API.newSink(sinkLambda)
      }
      else if(!(graph.getSinks().contains(nodeId) || graph.getSources().contains(nodeId))){
        apiId = API.newNode(intermediateLambda)
      }
      idMapping += (nodeId -> apiId)
    }
    graph.edges.foreach { case Edge(from : Graph.NodeId, to : Graph.NodeId) =>
      val fromAPIId : API.NodeId = idMapping.get(from).get
      val toAPIId   : API.NodeId = idMapping.get(to).get
      API.addDependency(fromAPIId, toAPIId)
    }
    API.construct(admitterCallback)
  }
  
  def mapNode(graphNode : Graph.NodeId) : API.NodeId = {
    idMapping.get(graphNode).get
  }
  
  def dynEdge() : Unit = {
    //val toId : (Graph.NodeId,_) = Random.shuffle(graph.nodes).head
    if(graph.legalEdge(fromIndex, toIndex) && !(graph.hasEdge(fromIndex,toIndex))){
        graph.addEdge(fromIndex, toIndex)
        addedEdges = new Edge(fromIndex,toIndex) +: addedEdges
        API.addDynamicDependency(idMapping.get(fromIndex).get, idMapping.get(toIndex).get)
        fromIndex += 1
        toIndex += 5
    }
    else{
      println("Shuffling")
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