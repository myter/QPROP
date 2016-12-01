package examples
import SIDUP.algorithm.API
import akka.actor._

object Diamond extends App {
  val system = ActorSystem()
  
  val sourceId     : API.NodeId = API.newSource[Integer]{ args => args(0).asInstanceOf[Integer]}
  val sinkId       : API.NodeId = API.newSink[Unit]{ args =>println("Received : " + args(0) + " ," + args(1))}
  val aNodeId      : API.NodeId = API.newNode[Integer]{ args => args(0).asInstanceOf[Integer]}
  val bNodeId      : API.NodeId = API.newNode[Integer]{ args => args(0).asInstanceOf[Integer]}
  API.addDependency(sourceId, aNodeId)
  API.addDependency(sourceId, bNodeId)
  API.addDependency(aNodeId, sinkId)
  API.addDependency(bNodeId, sinkId)
  val updateLambda : (List[API.NodeId], List[_]) => _ = API.construct()
  updateLambda(List[API.NodeId](sourceId),List(5))
  updateLambda(List[API.NodeId](sourceId),List(66))
}