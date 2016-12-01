package examples
import SIDUP.algorithm.API
import akka.actor._

object DynamicDiamond extends App {
  val system = ActorSystem()
  val sourceId     : API.NodeId = API.newSource[Integer]{ args => args(0).asInstanceOf[Integer]}
  val source2Id    : API.NodeId = API.newSource[Integer] { args => args(0).asInstanceOf[Integer] }
  val sinkId       : API.NodeId = API.newSink[Unit]{ args => args.foreach { x => print("Received "+ x + " ") };println()}
  val aNodeId      : API.NodeId = API.newNode[Integer]{ args => args(0).asInstanceOf[Integer]}
  val bNodeId      : API.NodeId = API.newNode[Integer]{ args => args(0).asInstanceOf[Integer]}
  API.addDependency(sourceId, aNodeId)
  API.addDependency(sourceId, bNodeId)
  API.addDependency(source2Id, aNodeId)
  API.addDependency(aNodeId, sinkId)
  API.addDependency(bNodeId, sinkId)
  val updateLambda : (List[API.NodeId], List[_]) => _ = API.construct()
  var index = 0
  while(true){
    updateLambda(List[API.NodeId](sourceId,source2Id),List(index,index))
    if(index == 5){
        API.addDynamicDependency(source2Id,bNodeId)
    }
    Thread.sleep(1000)
    index += 1
  }
}