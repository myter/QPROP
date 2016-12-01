package examples
import QPROPS.algorithm._

object Diamond extends App{
  var sourceCounter : Integer = 0
  //Create all necessary nodes
  val sourceId : API.NodeId = API.newNode({ _ => sourceCounter += 1 ; sourceCounter}, 0)
  val aNodeId  : API.NodeId = API.newNode({args => args.head.asInstanceOf[Integer]}, 0)
  val bNodeId  : API.NodeId = API.newNode({args => args.head.asInstanceOf[Integer]}, 0)
  val sinkId   : API.NodeId = API.newNode({args => println("Sink received values: " + args(0) + " , " + args(1))}, null)
  //Link nodes
  API.-=>(sourceId, List[API.NodeId](aNodeId,bNodeId))
  API.=->(List[API.NodeId](aNodeId,bNodeId), sinkId)
  //Create graph
  val update : (List[API.NodeId],List[_]) => _ = API.<>()
  //Produce values
  while(true){
    update(List[API.NodeId](sourceId),List(null))
    Thread.sleep(1000)
  }
}