package examples
import QPROPS.algorithm._
//Depending on when the node is added a value might be skipped due to node instability
object DynamicDiamond extends App {
  var sourceCounter : Integer = 0
  //Create all necessary nodes
  val sourceId : API.NodeId = API.newNode({ _ => sourceCounter += 1 ; sourceCounter}, 0, "sourceNode")
  val aNodeId  : API.NodeId = API.newNode({args => args.head.asInstanceOf[Integer]}, 0, "aNode")
  val bNodeId  : API.NodeId = API.newNode({args => args.head.asInstanceOf[Integer]}, 0, "bNode")
  val sinkId   : API.NodeId = API.newNode({args => println("Sink received values:"); args.foreach{arg => println(arg)}}, null,"sink")
  //Link nodes
  API.-=>(sourceId, List[API.NodeId](aNodeId,bNodeId))
  API.=->(List[API.NodeId](aNodeId,bNodeId), sinkId)
  //Create graph
  val update : (List[API.NodeId],List[_]) => _ = API.<>()
  //Produce values
  val newNode : API.NodeId = API.newNode({args => args.head.asInstanceOf[Integer]}, 0, "dynNode")
  API.->(sourceId, newNode)
  API.->(newNode,sinkId)
  API.<>()
  while(true){
    update(List[API.NodeId](sourceId),List(null))
    Thread.sleep(1000)
  }
  
}