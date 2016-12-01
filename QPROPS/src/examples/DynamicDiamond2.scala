package examples
import QPROPS.algorithm._

object DynamicDiamond2 extends App {
    var sourceCounter : Integer = 0
    var sourceCounter2 : Integer = 0
  //Create all necessary nodes
  val sourceId : API.NodeId = API.newNode({ _ => sourceCounter += 1 ; sourceCounter}, 0,"source1")
  val source2Id : API.NodeId = API.newNode({_ => sourceCounter2 += 1; sourceCounter2}, 0,"source2")
  val aNodeId  : API.NodeId = API.newNode({args => args.head.asInstanceOf[Integer]}, 0,"aNode")
  val bNodeId  : API.NodeId = API.newNode({args => args.head.asInstanceOf[Integer]}, 0,"bNode")
  val sinkId   : API.NodeId = API.newNode({args => args.foreach { x => print("Received "+ x + " ") };println()}, null,"sink")
  //Link nodes
  API.-=>(sourceId, List[API.NodeId](aNodeId,bNodeId))
  API.->(source2Id, aNodeId)
  API.=->(List[API.NodeId](aNodeId,bNodeId), sinkId)
  //Create graph
  val update : (List[API.NodeId],List[_]) => _ = API.<>()
  //Produce values
  var index = 0
  while(true){
    update(List[API.NodeId](sourceId,source2Id),List(null,null))
    if(index == 10){
        API.*->(source2Id, bNodeId)
    }
    Thread.sleep(1000)
    index += 1
  }
}