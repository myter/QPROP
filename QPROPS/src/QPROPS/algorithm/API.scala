package QPROPS.algorithm
import akka.actor._
import scala.collection.mutable.Map
import scala.reflect._
import scala.concurrent.duration._
//Since the actual TempNode class extends actor it is impossible to transparently create nodes (without explicitly having to spawn actor etc etc) before graph creation
//A TempTempNode differs the creation of the actual actor until graph construction starts
protected class TempNode[+T](val nodeLambda: List[Any] => T,val defVal: T, val name : String = null)

object API {
  type NodeId                                                = Integer
  private val system   : ActorSystem                         = ActorSystem()
  private var signals  : List[TempNode[Any]]                 = List[TempNode[Any]]()
  private var edges    : List[(TempNode[Any],TempNode[Any])] = List[(TempNode[Any],TempNode[Any])]()
  private var actors   : Map[TempNode[Any],ActorRef]         = Map[TempNode[Any],ActorRef]()
  private var idCounter: Integer                             = 0
  private var ids      : Map[NodeId,TempNode[Any]]           = Map[NodeId,TempNode[Any]]()
  private var dynamic  : Boolean                             = false
  
  private def getParents(node: TempNode[Any]) : List[TempNode[Any]] = {
    val parentEdges : List[(TempNode[Any],TempNode[Any])] = edges.filter(edge => edge._2 == node)
    parentEdges.map(edge => edge._1)
  }
  
  private def getChildren(node: TempNode[Any]) : List[TempNode[Any]] = {
    val childrenEdges : List[(TempNode[Any],TempNode[Any])] = edges.filter(edge => edge._1 == node)
    childrenEdges.map(edge => edge._2)
  }
  
  def newNode[T: ClassTag](nodeLambda: List[Any] => T,defVal: T) : NodeId = {
    val node : TempNode[T] = new TempNode[T](nodeLambda,defVal)
    idCounter             += 1
    ids                   += (idCounter -> node)
    signals                = node +: signals
    idCounter
  }
  
  //Allows to provide a "name" for a node, mostly used for debugging
  def newNode[T: ClassTag](nodeLambda : List[Any] => T, defVal : T, name : String) : NodeId = {
    val node : TempNode[T] = new TempNode[T](nodeLambda,defVal,name)
    idCounter             += 1
    ids                   += (idCounter -> node)
    signals                = node +: signals
    idCounter
  }
  
  //Connects the "from" node to a list of "to" nodes
  def -=>(from : NodeId, tos: List[NodeId]) : Unit = {
    //Add an edge between "from" and each "to", edges will be translated to actual actor references during construction
    val fromNode : TempNode[Any]         = ids.get(from).get
    val toNodes  : List[TempNode[Any]]   = tos.map { toId => ids.get(toId).get}
    toNodes.foreach { to => edges = (fromNode,to) +: edges }
  }
  
  //Connects the list of "from" nodes to the "to" node
  def =->(froms : List[NodeId], to: NodeId) : Unit = {
    val fromNodes : List[TempNode[Any]] = froms.map { fromId => ids.get(fromId).get}
    val toNode    : TempNode[Any]       = ids.get(to).get
    fromNodes.foreach { from => edges = (from,toNode) +: edges }
  }
  
  //Connects "from" node to "to" node
  def ->(from: NodeId, to: NodeId) : Unit = {
    val fromNode : TempNode[Any] = ids.get(from).get
    val toNode   : TempNode[Any] = ids.get(to).get
    edges = (fromNode,toNode) +: edges
  }
  
  //Dynamically connects "from" node to "to" node
  def *->(from: NodeId, to: NodeId) : Unit = {
    val toTemp : TempNode[Any] = ids.get(to).get
    val fromTemp : TempNode[Any] = ids.get(from).get
    val toAct : ActorRef = actors.get(toTemp).get
    val fromAct : ActorRef = actors.get(fromTemp).get
    toAct ! new DynamicParentMsg(fromAct)
  }
  
  //Construct the graph and start initialisation phase
  def <>() : (List[NodeId], List[_]) => _ = {
    //Could be made more efficient by doing BFT over graph to create AND initialise actor at the same time
    //Run over each signal to create actor
    signals.foreach { tempNode =>
      val isSource   : Boolean = (getParents(tempNode)).size == 0
      var node       : Node[Any] = null
      if (isSource) {
        if(tempNode.name == null){
          val actor : ActorRef = system.actorOf(Props(new SourceNode[Any](tempNode.nodeLambda,tempNode.defVal)))
          actors += (tempNode -> actor)
        }
        else{
         val actor : ActorRef = system.actorOf(Props(new SourceNode[Any](tempNode.nodeLambda,tempNode.defVal)), tempNode.name)
         actors += (tempNode -> actor)
        }
         
      }
      else{
        if(tempNode.name == null){
          val actor : ActorRef = system.actorOf(Props(new Node[Any](tempNode.nodeLambda,tempNode.defVal)))
          actors += (tempNode -> actor)
        }
        else{
          val actor : ActorRef = system.actorOf(Props(new Node[Any](tempNode.nodeLambda,tempNode.defVal)), tempNode.name)
          actors += (tempNode -> actor)
        }
        
      }

    }
    //Initialise actor with correct parents and children
    signals.foreach { node => 
      val nodeActor : ActorRef         = actors.get(node).get
      val parents   : List[ActorRef]  = getParents(node).map { node => actors.get(node).get}
      val children  : List[ActorRef]  = getChildren(node).map { node => actors.get(node).get}
      if(dynamic){
        nodeActor ! new DynamicInitMsg(parents,children)
      }
      else{
        nodeActor ! new InitMsg(parents, children)
      }
    }
    val ret = (sourceIds : List[NodeId] , updateValues : List[_]) => {
      (sourceIds.zipWithIndex).foreach { case (sourceId : NodeId, index : Int) =>
        val sourceNode   : TempNode[Any] = ids.get(sourceId).get
        val actor        : ActorRef      = actors.get(sourceNode).get
        val updateValue  : Any           = updateValues(index)
        actor ! new UpdateMsg(List(updateValue))        
      }
    }
    signals    = List[TempNode[Any]]()
    edges      = List[(TempNode[Any],TempNode[Any])]()
    //All other nodes constructed after this are dynamic by definition
    dynamic    = true
    ret
  }
  
  
  //Not very clean, but ok for the sake of benchmarks
  def cleanUp() : Unit = { 
    signals    = List[TempNode[Any]]()
    edges      = List[(TempNode[Any],TempNode[Any])]()
    actors     = Map[TempNode[Any],ActorRef]()
    idCounter  = 0
    ids        = Map[NodeId,TempNode[Any]]()
    dynamic    = false
    actors.foreach{ case (_ : Any, actor : ActorRef) =>
      system.stop(actor)
    }
    Thread.sleep(2000)
  }
  
 
 
}