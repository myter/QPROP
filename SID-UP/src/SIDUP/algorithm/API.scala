package SIDUP.algorithm

import scala.collection.mutable.Map
import akka.actor._

protected class TempNode[+T](val nodeLambda : List[_] => T, var dependencies : List[API.NodeId])

object API {
  type NodeId                                        = Integer
  private var system  : ActorSystem                  = ActorSystem()
  private var signals : Map[NodeId,TempNode[_]]      = Map[NodeId,TempNode[_]]()
  private var sources : List[NodeId]                 = List[NodeId]()
  private var sinks   : List[NodeId]                 = List[NodeId]()
  private var idPool  : Integer                      = 0
  private var ready   : List[NodeId]                 = List[NodeId]()
  private var actors  : Map[NodeId,Admitter.NodeRef] = Map[NodeId,Admitter.NodeRef]()
  private var admitter : Admitter.NodeRef            = null

  
  def newNode[T](nodeLambda : List[_] => T) : NodeId = {
    val newNode : TempNode[T] = new TempNode[T](nodeLambda,List[API.NodeId]())
    val nodeId  : Integer     = idPool
    signals                  += (nodeId -> newNode)
    idPool                   += 1
    nodeId
  }
  
  def newSource[T](nodeLambda : List[_] => T) : NodeId = {
    val nodeId : NodeId = newNode[T](nodeLambda)
    sources             = nodeId +: sources
    nodeId
  }
  
  def newSink[T](nodeLambda : List[_] => T)   : NodeId = {
    val nodeId : NodeId = newNode[T](nodeLambda)
    sinks               = nodeId +: sinks
    nodeId
  }
  
  def addDependency(from : NodeId, to : NodeId) : Unit = {
    val node : TempNode[_] = signals.get(to).get
    node.dependencies = from +: node.dependencies
  }
  
  def addDynamicDependency(from : NodeId,to : NodeId) : Unit = {
    val fromAct = actors.get(from).get
    val toAct = actors.get(to).get
    val sourcesAct = sources.map { sourceId => actors.get(sourceId).get }
    admitter ! new AddDependency(sourcesAct,fromAct,toAct)
  }
  
  private def containsAll[E](container : List[E], elements : List[E]) : Boolean = {
    elements.foldLeft(true)((acc : Boolean, element : E) =>
      acc && container.contains(element)
    )
  }
  
  def construct(admitterCallback : () => _ = () => null) : (List[NodeId],List[_]) => _ = {
    //Construct nodes in right order and store them in actors map
    //Super inefficient but not part of benchmark so does not matter at this point 
    ready = ready ++ sources
    sources.foreach { sourceId : NodeId =>  
      val tempNode : TempNode[_]       = signals.get(sourceId).get
      val actorRef : Admitter.NodeRef  = system.actorOf(Props(new SourceNode[Any](tempNode.nodeLambda)))
      actors                          += (sourceId -> actorRef)
    }
    while(ready.length != signals.size){
      signals.foreach {case (signalId : NodeId, node : TempNode[_]) => 
        if(containsAll(ready,node.dependencies) && !(ready.contains(signalId))){
          val depRefs  : List[Admitter.NodeRef] = node.dependencies.map { dep : NodeId => actors.get(dep).get }
          if(sources.contains(signalId)){
            val actorRef : Admitter.NodeRef       = system.actorOf(Props(new SourceNode[Any](node.nodeLambda)))
            actors                               += (signalId -> actorRef)
            ready = signalId +: ready
          }
          else{
            if(sinks.contains(signalId)){
              val actorRef : Admitter.NodeRef       = system.actorOf(Props(new SinkNode[Any](node.nodeLambda,depRefs)))
              actors                               += (signalId -> actorRef)
              ready = signalId +: ready
            }
            else{
              val actorRef : Admitter.NodeRef       = system.actorOf(Props(new Node[Any](node.nodeLambda,depRefs)))
              actors                               += (signalId -> actorRef)
              ready = signalId +: ready              
            }
          }

        }
      }
    }
    //Construct admitter
    val sourceRefs : List[Admitter.NodeRef] = sources.map { sourceId : NodeId => actors.get(sourceId).get}
    val sinkRefs   : List[Admitter.NodeRef] = sinks.map { sinkId : NodeId => actors.get(sinkId).get }
    admitter     = system.actorOf(Props(new Admitter(sourceRefs,sinkRefs,admitterCallback)))
    //Send start message to all
    actors.foreach{case (nodeId : NodeId, actorRef : Admitter.NodeRef) => 
      if(sinks.contains(nodeId)){
        actorRef ! new StartSink(admitter)
      }
      else{
        actorRef ! new Start()
      }
    }
    //return lambda which sends message to admitter
    (changedSources : List[NodeId], values : List[_]) => {
      val sourceRefs : List[Admitter.NodeRef] = changedSources.map { sourceId : NodeId => actors.get(sourceId).get}
      admitter ! new ChangeSources(sourceRefs,values)
    }
    
  }
  
  //Not very clean, but ok for the sake of benchmarks
  def cleanUp() : Unit = {
    signals   = Map[NodeId,TempNode[_]]()
    sources   = List[NodeId]()
    sinks     = List[NodeId]()
    idPool    = 0
    ready     = List[NodeId]()
    actors    = Map[NodeId,Admitter.NodeRef]()
     actors.foreach{ case (_ : Any, actor : ActorRef) =>
      system.stop(actor)
    }
     Thread.sleep(2000)
  }

 
}