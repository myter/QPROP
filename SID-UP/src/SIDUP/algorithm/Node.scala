package SIDUP.algorithm
import akka.actor._
import scala.collection.mutable.Map

case class Start()
case class ParentReachableRequest()
case class ParentReachableResponse(reachableSources : Set[Admitter.NodeRef])
case class NewPulse(pulse : MirroredPulse)
case class NewChange(change : ChangeValue)
case class UpdateReachable(isNewParent : Boolean, reachables : Set[Admitter.NodeRef])
//Message send by admitter to source node
case class UpdateValue(pulse : PulseValue)
case class UpdateChangeValue(change : ChangeValue)
//Message send by admitter to sink node
case class StartSink(admitter : Admitter.AdmitterRef)
//Message used by termination detection
case class AckMessage()
//Used for debugging purposes
case class PrintReachables()

class Node[T](val nodeLambda : List[_] => T, val dependencies : List[Admitter.NodeRef]) extends Actor {
  var steadyValue            : T                                             = null.asInstanceOf[T]
  var children               : List[Admitter.NodeRef]                        = List[Admitter.NodeRef]()
  var reachableSources       : Set[Admitter.NodeRef]                         = Set[Admitter.NodeRef]()
  var parentReachableSources : Map[Admitter.NodeRef,Set[Admitter.NodeRef]]   = Map[Admitter.NodeRef,Set[Admitter.NodeRef]]()
  val termination            : DijkstraScholten                              = new DijkstraScholten(() => returnToIdle())
  var waiting                : List[Admitter.NodeRef]                        = List[Admitter.NodeRef]()
  var currentState           : PulseState.Value                              = PulseState.pending
  var currentChangeState     : ChangeState.Value                             = ChangeState.unchanged
  var mirroredPulses         : Map[Admitter.NodeRef,MirroredPulse]           = Map[Admitter.NodeRef,MirroredPulse]()
  var inChange               : Boolean                                       = false
  
  def receive = {
    case Start()                                                            => handleStart()
    case ParentReachableRequest()                                           => handleParentRequest(sender)
    case ParentReachableResponse(reachableSources : Set[Admitter.NodeRef])  => handleParentResponse(sender,reachableSources)
    case NewPulse(pulse : MirroredPulse)                                    => handleNewPulse(sender,pulse)
    case NewChange(change : ChangeValue)                                    => handleNewChange(sender,change)
    case UpdateReachable(isNewParent : Boolean, reachables : Set[Admitter.NodeRef]) => handleUpdateReachable(isNewParent,sender,reachables)
    case AckMessage()                                                       => handleAckMessage()
    //Debugging messages
    case PrintReachables()                                                  => handlePrintReachables()
  }
  
  //returns boolean indicating whether node knows all sources able to reach it 
  protected def receivedAll() : Boolean = {
    dependencies.length == parentReachableSources.size
  }
  
    //initiates "construction" phase of sid-up algorithm (i.e. node start by asking parents for reachable set of source nodes)
  protected def handleStart() : Unit = {
    dependencies.foreach { parent =>
      //Create new dummy mirror
      mirroredPulses.put(parent, new MirroredPulse())
      parent ! new ParentReachableRequest()
    }
  }
  
  protected def handleParentRequest(child : Admitter.NodeRef) : Unit = {
    children = child +: children
    //Check if we already know all sources able to reach us
    if(receivedAll()){
       child ! new ParentReachableResponse(reachableSources)
    }
    else{
      waiting = child +: waiting
    }
  }
  
  protected def handleParentResponse(parent : Admitter.NodeRef, reachables : Set[Admitter.NodeRef]) : Unit = {
    reachableSources = reachableSources.union(reachables)
    parentReachableSources += (parent -> reachables)
    if(receivedAll()){
      waiting.foreach { child =>
        child ! new ParentReachableResponse(reachableSources)
      }
    }
  }
  
  protected def handleNewPulse(sender : Admitter.NodeRef, pulse : MirroredPulse) : Unit = {
    termination.newParentMessage(sender)
    //Update sender mirrored pulse
    mirroredPulses.put(sender, pulse)
    var continue : Boolean = true
    //Check if one of the node's parents (which is reachable by changed source) still needs to pulse 
    dependencies.foreach { parent : Admitter.NodeRef => 
      val mirror = mirroredPulses.get(parent).get
      if(mirror.state == PulseState.pending){
        val parrentReachable = parentReachableSources.get(parent).get
        def reachableChanges = parrentReachable.&(pulse.changedSources)
        if(!(reachableChanges.isEmpty)){
          //Parent still needs to pulse since it is reachable by at least one sources that has changed for this turn
          continue = false
        }
      }
    }
    if(continue){
      var anyChanged : Boolean   = false
      var values     : List[_]   = List()
      dependencies.foreach { parent : Admitter.NodeRef =>  
        val mirror : MirroredPulse = mirroredPulses.get(parent).get
        values                     = mirror.steadyValue +: values
        if(mirror.state == PulseState.changed){
          anyChanged = true
        }
      }
      //If any of the mirrors has changed, re-eval the node and propagate change. Otherwise propagate no change 
      if(anyChanged){
        steadyValue                  = nodeLambda(values)
        currentState                 = PulseState.changed
        val newPulse : MirroredPulse = new MirroredPulse(pulse.changedSources,steadyValue,currentState)
        children.foreach { child : Admitter.NodeRef =>  
          termination.newChildMessage()
          child ! new NewPulse(newPulse)
        }
        resetState()
        if(children.length == 0) {
          termination.sinkTerminated()
        }
      }
      else{
        currentState                 = PulseState.unchanged
        val newPulse : MirroredPulse = new MirroredPulse(pulse.changedSources,steadyValue,currentState)
        children.foreach { child : Admitter.NodeRef =>
          termination.newChildMessage()
          child ! new NewPulse(newPulse)
        }
        //Although termination still needs to confirm, propagation wise the turn has ended for this node
        resetState()
        //If we are a sink, Dijkstra-Scholten needs to start signaling termination across graph 
        if(children.length == 0) {
          termination.sinkTerminated()
        }
      }
    }
  }
  
  protected def handleNewChange(sender : Admitter.NodeRef,change : ChangeValue) : Unit = {
    termination.newParentMessage(sender)
    val from : Admitter.NodeRef = change.from
    val to : Admitter.NodeRef = change.to
    if(from.equals(self) && !(inChange)){
      if(children.contains(to)){
        println("WHAT?!")
        children.foreach { child : Admitter.NodeRef =>  
          child ! NewChange(change)
          termination.newChildMessage()
        }
      }
      else{
        children = to +: children
        inChange = true
        termination.newChildMessage()
        to ! new UpdateReachable(true,reachableSources)
      }
    }
    else if( !(inChange)){
      inChange = true
      children.foreach { child : Admitter.NodeRef =>  
          child ! NewChange(change)
          termination.newChildMessage()
      }
    }
    if(children.length == 0 && !(to.equals(self))){
      termination.sinkTerminated()
    }
  }
  
  protected def handleUpdateReachable(isNewParent : Boolean,sender : Admitter.NodeRef, reachables : Set[Admitter.NodeRef]) : Unit = {
    termination.newParentMessage(sender)
    if(isNewParent){
      if(parentReachableSources.get(sender).isDefined){
        println("Should not happen")
      }
      else{
        mirroredPulses.put(sender,new MirroredPulse())
        parentReachableSources += (sender -> reachables)
      }
    }
    else{
      val previousReachables = parentReachableSources.get(sender).get
      parentReachableSources.put(sender,previousReachables.union(reachables))
    }
    reachableSources = reachableSources.union(reachables)
    children.foreach { child : Admitter.NodeRef =>  
      termination.newChildMessage()
      child ! new UpdateReachable(false,reachableSources)
    }
    if(children.length == 0){
      termination.sinkTerminated()
    }
  }
  
  protected def resetState() : Unit = {
    currentState = PulseState.pending
    dependencies.foreach { parent : Admitter.NodeRef => 
      val mirror       : MirroredPulse = mirroredPulses.get(parent).get
      val resetMirror  : MirroredPulse = new MirroredPulse(mirror.changedSources,mirror.steadyValue,PulseState.pending)
      mirroredPulses.put(parent, resetMirror)
    }
  }
  
  protected def handleAckMessage() : Unit = {
    termination.newAckMessage()
  }
  
  protected def returnToIdle() : Unit = {
    inChange = false
  }
  
  //Debugging handles
  protected def handlePrintReachables() : Unit = {
    println(reachableSources)
  }
  
}



class SourceNode[T](override val nodeLambda : List[_] => T) extends Node[T](nodeLambda,List[Admitter.NodeRef]()) {
  reachableSources = reachableSources.+(self)
  
  override def receive = super.receive orElse {
    case UpdateValue(pulse : PulseValue) => handleUpdate(sender,pulse)
    case UpdateChangeValue(change : ChangeValue) => handleChange(sender,change)
  }
  
  protected def handleUpdate(sender : Admitter.NodeRef, pulse : PulseValue) : Unit = {
    termination.newParentMessage(sender)
    currentState     = PulseState.changed
    val mirroredPulse = new MirroredPulse(pulse.changedSources,pulse.value,currentState)
    children.foreach { child : Admitter.NodeRef => 
      child ! new NewPulse(mirroredPulse)
      termination.newChildMessage()
    }    
  }
  
  protected def handleChange(sender : Admitter.NodeRef, change : ChangeValue) : Unit = {
     termination.newParentMessage(sender)
     val from = change.from
     val to = change.to
     if(from.equals(self)){
      children = to +: children
      to ! new UpdateReachable(true,reachableSources)
      termination.newChildMessage()
    }
    else{
      children.foreach { child : Admitter.NodeRef =>  
          child ! NewChange(change)
          termination.newChildMessage()
      }
    }     
  }
}

case class IllegalMessageException(message : String) extends Exception(message)

class SinkNode[T](override val nodeLambda: List[_] => T, override val dependencies : List[Admitter.NodeRef]) extends Node[T](nodeLambda,dependencies) {
  var admitter : Admitter.AdmitterRef  = null
    
  override def receive = super.receive orElse {
    case StartSink(admitter : Admitter.AdmitterRef) => handleStartSink(admitter)
  }
  
  override protected def handleParentResponse(parent : Admitter.NodeRef, reachables : Set[Admitter.NodeRef]) : Unit = {
    super.handleParentResponse(parent, reachables)
    //When a sink node is constructed, entire graph above it is constructed. Signal this to admitter
    if(super.receivedAll()){
      admitter ! new SinkReady()
    }
  }
  
  protected def handleStartSink(admRef : Admitter.AdmitterRef) : Unit = {
    admitter = admRef
    super.handleStart()
  }
  
  override protected def handleStart() : Unit = {
    throw new IllegalMessageException("Sink nodes must be started with the StartSink message")
  }
  
}