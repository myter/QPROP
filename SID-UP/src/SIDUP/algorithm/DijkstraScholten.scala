package SIDUP.algorithm

protected object TerminationState extends Enumeration {
  val Processing,Idle = Value
}

//Every node has instance of this class, needed to determine the end of an update turn (implements the dijsktra-scholten algorithm)
//Construction parameter listener is used to notify admitters of the termination of an update turn
class DijkstraScholten(val listener : () => Any = {() => null}) {
  //Deficit on incoming edges
  var c        : Integer                 = 0
  //Deficit on outgoing edges
  var d        : Integer                 = 0
  //Reference to parents of incoming edges 
  var incoming : List[Admitter.NodeRef]  = List[Admitter.NodeRef]()
  //State in which the node is currently in
  var state    : TerminationState.Value  = TerminationState.Idle
  
  //Node has received a pulse message from a parent 
  def newParentMessage(parent : Admitter.NodeRef) : Unit = {
    state     = TerminationState.Processing
    d        += 1
    incoming  = parent +: incoming
  }
  
  //Node is sending pulse message to child
  def newChildMessage() : Unit = {
    state    = TerminationState.Processing
    c       += 1
  }
  
  //A child has notified its termination
  def newAckMessage() : Unit = {
    c       -= 1
    //If outgoing deficit is zero, node has terminated and can send ack to all parents
    if (c == 0){
      sendAcks()
    }
  }
  
  //Used by sink nodes to signal termination
  def sinkTerminated() : Unit = {
    sendAcks()
  }
  
  def sendAcks() : Unit = {
    incoming.foreach { parent : Admitter.NodeRef => 
      d -= 1
      parent ! new AckMessage()
    }
    if (d == 0) {
      state     = TerminationState.Idle
      incoming  = List[Admitter.NodeRef]()
      listener()
    }
    else {
      error("Should not happen")
    }
  }
  
  def isIdle() : Unit = {
    state == TerminationState.Idle
  }
  
}