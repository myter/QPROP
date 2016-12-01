package QPROPS.algorithm
import scala.collection.mutable.Map
import scala.collection.mutable.AnyRefMap
import akka.actor._
import scala.collection.mutable.Queue
import util.control.Breaks._

case class SourcesMsg(from : ParentRef, sources : List[ActorRef])
case class StartMsg()
case class PropagationValueMsg(value : PropagationValue[Any])
case class UpdateMsg(value : Any)
case class InitMsg(directParents : List[ActorRef], directChildren : List[ActorRef])
case class DynamicInitMsg(directParents : List[ActorRef], directChildren : List[ActorRef])
case class GetSourcesMsg(update : Boolean)
case class RcvSourcesMsg(from : ParentRef,parents : List[ActorRef],defVal: Any,update : Boolean)
case class UpdateSourcesMsg(from : ParentRef, newSources : List[ActorRef], updateDefVal : Boolean = false)
case class SourcesUpdatedMsg()
case class AddChildMsg(newChild: ActorRef)
case class DynamicParentMsg(newParent: ActorRef)
//Debug Messages
case class PrintParentsMsg()
case class PrintChildMsg()

class Node[+T](val nodeLambda: List[Any] => T,val defVal: T) extends Actor {
  var directParents   : List[ActorRef]                = List[ActorRef]()
  var newParents      : List[ActorRef]                = List[ActorRef]()
  var sourceMap       : AnyRefMap[ActorRef,List[ActorRef]]  = AnyRefMap[ActorRef,List[ActorRef]]()
  var directChildren  : List[ActorRef]                = List[ActorRef]()
  val defValues       : AnyRefMap[ActorRef,Any]             = AnyRefMap[ActorRef,Any]()
  val lastValues      : AnyRefMap[ActorRef,Any]             = AnyRefMap[ActorRef,Any]()
  var inputQueues     : AnyRefMap[ActorRef,IQSet]           = AnyRefMap[ActorRef,IQSet]()
  var instabilitySet  : List[ActorRef]                = List[ActorRef]()
  var updatesConfirmed : Int                          = 0
  var forwardUpdateRef : ActorRef                     = null
  //Not part of the algorithm, but needed given scala's actor system
  var initReceived    : Boolean                       = false
  var waitingSources  : List[(ParentRef,List[ActorRef])] = List[(ParentRef,List[ActorRef])]()
  var parentsReceived : Int                           = 0
  var startsReceived  : Int                           = 0
  var selfRef         : ParentRef                     = null
  
  def receive = {
    case InitMsg(directParents,directChildren) => handleInitMsg(directParents,directChildren)
    case DynamicInitMsg(directParents,directChildren) => handleDynamicInitMsg(directParents,directChildren)
    case SourcesMsg(from,sources) => handleSourcesMsg(from,sources)
    case StartMsg() => handleStartMsg()
    case PropagationValueMsg(value) => handlePropagationValueMsg(sender,value)
    case GetSourcesMsg(update) => handleGetSourcesMsg(sender,update)
    case RcvSourcesMsg(from,sources,defVal,update) => handleRcvSourcesMsg(from,sources,defVal,update)
    case UpdateSourcesMsg(from,newSources,updateDefVal) => handleUpdateSourcesMsg(from,newSources,updateDefVal)
    case SourcesUpdatedMsg() => handleSourcesUpdatedMsg()
    case AddChildMsg(newChild) => handleAddChildMsg(newChild)
    case DynamicParentMsg(newParent) => handleDynamicParentMsg(newParent)
    //USED FOR DEBUGGING
    case PrintParentsMsg() => handlePrintMsg()
    case PrintChildMsg() => handlePrintChildMsg()
  }
  
  protected def handleInitMsg(directParents : List[ActorRef], directChildren : List[ActorRef]) : Unit = {
    initReceived         = true
    this.directParents   = directParents
    this.directChildren  = directChildren
    this.selfRef         = new ParentRef(self,defVal)
    directParents.foreach{ parent =>
      inputQueues += (parent -> new IQSet())
    }
    waitingSources.foreach { case (directParent : ParentRef,sourcesList : List[ActorRef]) =>
      handleSourcesMsg(directParent,sourcesList)
    }
  }
  
  protected def handleDynamicInitMsg(directParents: List[ActorRef], directChildren : List[ActorRef]) : Unit = {
    this.directParents = directParents
    this.newParents    = directParents
    this.directChildren = directChildren
    this.selfRef = new ParentRef(self,defVal)
    directParents.foreach { parent =>  
      inputQueues += (parent -> new IQSet())
    }
    directParents.foreach { parent =>  
      parent ! new GetSourcesMsg(true)
    }
  }
  
  protected def getAllSources() : List[ActorRef] = {
    sourceMap.valuesIterator.toList.flatten
  }
  
  protected def sendSources() : Unit = {
    directChildren.foreach { directChild =>
      directChild ! new SourcesMsg(selfRef,getAllSources())
    }
  }
  
  protected def sendStart() : Unit = {
    directParents.foreach { directParent => 
      directParent ! new StartMsg() 
    }
  }
  
  protected def updateSourcesInfo(from : ParentRef, sources: List[ActorRef]) : Unit = {
    sourceMap.put(from.ref, sources)
    defValues += (from.ref -> from.defaultValue)
    sources.foreach { source =>
      (inputQueues.get(from.ref).get).addQueue(source)
    }
  }
  
  protected def handleSourcesMsg(directParent : ParentRef, sources : List[ActorRef]) :  Unit = {
    if(initReceived){
      parentsReceived += 1
      updateSourcesInfo(directParent,sources)
      //This actor has received the indirect parents from all of its direct parents, send the indirect parents to all direct children
      if(parentsReceived == directParents.length){
        sendSources()
        //Sink nodes initiate start phase as soon as they have received all parent sets
        if(directChildren.length == 0){
          sendStart()
        }
      }
   }
   else{
      waitingSources = (directParent,sources) +: waitingSources 
   }
  }
  
  protected def handleStartMsg() : Unit = {
    startsReceived += 1
    if(startsReceived == directChildren.length){
      sendStart()
    }
  }
  
  protected def propagate(value : PropagationValue[Any]) : Unit = {
    directChildren.foreach((child : ActorRef) => {
      child ! new PropagationValueMsg(value)
    })
  }
  
  protected def getArgumentPosition(queueOwner : ActorRef) : Integer = {
    directParents.indexOf(queueOwner)
  }
  
  protected def getPropagationArguments(newValueOrigin : ActorRef) : List[Any] = {
    var arguments : Array[Any] = Array.fill(directParents.length){0}
    inputQueues.foreach((kv : (ActorRef,IQSet)) => {
        val directParent : ActorRef = kv._1
        val queues       : IQSet    = kv._2
        if(queues.hasQueue(newValueOrigin)){
          val queue  : Queue[PropagationValue[Any]] = queues.getQueue(newValueOrigin)
          val value  : Any        = queue.dequeue().value
          arguments(getArgumentPosition(directParent)) = value
        }
        else{
          if(lastValues.contains(directParent)){
            arguments(getArgumentPosition(directParent)) = lastValues.get(directParent).get
          }
          else{
            arguments(getArgumentPosition(directParent)) = defValues.get(directParent).get
          }
        }
      })
    arguments.toList
  }
  
  protected def canStabilise(qs: List[Queue[PropagationValue[Any]]],valueOrigin : ActorRef) : Boolean = {
    var commonStamps : List[Integer] = List[Integer]()
    var foundCommon  : Boolean       = false
    //Get all time stamps for which each queue holds at least one value 
    qs.foreach { queue =>  
        queue.foreach { value =>  
          var found : Boolean = true
          qs.foreach { otherQueue => 
            if(!(otherQueue.equals(queue))){
              found = found && otherQueue.find { otherValue => otherValue.timeStamp == value.timeStamp}.isDefined
            }
          }
          if(found && !(commonStamps.contains(value.timeStamp))){
            commonStamps = commonStamps :+ value.timeStamp.asInstanceOf[Integer]
          }
          foundCommon = foundCommon || found
        }
    }
    //If there are such common time stamps, find the lowest and remove all values from queues of specified origin with lower time stamps
    if(foundCommon){
      val lowest = commonStamps.min
      qs.foreach { queue => 
        queue.dequeueAll { value => value.timeStamp < lowest }  
      }
      instabilitySet = instabilitySet.filter { actorRef => actorRef != valueOrigin }
    }
    foundCommon
  }
  
  protected def canPropagate(newValueOrigin : ActorRef) : Boolean = {
    var propagate : Boolean = true
    var qs : List[Queue[PropagationValue[Any]]] = List[Queue[PropagationValue[Any]]]()
    //Check if all queues holding values from newValueOrigin have at least one value queued (otherwise its unsafe to propagate)
    breakable{
        inputQueues.foreach((kv : (ActorRef,IQSet)) => {
        val directParent : ActorRef = kv._1
        val queues       : IQSet    = kv._2
        if(queues.hasQueue(newValueOrigin)){
          val queue : Queue[PropagationValue[Any]] = queues.getQueue(newValueOrigin)
          qs = queue +: qs
          if(queue.isEmpty){
            propagate = false
            break
          }
        }
      })
    }
    if(instabilitySet.contains(newValueOrigin)){
      propagate && canStabilise(qs,newValueOrigin)
    }
    else{
      propagate
    }
  }
  
  protected def handlePropagationValueMsg(from : ActorRef, value : PropagationValue[Any]) : Unit = {
    val valueOrigin : ActorRef   = value.origin
    val timeStamp   : Int        = value.timeStamp
    val inputQueue  : Queue[PropagationValue[Any]] = (inputQueues.get(from).get).getQueue(valueOrigin)
    inputQueue += value
    lastValues += (from -> value.value)
    val prop : Boolean = canPropagate(valueOrigin)
    if(prop){
      val updateArguments : List[Any] = getPropagationArguments(valueOrigin)
      val newNodeValue : T = nodeLambda(updateArguments)
      propagate(new PropagationValue(valueOrigin,newNodeValue,timeStamp))
    }
  }
  
  protected def handleGetSourcesMsg(from : ActorRef,update: Boolean) : Unit = {
    from ! new RcvSourcesMsg(selfRef,getAllSources(),defVal,update)
  }
  
  protected def handleRcvSourcesMsg(from: ParentRef, sources : List[ActorRef],dVal: Any,update : Boolean) : Unit = {
    parentsReceived += 1
    val mySources = getAllSources()
    defValues.put(from.ref, dVal)
    sources.foreach { source =>  
      if(mySources.contains(source) && !instabilitySet.contains(source)){
        instabilitySet = instabilitySet :+ source
      }
    }
    updateSourcesInfo(from,sources)
    if(parentsReceived == directParents.length){
        directChildren.foreach { directChild => 
          directChild ! new UpdateSourcesMsg(selfRef,getAllSources(),update)
        }
    }
  }
  
  protected def handleUpdateSourcesMsg(from : ParentRef, sources : List[ActorRef],updateDefVal : Boolean) : Unit = {
    forwardUpdateRef = from.ref
    val mySources = getAllSources()
    sources.foreach { source =>  
      if(mySources.contains(source) && !instabilitySet.contains(source)){
        instabilitySet = instabilitySet :+ source
      }
    }
    if(updateDefVal){
      inputQueues += (from.ref -> new IQSet())
      defValues   += (from.ref -> from.defaultValue)
      directParents = from.ref +: directParents 
    }
    updateSourcesInfo(from,sources)
    if(directChildren.length == 0){
      from.ref ! new SourcesUpdatedMsg()
    }
    else{
      directChildren.foreach { directChild =>
        directChild ! new UpdateSourcesMsg(selfRef,sources)
      }
    }
  }
  
  protected def handleSourcesUpdatedMsg() : Unit = { 
     updatesConfirmed += 1
     if(updatesConfirmed == directChildren.length){
       updatesConfirmed = 0
       //This node is the one that was dynamically added. All transient children have updated, notify parent to start streaming new values
       if(forwardUpdateRef == null){
         newParents.foreach { parent => 
           parent ! new AddChildMsg(selfRef.ref)  
         }
         newParents = List[ActorRef]()
       }
       else{
         forwardUpdateRef ! new SourcesUpdatedMsg()
       }
     }
  }
  
  protected def handleAddChildMsg(newChild : ActorRef) : Unit = {
    //if child is added to source node, need to make sure source node doesn't buffer update messages as a result of startsReceived != directChildren.length
    startsReceived += 1
    directChildren = newChild +: directChildren
  }
  
  protected def handleDynamicParentMsg(newParent: ActorRef) : Unit = {
    inputQueues.put(newParent,new IQSet())
    directParents =  newParent +: directParents
    newParents = List[ActorRef](newParent)
    newParent ! new GetSourcesMsg(false)
  }
  
  //USED FOR DEBUGGING PURPOSES 
  
  protected def handlePrintMsg()  = {
    inputQueues.foreach((kv : (ActorRef,IQSet)) => {
      ((kv._2).queues).foreach((oq: (ActorRef, Queue[PropagationValue[Any]])) =>
        println("Input queue for actor: " + self.path.name + " = " + kv._1.path.name + " <> " + oq._1.path.name)
      )
      }
    )
  }
  
  protected def handlePrintChildMsg() : Unit = {
    directChildren.foreach { child =>  
      println("Node : " + child.path.name + " has child "  + child.path.name)  
    }
  }
  
}

class SourceNode[T](override val nodeLambda: List[Any] => T,override val defVal: T) extends Node[T](nodeLambda,defVal) {
  //A source node only has itself as "parent" by default (i.e. it reacts to parent request by sending itself)
  sourceMap                          = AnyRefMap[ActorRef,List[ActorRef]](self -> List[ActorRef](self))
  val bufferedUpdates : Queue[T]     = new Queue[T]() 
  var timeStampCounter : Int             = 0
  
  override def receive = super.receive orElse {
    case UpdateMsg(value : List[Any]) => handleUpdateMsg(value)
  }
  
  protected override def handleInitMsg(directParents: List[ActorRef], directChildren: List[ActorRef]) : Unit = {
    super.handleInitMsg(directParents, directChildren)
    super.sendSources()
  }
  
  protected def initiatePropagation(value : T) : Unit = {
    val propValue = new PropagationValue[Any](self,value,timeStampCounter)
    directChildren.foreach((child : ActorRef) => {
       child ! new PropagationValueMsg(propValue)
    })
    timeStampCounter += 1
  }
  
  protected override def sendStart() : Unit = {
    //A source node has no parents to send "start" to, however we need to empty the buffer of values received before the node was ready to propagate
    bufferedUpdates.foreach((value : T) => {
      initiatePropagation(value)
    })
  }
  
  protected def handleUpdateMsg(value : List[Any]) : Unit = {
    val toPropagate : T = nodeLambda(value)
    if (startsReceived == directChildren.length){
      initiatePropagation(toPropagate)
    }
    else{
      bufferedUpdates += toPropagate
    }
  }
}

