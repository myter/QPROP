package SIDUP.algorithm
import scala.collection.mutable.Map
import akka.actor._

case class ChangeSources(sources : List[Admitter.NodeRef], values : List[_])
case class AddDependency(sources : List[Admitter.NodeRef], from: Admitter.NodeRef, to: Admitter.NodeRef)
case class SinkReady()

//Coordinates access to propagation graph, callback is invoked when propagation phase has finished
class Admitter(sources : List[Admitter.NodeRef] , sinks : List[Admitter.NodeRef], callback : () => _) extends Actor{
  val termination        : DijkstraScholten                         = new DijkstraScholten(() => returnToIdle())
  var idle               : Boolean                                  = true
  var constructionReady  : Boolean                                  = false
  var sinksReady         : Integer                                  = 0
  var waitingPulses      : List[(List[Admitter.NodeRef],List[_])]   = List[(List[Admitter.NodeRef],List[_])]()
  var waitingChanges     : List[(List[Admitter.NodeRef],Admitter.NodeRef,Admitter.NodeRef)] = List[(List[Admitter.NodeRef],Admitter.NodeRef,Admitter.NodeRef)]()
  
  def receive = {
    case ChangeSources(sources : List[Admitter.NodeRef], values : List[_]) => handleChangeSources(sources,values)
    case AddDependency(sources : List[Admitter.NodeRef], from : Admitter.NodeRef,to : Admitter.NodeRef)      => handleAddDependency(sources,from,to)
    case SinkReady()                                                       => handleSinkReady()
    case AckMessage()                                                      => handleAckMessage()
  }
  
  protected def handleChangeSources(sources : List[Admitter.NodeRef], values : List[_]) : Unit = {
    if(constructionReady && idle){
      sendPulse(sources,values)
    }
    else{
      waitingPulses = (sources,values) +: waitingPulses
    }
  }
  
  protected def handleAddDependency(sources : List[Admitter.NodeRef], from : Admitter.NodeRef,to : Admitter.NodeRef) : Unit = {
    if(constructionReady && idle){
      sendDependencyChange(sources,from,to)
    }
    else{
      waitingChanges = (sources,from,to) +: waitingChanges
    }
  }
  
  protected def sendPulse(sources : List[Admitter.NodeRef], values : List[_]) : Unit = {
    idle = false
    (sources.zipWithIndex).foreach{ case (sourceRef,index) =>
      termination.newChildMessage()
      val newPulse : PulseValue = new PulseValue(sources.toSet,values(index))
      sourceRef ! new UpdateValue(newPulse)
    }
  }
  
  protected def sendDependencyChange(sources : List[Admitter.NodeRef], from : Admitter.NodeRef,to : Admitter.NodeRef) : Unit = {
    idle = false
    (sources.zipWithIndex).foreach{ case (sourceRef,index) =>
      termination.newChildMessage()
      val changePulse : ChangeValue = new ChangeValue(from,to)
      sourceRef ! new UpdateChangeValue(changePulse)
    }
  }
  
  protected def handleAckMessage() : Unit = {
    termination.newAckMessage()
  }
  
  protected def handleSinkReady() : Unit = {
    sinksReady +=1
    if(sinksReady == sinks.length){
      constructionReady = true
      //send first buffered pulse, termination will make sure the rest is sent after the turn has finished (one-by-one)
      sendBufferedPulse()
    }
  }
  
  protected def sendBufferedPulse() : Unit = {
    if(waitingChanges.length > 0){
        val (sources : List[Admitter.NodeRef], from : Admitter.NodeRef, to : Admitter.NodeRef) = waitingChanges.last
        sendDependencyChange(sources,from,to)
        waitingChanges = waitingChanges.take(waitingChanges.length - 1)
    }
    else if(waitingPulses.length > 0){
      val (sources : List[Admitter.NodeRef], values : List[_]) = waitingPulses.last
      sendPulse(sources,values)
      //remove pulse just sent
      waitingPulses = waitingPulses.take(waitingPulses.length - 1)
    }
  }
  
  //Termination has signalled that propagation phase has finished, able to send messages again (or send waiting message)
  protected def returnToIdle() : Unit = {
    idle = true
    callback()
    sendBufferedPulse()
  }
  
}

object Admitter {
  type SourceId     = Integer
  type NodeRef      = ActorRef
  type AdmitterRef  = ActorRef
}