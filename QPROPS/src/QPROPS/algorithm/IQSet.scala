package QPROPS.algorithm
import akka.actor.ActorRef
import scala.collection.mutable.Queue

class IQSet {
  var queues : Map[ActorRef,Queue[PropagationValue[Any]]] = Map[ActorRef,Queue[PropagationValue[Any]]]()
  
  def addQueue(origin : ActorRef) : Unit = {
    queues += (origin -> new Queue[PropagationValue[Any]]())
  }
  
  def hasQueue(origin: ActorRef) : Boolean = {
    queues.contains(origin)
  }
  
  def getQueue(origin: ActorRef) : Queue[PropagationValue[Any]] = {
    val queue : Option[Queue[PropagationValue[Any]]] = queues.get(origin)
    queue.get
  }
  
}