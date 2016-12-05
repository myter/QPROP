package QPROPS.algorithm
import akka.actor._

trait ObservableObject {
  var registeredSources : List[ActorRef] = List[ActorRef]()
  
  def registerSource(source : ActorRef) : Unit = {
    registeredSources = source +: registeredSources
  }
  
  def valueChanged(newVal : Any) : Unit = {
    registeredSources.foreach { source : ActorRef => 
      source ! new UpdateMsg(newVal)
    }
  }
  
}