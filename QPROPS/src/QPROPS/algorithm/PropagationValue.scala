package QPROPS.algorithm
import akka.actor.ActorRef

class PropagationValue[T](val origin : ActorRef,val value: T, val timeStamp: Int)