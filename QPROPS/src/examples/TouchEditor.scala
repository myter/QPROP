package examples
import QPROPS.algorithm._
import java.awt.MouseInfo
import akka.actor._
import scala.concurrent.duration._
import scala.concurrent._

class MouseTracker extends ObservableObject {
  val actorSystem = ActorSystem()
  val scheduler = actorSystem.scheduler
  val track = this
  val r = scala.util.Random
  val task = new Runnable { def run() { 
      //simulate mouse positions
      track.valueChanged(r.nextInt(500))
    } 
  }
  implicit val executor = actorSystem.dispatcher
  scheduler.schedule(5 seconds, 10 seconds, task)
}

class Touchpad extends ObservableObject{
  val actorSystem = ActorSystem()
  val scheduler = actorSystem.scheduler
  val track = this
  val r = scala.util.Random
  val task = new Runnable { def run() { 
      //Simulate trackpad events
      track.valueChanged(r.nextPrintableChar())
    } 
  }
  implicit val executor = actorSystem.dispatcher
  scheduler.schedule(3 seconds, 14 seconds, task)
}


object TouchEditor extends App {
  val mouseXId : API.NodeId = API.newSource(new MouseTracker, 0)
  val mouseYId : API.NodeId = API.newSource(new MouseTracker, 0)
  val touchpadId : API.NodeId = API.newSource(new Touchpad, null)
  val charReqId : API.NodeId = API.newNode({args => args.head.asInstanceOf[Character]}, null)
  def mouseUL(args : List[Any]) : (Integer,Integer) = {
      val x = args(0).asInstanceOf[Integer]
      val y = args(1).asInstanceOf[Integer]
      (x,y)
  }
  val mousePosId  : API.NodeId = API.newNode(mouseUL, (0,0))
  def editorUL(args : List[Any]) : Unit = {
    val mousePos : (Integer,Integer) = args(1).asInstanceOf[(Integer,Integer)]
    val char : Character = args(0).asInstanceOf[Character]
    println("Mouse position: " + mousePos + " recognised character : " + char)
  }
  def editorId : API.NodeId = API.newNode(editorUL,null)
  API.->(touchpadId,charReqId)
    API.=->(List(mouseXId,mouseYId),mousePosId)
  API.=->(List(mousePosId,charReqId),editorId)
  API.<>()
}