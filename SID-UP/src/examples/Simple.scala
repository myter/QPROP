package examples
import SIDUP.algorithm._
import akka.actor._

object Simple {
  def main(args: Array[String]) : Unit = {
  val system = ActorSystem()
  
  
  val source  = system.actorOf(Props(new SourceNode[Integer](args => args(0).asInstanceOf[Integer])), name = "source")
  val aNode   = system.actorOf(Props(new Node[Integer](args => args(0).asInstanceOf[Integer],List[Admitter.NodeRef](source))), name = "aNode")
  val bNode   = system.actorOf(Props(new Node[Integer](args => args(0).asInstanceOf[Integer],List[Admitter.NodeRef](source))), name = "bNode")
  val sink    = system.actorOf(Props(new SinkNode[Unit](args => println("Received : " + args(0) + " , " + args(1)),List[Admitter.NodeRef](aNode,bNode))), name = "sink")
  val adm     = system.actorOf(Props(new Admitter(List[Admitter.NodeRef](source),List[Admitter.NodeRef](sink),{() => null})))
  source ! new Start()
  aNode ! new Start()
  bNode ! new Start()
  sink ! new StartSink(adm)
  adm ! new ChangeSources(List[Admitter.NodeRef](source),List(5))
  }
}