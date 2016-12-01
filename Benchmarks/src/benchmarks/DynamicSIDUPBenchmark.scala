package benchmarks
import graphGeneration._
import translators._
import SIDUP.algorithm._

class DynamicSIDUPBenchmark(finishOnAdmitter : Boolean,var graph : Graph, warmUps : Int, runs : Int, events : Int, finishListener : List[Long] => _ = {results : List[Long] => null},dependencies : Int) extends SIDUPBenchmark(finishOnAdmitter : Boolean, graph : Graph, warmUps : Int, runs : Int, events : Int, finishListener = {results : List[Long] => null}) {
    var graphCopies : List[Graph] = List[Graph]()
  for(i <- 0 to runs){
    graphCopies = graph.copy() +: graphCopies
  } 
  
  protected override def startRuns() : Unit = {
    println("Starting run : " + completedRuns + " dependencies: " + dependencies)
    translator.cleanUp()
    val previousEdges = translator.addedEdges
    translator = new SIDUPTranslator(admitterCallback,intermediateLambda,sourceLambda,sinkLambda)
    updater    = translator.translate(graph)
    translator.reverse(previousEdges)
    println("Translator done")
    startTime = System.nanoTime()
    if(!(completedRuns == runs)){
      if(finishOnAdmitter){
        //Let each source node propagate the correct amount of events 
        val sources : List[API.NodeId] = graph.getSources().map{ sourceId : Graph.NodeId => translator.mapNode(sourceId)}
        sources.foreach { source : API.NodeId =>  
          for(i <- 1 to events){
            updater(List[API.NodeId](source),List[Any](null))
          }
        }
      }
      else{
        //Bypass admitter by letting all nodes propagate at a single event at the exact same time
        val sources : List[API.NodeId] = graph.getSources().map{ sourceId : Graph.NodeId => translator.mapNode(sourceId)}
        val values  : List[Any]        = sources.map{sourceID : API.NodeId => null}
        for(i <- 1 to events){
            updater(sources,values)
        }
      }
    }
    else{
      println("Runs completed")
      translator.cleanUp()
      finishListener(results)
    }
  }
  protected override def turnFinished(stopTime : Long) : Unit = {
    if(warmedUp){
      runEvents  += 1
      var runCompleted : Boolean = false
      if(finishOnAdmitter){
        println("Turn finished: " + runEvents)
        runCompleted = (runEvents == graph.getSources().length * events)
        if(runEvents % ((events * graph.getSources().length) / dependencies) == 0){
          translator.dynEdge()
        }
      }
      else{
        runCompleted = (runEvents == events)
      }
      if(runCompleted){
        completedRuns       += 1
        results              = (stopTime - startTime) +: results
        runEvents            = 0
        startRuns()
      }
    }
    else{
      warmUpEvents += 1
      //Since warmups change all sources simultaneously events = times to warmup for SIDUP benchmark
      if(warmUpEvents == warmUps){
        warmedUp = true
        println("Warmup complete")
        startRuns()
      }
    }    
  }
  
}