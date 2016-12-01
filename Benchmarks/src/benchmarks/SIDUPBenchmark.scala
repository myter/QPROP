package benchmarks
import graphGeneration._
import translators._
import SIDUP.algorithm._

class SIDUPBenchmark(finishOnAdmitter : Boolean, graph : Graph, warmUps : Int, runs : Int, events : Int, finishListener : List[Long] => _ = {results : List[Long] => null}) extends Benchmark {
val intermediateLambda : List[_] => _                       = {args : List[_] => args}
  val sourceLambda       : List[_] => _                     = {args : List[_] => args}
  var sinkLambda         : List[_] => _                     = null
  var admitterCallback   : () => _                          = null
  var translator         : SIDUPTranslator                  = null
  var updater            : (List[API.NodeId],List[_]) => _  = null
  var startTime          : Long                             = -1
  var warmedUp           : Boolean                          = false
  //Number of warmup events processed
  var warmUpEvents       : Int                              = 0
  //Number of currently completed runs
  var completedRuns      : Int                              = 0
  //Number of run events processed for current run
  var runEvents          : Int                              = 0
  //Raw results
  var results            : List[Long]                       = List[Long]()
  
  //Used to showcase that part of SIDUP's underperformance stems from the turn system
  if(finishOnAdmitter){
    sinkLambda       = {args : List[_] => args}
    admitterCallback = () => turnFinished(System.nanoTime())

  }
  else{
    sinkLambda       = {args : List[_] => valueProcessed(System.nanoTime())}
    admitterCallback = () => null
  }
  
  protected def startRuns() : Unit = {
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
  
  //Admitter has signaled that a turn has completed
  protected def turnFinished(stopTime : Long) : Unit = {
    if(warmedUp){
      runEvents  += 1
      var runCompleted : Boolean = false
      if(finishOnAdmitter){
        runCompleted = (runEvents == graph.getSources().length * events)
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
  
  //Called by sink node in case termination has been set to a sink node processing a value, not the admitter return to idle
  private def valueProcessed(stopTime : Long) : Unit = {
    turnFinished(stopTime)
  }
  
  //Let all sources propagate a single value through the graph for the given amount of warmup runs
  private def warmup() : Unit = {
    for(i <- 1 to warmUps){
      val sources : List[API.NodeId] = graph.getSources().map { sourceId : Graph.NodeId => translator.mapNode(sourceId)}
      //Arguments do not matter for benchmarking purposes
      val args    : List[Any]        = sources.map {source : API.NodeId => null}
      updater(sources,args)
    }
  }
  
  def start() : Unit = {
    translator       = new SIDUPTranslator(admitterCallback,intermediateLambda,sourceLambda,sinkLambda)
    updater          = translator.translate(graph)
    warmup()
  }
}