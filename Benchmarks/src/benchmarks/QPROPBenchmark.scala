package benchmarks
import graphGeneration._
import translators._
import QPROPS.algorithm._

class QPROPBenchmark(graph : Graph, warmUps : Int, runs : Int, events : Int, finishListener : List[Long] => _ = {results : List[Long] => null}) extends Benchmark{
  val intermediateLambda : List[_] => _                     = {args : List[_] => args}
  val sourceLambda       : List[_] => _                     = {args : List[_] => args}
  val sinkLambda         : List[_] => _                     = {_ : List[_] => this.valueProcessed(System.nanoTime())}
  var translator         : QPROPTranslator                  = null
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
  
  
  protected def startRuns() : Unit = {
    startTime = System.nanoTime()
    if(!(completedRuns == runs)){
      println("Starting run : " + completedRuns)
      //Let each source node propagate the correct amount of events 
      val sources : List[API.NodeId] = graph.getSources().map{ sourceId : Graph.NodeId => translator.mapNode(sourceId)}
      sources.foreach { source : API.NodeId =>  
        for(i <- 1 to events){
          updater(List[API.NodeId](source),List[Any](null))
        }
      }
    }
    else{
      println("Runs completed")
      translator.cleanUp()
      finishListener(results)
    }
  }
  
  //Method called by sink node after processing a value
  protected def valueProcessed(stopTime : Long) : Unit = {
    if(warmedUp){
      runEvents  += 1
      if(runEvents == graph.getSources().length * events){
        completedRuns       += 1
        results              = (stopTime - startTime) +: results
        runEvents            = 0
        startRuns()
      }
    }
    else{
      warmUpEvents += 1
      if(warmUpEvents == graph.getSources().length * warmUps){
        warmedUp = true
        println("Warmup complete")
        startRuns()
      }
    }
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
    println("Starting benchmark for graph size : " + graph.nodes.size + " sources: " + graph.getSources().length + " events: " + events)
    translator = new QPROPTranslator(intermediateLambda,sourceLambda,sinkLambda)
    updater    = translator.translate(graph)
    warmup()
  }
  
  
}