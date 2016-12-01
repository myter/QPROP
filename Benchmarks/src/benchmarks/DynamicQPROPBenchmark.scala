package benchmarks
import graphGeneration._
import translators._
import QPROPS.algorithm._

class DynamicQPROPBenchmark(graph : Graph, warmUps : Int, runs : Int, events : Int, finishListener : List[Long] => _ = {results : List[Long] => null},dependencies : Int) extends QPROPBenchmark(graph : Graph, warmUps : Int, runs : Int, events : Int, finishListener  = {results : List[Long] => null}) {
  /*override val intermediateLambda : List[_] => _                     = {args : List[_] => args}
  override val sourceLambda       : List[_] => _                     = {args : List[_] => args}
  override val sinkLambda         : List[_] => _                     = {_ : List[_] => this.valueProcessed(System.nanoTime())}*/
  
  protected override def startRuns() : Unit = {
    println("Starting run : " + completedRuns + " dependencies: " + dependencies)
    translator.cleanUp()
    val previousEdges = translator.addedEdges
    translator = new QPROPTranslator(intermediateLambda,sourceLambda,sinkLambda)
    updater    = translator.translate(graph)
    translator.reverse(previousEdges)
    startTime = System.nanoTime()
    if(!(completedRuns == runs)){
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
  
  protected override def valueProcessed(stopTime : Long) : Unit = {
    if(warmedUp){
      runEvents  += 1
      if(runEvents == graph.getSources().length * events){
        completedRuns       += 1
        results              = (stopTime - startTime) +: results
        runEvents            = 0
        startRuns()
      }
      else if(runEvents % ((events * graph.getSources().length) / dependencies) == 0){
        translator.dynEdge()
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
}