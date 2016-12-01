package benchmarks
import graphGeneration._

//Show that part of QPROP's advantage over SIDUP is the lack of turns (i.e. no termination detection needed)
object TurnBenchmark {
  type Listener                                          = List[Long] => _
  var benchmarks                 : List[Benchmark]       = List[Benchmark]()
  var finishListener             : () => _               = null
  //Chain Results
  var QPROPChainResults          : List[List[Long]]      = List[List[Long]]()
  var SIDUPChainResults          : List[List[Long]]      = List[List[Long]]()
  //Complex results
  var QPROPComplexResults        : List[List[Long]]      = List[List[Long]]()
  var SIDUPComplexResults        : List[List[Long]]      = List[List[Long]]()


  
  def run(completionListener : () => _) : Unit = {
    finishListener                      = completionListener
    val QPROPChainListener   : Listener = {results : List[Long] => QPROPChainResults   = QPROPChainResults :+ results ; runNext()}
    val QPROPComplexListener : Listener = {results : List[Long] => QPROPComplexResults = QPROPComplexResults :+ results ; runNext()}
    val SIDUPChainListener   : Listener = {results : List[Long] => SIDUPChainResults   = SIDUPChainResults :+ results ; runNext()}
    val SIDUPComplexListener : Listener = {results : List[Long] => SIDUPComplexResults = SIDUPComplexResults :+ results ; runNext()}
    val chain                : Graph = new Chain(Benchmark.defaultSize,Benchmark.defaultSources).generate()
    val complex              : Graph = new Complex(Benchmark.defaultComplex, Benchmark.defaultSize ,Benchmark.defaultSources).generate()
    Benchmark.events.foreach { events : Int => 
      //schedule(chain,QPROPChainListener,SIDUPChainListener,events)
      schedule(complex,QPROPComplexListener,SIDUPComplexListener,events)
    }
    runNext()
  }
  
  private def schedule(graph : Graph, QPROPListener : Listener, SIDUPListener : Listener, events : Int) : Unit = {
    val qBench   : QPROPBenchmark   = new QPROPBenchmark(graph,Benchmark.warmUps,Benchmark.runs,events,QPROPListener)
    val sBench   : SIDUPBenchmark   = new SIDUPBenchmark(false,graph,Benchmark.warmUps,Benchmark.runs,events,SIDUPListener)
    benchmarks                      = benchmarks ++ List[Benchmark](qBench,sBench)
  }
  
  var counter = 0
  
  private def runNext() : Unit = {
    if(counter.%(2) == 0){
      println("Running QPROP, turn benchmark nr : " + counter)
    }
    else{
      println("Running SIDUP, turn benchmark nr : " + counter)
    }
    if(benchmarks.length > 0){
      println("Benchmarks remaining = " + benchmarks.length)
      val next : Benchmark = benchmarks(0)
      benchmarks           = benchmarks.drop(1)
      println("Starting")
      counter += 1
      next.start()
    }
    else{
      println("Benchmark finished")
      finishListener()
    }
  }
}