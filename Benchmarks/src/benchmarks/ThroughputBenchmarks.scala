package benchmarks
import graphGeneration._

//Measure how long it takes for each algorithm to propagate a number of events
object ThroughputBenchmarks {
  type Listener                                          = List[Long] => _
  var benchmarks                 : List[Benchmark]       = List[Benchmark]()
  var finishListener             : () => _               = null
  //Chain Results
  var QPROPChainResults          : List[List[Long]]      = List[List[Long]]()
  var SIDUPChainResults          : List[List[Long]]      = List[List[Long]]()
  //Fan results
  var QPROPFanResults            : List[List[Long]]      = List[List[Long]]()
  var SIDUPFanResults            : List[List[Long]]      = List[List[Long]]()
  //Complex results
  var QPROPComplexResults        : List[List[Long]]      = List[List[Long]]()
  var SIDUPComplexResults        : List[List[Long]]      = List[List[Long]]()

  
  def run(completionListener : () => _) : Unit = {
    finishListener = completionListener
    //scheduleChain()
    //scheduleFan()
    scheduleComplex()
    runNext()
  }
  
  private def scheduleChain() : Unit = {
    val QPROPFinishListener   : Listener       = {results : List[Long] => QPROPChainResults   = QPROPChainResults :+ results; runNext()}
    val SIDUPFinishListener   : Listener       = {results : List[Long] => SIDUPChainResults   = SIDUPChainResults :+ results; runNext()}
    val chain                 : Graph          = new Chain(Benchmark.defaultSize,Benchmark.defaultSources).generate()
    Benchmark.events.foreach { events : Int => 
      schedule(chain,QPROPFinishListener,SIDUPFinishListener,events)
    }
  }
  
  private def scheduleFan() : Unit = {
    val QPROPFinishListener   : Listener       = {results : List[Long] => QPROPFanResults   = QPROPFanResults :+ results; runNext()}
    val SIDUPFinishListener   : Listener       = {results : List[Long] => SIDUPFanResults   = SIDUPFanResults :+ results; runNext()}
    val fan                   : Graph          = new Fan(Benchmark.defaultSize,Benchmark.defaultSources).generate()
    Benchmark.events.foreach { events : Int => 
        schedule(fan,QPROPFinishListener,SIDUPFinishListener,events)
    }
  }
  
  private def scheduleComplex() : Unit = {
    val QPROPFinishListener   : Listener       = {results : List[Long] => QPROPComplexResults   = QPROPComplexResults :+ results; runNext()}
    val SIDUPFinishListener   : Listener       = {results : List[Long] => SIDUPComplexResults   =   SIDUPComplexResults :+ results; runNext()}
    val complex               : Graph          = new Complex(Benchmark.defaultComplex,Benchmark.defaultSize,Benchmark.defaultSources).generate()
    Benchmark.events.foreach { events : Int => 
        schedule(complex,QPROPFinishListener,SIDUPFinishListener,events)
    }
  }
  
  private def schedule(graph : Graph, QPROPListener : Listener, SIDUPListener : Listener, events : Int) : Unit = {
    val qBench   : QPROPBenchmark   = new QPROPBenchmark(graph,Benchmark.warmUps,Benchmark.runs,events,QPROPListener)
    val sBench   : SIDUPBenchmark   = new SIDUPBenchmark(true,graph,Benchmark.warmUps,Benchmark.runs,events,SIDUPListener)
    benchmarks                      = benchmarks ++ List[Benchmark](qBench,sBench)
  }
  
  var counter = 0
  
  private def runNext() : Unit = {
    if(counter.%(2) == 0){
      println("Running QPROP,throughput benchmark nr : " + counter)
    }
    else{
      println("Running SIDUP, troughput benchmark nr : " + counter)
    }
    if(benchmarks.length > 0){
      println("Benchmarks remaining = " + benchmarks.length)
      val next : Benchmark = benchmarks(0)
      benchmarks           = benchmarks.drop(1)
      counter += 1
      next.start()
    }
    else{
      println("Benchmark finished")
      finishListener()
    }
  }
  

}