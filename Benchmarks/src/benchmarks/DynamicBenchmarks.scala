package benchmarks
import graphGeneration._
object DynamicBenchmarks {
  type Listener                                          = List[Long] => _
  var benchmarks                 : List[Benchmark]       = List[Benchmark]()
  var finishListener             : () => _               = null
  //Complex results
  var QPROPComplexResults        : List[List[Long]]      = List[List[Long]]()
  var SIDUPComplexResults        : List[List[Long]]      = List[List[Long]]()
  //Temp
  var edges : Int = 0

  
  def run(completionListener : () => _) : Unit = {
    this.edges = edges
    finishListener = completionListener
    scheduleComplex()
    runNext()
  }
  
  private def scheduleComplex() : Unit = {
    val QPROPFinishListener   : Listener       = {results : List[Long] => QPROPComplexResults   =   QPROPComplexResults :+ results; runNext()}
    val SIDUPFinishListener   : Listener       = {results : List[Long] => SIDUPComplexResults   =   SIDUPComplexResults :+ results; runNext()}
    val complex               : Graph          = new Complex(Benchmark.defaultComplex,Benchmark.dynamicSize,Benchmark.defaultSources).generate()
    Benchmark.dynDependencies.foreach { deps : Int =>
        schedule(complex,QPROPFinishListener,SIDUPFinishListener,deps)
    }
  }
  
  private def schedule(graph : Graph, QPROPListener : Listener, SIDUPListener : Listener, dependencies : Int) : Unit = {
    val qBench   : QPROPBenchmark   = new DynamicQPROPBenchmark(graph,Benchmark.warmUps,Benchmark.runs,Benchmark.dynamicEvents,QPROPListener,dependencies)
    val sBench   : SIDUPBenchmark   = new DynamicSIDUPBenchmark(true,graph,Benchmark.warmUps,Benchmark.runs,Benchmark.dynamicEvents,SIDUPListener,dependencies)
    benchmarks                      = benchmarks ++ List[Benchmark](qBench,sBench)
  }
  
  var counter = 0
  
  private def runNext() : Unit = {
    if(counter.%(2) == 0){
      println("Running QPROP,Dynamic benchmark nr : " + counter)
    }
    else{
      println("Running SIDUP, Dynamic benchmark nr : " + counter)
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