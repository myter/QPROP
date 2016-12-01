package benchmarks
import graphGeneration._
import benchmarks._


//Propagate a single value (for each source node) through the graph and measure time needed to reach sink node (for QPROP) and admitter returning to idle state (SIDUP)
object SpeedBenchmarks {
  type Listener                                          = List[Long] => _
  var benchmarks                 : List[Benchmark]       = List[Benchmark]()
  var finishListener             : () => _               = null
  //Chain Results
  var QPROPChainSizeResults      : List[List[Long]]      = List[List[Long]]()
  var SIDUPChainSizeResults      : List[List[Long]]      = List[List[Long]]()
  var QPROPChainSourceResults    : List[List[Long]]      = List[List[Long]]()
  var SIDUPChainSourceResults    : List[List[Long]]      = List[List[Long]]()
  //Fan results
  var QPROPFanSizeResults        : List[List[Long]]      = List[List[Long]]()
  var SIDUPFanSizeResults        : List[List[Long]]      = List[List[Long]]()
  var QPROPFanSourceResults      : List[List[Long]]      = List[List[Long]]()
  var SIDUPFanSourceResults      : List[List[Long]]      = List[List[Long]]()
  //Complex results
  var QPROPComplexSizeResults    : List[List[Long]]      = List[List[Long]]()
  var SIDUPComplexSizeResults    : List[List[Long]]      = List[List[Long]]()
  var QPROPComplexSourceResults  : List[List[Long]]      = List[List[Long]]()
  var SIDUPComplexSourceResults  : List[List[Long]]      = List[List[Long]]()

  
  def run(completionListener : () => _) : Unit = {
    finishListener = completionListener
    //scheduleChain()
    //scheduleFan()
    scheduleComplex()
    runNext()
  }
  
  private def scheduleChain() : Unit = {
    val QPROPSizeFinishListener   : Listener       = {results : List[Long] => QPROPChainSizeResults   = QPROPChainSizeResults :+ results; runNext()}
    val SIDUPSizeFinishListener   : Listener       = {results : List[Long] => SIDUPChainSizeResults   = SIDUPChainSizeResults :+ results; runNext()}
    val QPROPSourceFinishListener : Listener       = {results : List[Long] => QPROPChainSourceResults = QPROPChainSourceResults :+ results; runNext()}
    val SIDUPSourceFinishListener : Listener       = {results : List[Long] => SIDUPChainSourceResults = SIDUPChainSourceResults :+ results; runNext()}
    //Vary graph sizes for single source node
    Benchmark.sizes.foreach { graphSize : Int =>
        val chain    : Graph            = new Chain(graphSize,Benchmark.defaultSources).generate()
        schedule(chain,QPROPSizeFinishListener,SIDUPSizeFinishListener)
    }
    //Vary source nodes for fixed graph size
   /* Benchmark.sources.foreach { sources : Int =>
      val chain      : Graph            = new Chain(Benchmark.defaultSize,sources).generate()
      schedule(chain,QPROPSourceFinishListener,SIDUPSourceFinishListener)
    }*/
  }
  
  private def scheduleFan() : Unit = {
    val QPROPSizeFinishListener   : Listener       = {results : List[Long] => QPROPFanSizeResults   = QPROPFanSizeResults :+ results; runNext()}
    val SIDUPSizeFinishListener   : Listener       = {results : List[Long] => SIDUPFanSizeResults   = SIDUPFanSizeResults :+ results; runNext()}
    val QPROPSourceFinishListener : Listener       = {results : List[Long] => QPROPFanSourceResults = QPROPFanSourceResults :+ results; runNext()}
    val SIDUPSourceFinishListener : Listener       = {results : List[Long] => SIDUPFanSourceResults = SIDUPFanSourceResults :+ results; runNext()}
    Benchmark.sizes.foreach { graphSize : Int => 
      val fan : Graph = new Fan(graphSize,Benchmark.defaultSources).generate()
      schedule(fan,QPROPSizeFinishListener,SIDUPSizeFinishListener)
    }
   /* Benchmark.sources.foreach { sources : Int =>
      val fan : Graph = new Fan(Benchmark.defaultSize,sources).generate()
      schedule(fan,QPROPSourceFinishListener,SIDUPSourceFinishListener)
    }*/
  }
  
  private def scheduleComplex() : Unit = {
    val QPROPSizeFinishListener   : Listener       = {results : List[Long] => QPROPComplexSizeResults   = QPROPComplexSizeResults :+ results; runNext()}
    val SIDUPSizeFinishListener   : Listener       = {results : List[Long] => SIDUPComplexSizeResults   = SIDUPComplexSizeResults :+ results; runNext()}
    val QPROPSourceFinishListener : Listener       = {results : List[Long] => QPROPComplexSourceResults = QPROPComplexSourceResults :+ results; runNext()}
    val SIDUPSourceFinishListener : Listener       = {results : List[Long] => SIDUPComplexSourceResults = SIDUPComplexSourceResults :+ results; runNext()}
    Benchmark.sizes.foreach { graphSize : Int => 
      val complex : Graph = new Complex(Benchmark.defaultComplex,graphSize,Benchmark.defaultSources).generate()
      schedule(complex,QPROPSizeFinishListener,SIDUPSizeFinishListener)
    }
    /*Benchmark.sources.foreach { sources : Int =>
      val complex : Graph = new Complex(Benchmark.defaultComplex,Benchmark.defaultSize,sources).generate()
      schedule(complex,QPROPSourceFinishListener,SIDUPSourceFinishListener)
    }*/
  }
  
  private def schedule(graph : Graph, QPROPListener : Listener, SIDUPListener : Listener) : Unit = {
    val qBench   : QPROPBenchmark   = new QPROPBenchmark(graph,Benchmark.warmUps,Benchmark.runs,Benchmark.defaultEvents,QPROPListener)
    val sBench   : SIDUPBenchmark   = new SIDUPBenchmark(true,graph,Benchmark.warmUps,Benchmark.runs,Benchmark.defaultEvents,SIDUPListener)
    benchmarks                      = benchmarks ++ List[Benchmark](qBench,sBench)
  }
  
  var counter = 0
  
  private def runNext() : Unit = {
    if(counter.%(2) == 0){
      println("Running QPROP, speed benchmark nr : " + counter)
    }
    else{
      println("Running SIDUP,speed benchmark nr : " + counter)
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
      prettyPrint()
    }
  }
  
  //Used for debugging
  private def average(results : List[Long]) : Long = {
    (results.foldLeft(0.asInstanceOf[Long])((acc : Long, value : Long) => acc + value)) / results.length
  }
  
  private def prettyPrint() : Unit = {
    (Benchmark.sizes.zipWithIndex).foreach { case ( size : Int, index : Int) =>
      println("Results for size : " + size)
      println("===========================")
      println("Results for Complex")
      val QPROPRes : List[Long] = QPROPComplexSizeResults(index)
      val SIDUPRes : List[Long] = SIDUPComplexSizeResults(index)
      println("QPROP Result : " +  average(QPROPRes))
      println("SIDUP Result : " + average(SIDUPRes))
    }
  }
}