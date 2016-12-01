package benchmarks

object Main {
  var speedPath      : String         = null
  var throughputPath : String         = null
  var turnPath       : String         = null
  var dynamicPath    : String         = null
  var sizeSchema     : Array[String]  = Array("Size","Mean","LowerBound","UpperBound")
  var sourceSchema   : Array[String]  = Array("Sources","Mean","LowerBound","UpperBound")
  var eventSchema    : Array[String]  = Array("Events","Mean","LowerBound","UpperBound")
  var dynamicSchema  : Array[String]  = Array("Dependencies","Mean","LowerBound","UpperBound")
  
  def recordify(results : List[List[Long]], resultType : List[Int]) : List[Array[String]] = {
    var records : List[Array[String]] = List[Array[String]]()
    (results.zipWithIndex).foreach { case (results : List[Long], index : Int) =>
      val resType    : Int            = resultType(index)
      val mean       : Double         = Benchmark.calcMean(results)
      val lowerBound : Double         = Benchmark.calcLowerBound(results)
      val upperBound : Double         = Benchmark.calcUpperBound(results)
      val record     : Array[String]  = Array[String](resType.toString(),mean.toString(),lowerBound.toString(),upperBound.toString())
      records                         = records :+ record
    }
    records
  }
  
  /*def gatherAndWrite(path : String, topoName : String, QPROPSizeRes : List[List[Long]], SIDUPSizeRes : List[List[Long]], QPROPSourceRes : List[List[Long]], SIDUPSourceRes : List[List[Long]]) : Unit = {
    //Write out results for varying sizes
    var QPROPSizes   : List[Array[String]] = recordify(QPROPSizeRes,Benchmark.sizes)
    ResultWriter.write(path + "/" + topoName + "QPROPsize.csv", sizeSchema, QPROPSizes)
    var SIDUPSizes   : List[Array[String]] = recordify(SIDUPSizeRes,Benchmark.sizes)
    ResultWriter.write(path + "/" + topoName + "SIDUPsize.csv", sizeSchema, SIDUPSizes)
    //write out results for varying amounts of sources
    var QPROPSources : List[Array[String]] = recordify(QPROPSourceRes,Benchmark.sources)
    ResultWriter.write(path +  "/" + topoName + "QPROPsource.csv", sourceSchema, QPROPSources)
    var SIDUPSources : List[Array[String]] = recordify(SIDUPSourceRes,Benchmark.sources)
    ResultWriter.write(path +  "/" + topoName + "SIDUPsource.csv", sourceSchema, SIDUPSources)
  }*/
  
  def speedFinished() : Unit = {
    //Gather & write results for Chain benchmarks
    //gatherAndWrite(speedPath,"Chain",SpeedBenchmarks.QPROPChainSizeResults,SpeedBenchmarks.SIDUPChainSizeResults,SpeedBenchmarks.QPROPChainSourceResults,SpeedBenchmarks.SIDUPChainSourceResults)
    //Gather & write results for Fan benchmarks
    //gatherAndWrite(speedPath,"Fan",SpeedBenchmarks.QPROPFanSizeResults,SpeedBenchmarks.SIDUPFanSizeResults,SpeedBenchmarks.QPROPFanSourceResults,SpeedBenchmarks.SIDUPFanSourceResults)
    //Gather & write results for Complex benchmarks
    //gatherAndWrite(speedPath,"Complex",SpeedBenchmarks.QPROPComplexSizeResults,SpeedBenchmarks.SIDUPComplexSizeResults,SpeedBenchmarks.QPROPComplexSourceResults,SpeedBenchmarks.SIDUPComplexSourceResults)
    var QPROPComplex : List[Array[String]] = recordify(SpeedBenchmarks.QPROPComplexSizeResults,Benchmark.sizes)
    ResultWriter.write(speedPath + "/" + "QPROPComplex.csv", sizeSchema, QPROPComplex)
    var SIDUPComplex : List[Array[String]] = recordify(SpeedBenchmarks.SIDUPComplexSizeResults,Benchmark.sizes)
    ResultWriter.write(speedPath + "/" + "SIDUPComplex.csv",sizeSchema,SIDUPComplex)
    TurnBenchmark.run(turnFinished)
  }
  
  def throughputFinished() : Unit = {
    /*var QPROPChain   : List[Array[String]] = recordify(ThroughputBenchmarks.QPROPChainResults,Benchmark.events)
    ResultWriter.write(throughputPath + "/" + "QPROPChain.csv", eventSchema, QPROPChain)
    var SIDUPChain   : List[Array[String]] = recordify(ThroughputBenchmarks.SIDUPChainResults,Benchmark.events)
    ResultWriter.write(throughputPath + "/" + "SIDUPChain.csv", eventSchema, SIDUPChain)
    var QPROPFan     : List[Array[String]] = recordify(ThroughputBenchmarks.QPROPFanResults,Benchmark.events)
    ResultWriter.write(throughputPath + "/" + "QPROPFan.csv", eventSchema, QPROPFan)
    var SIDUPFan     : List[Array[String]] = recordify(ThroughputBenchmarks.SIDUPFanResults,Benchmark.events)
    ResultWriter.write(throughputPath + "/" + "SIDUPFan.csv", eventSchema, SIDUPFan)*/
    var QPROPComplex : List[Array[String]] = recordify(ThroughputBenchmarks.QPROPComplexResults,Benchmark.events)
    ResultWriter.write(throughputPath + "/" + "QPROPComplex.csv", eventSchema, QPROPComplex)
    var SIDUPComplex : List[Array[String]] = recordify(ThroughputBenchmarks.SIDUPComplexResults,Benchmark.events)
    ResultWriter.write(throughputPath + "/" + "SIDUPComplex.csv",eventSchema,SIDUPComplex)
  }
  
  def turnFinished() : Unit = {
    /*var QPROPChain : List[Array[String]] = recordify(TurnBenchmark.QPROPChainResults,Benchmark.events)
    ResultWriter.write(turnPath + "/" + "QPROPChain.csv",sizeSchema, QPROPChain)
    var SIDUPChain : List[Array[String]] = recordify(TurnBenchmark.SIDUPChainResults,Benchmark.events)
    ResultWriter.write(turnPath + "/" + "SIDUPChain.csv", sizeSchema, SIDUPChain)*/
    var QPROPComplex : List[Array[String]] = recordify(TurnBenchmark.QPROPComplexResults,Benchmark.events)
    ResultWriter.write(turnPath + "/" + "QPROPComplex.csv", sizeSchema, QPROPComplex)
    var SIDUPComplex : List[Array[String]] = recordify(TurnBenchmark.SIDUPComplexResults,Benchmark.events)
    ResultWriter.write(turnPath + "/" + "SIDUPComplex.csv", sizeSchema, SIDUPComplex)
    ThroughputBenchmarks.run(throughputFinished)
  }
  
  def dynamicFinished() : Unit = {
    var QPROPComplex : List[Array[String]] = recordify(DynamicBenchmarks.QPROPComplexResults,Benchmark.dynDependencies)
    ResultWriter.write(dynamicPath + "/" + "QPROPComplex.csv", sizeSchema, QPROPComplex)
    var SIDUPComplex : List[Array[String]] = recordify(DynamicBenchmarks.SIDUPComplexResults,Benchmark.dynDependencies)
    ResultWriter.write(dynamicPath + "/" + "SIDUPComplex.csv", dynamicSchema, SIDUPComplex)
  }
  
  def main(args : Array[String]) {
    //Used for debugging before running benchmarks on server
    val serverMode : Boolean = args(0).toBoolean
    //val serverMode = false
    if(serverMode){
      speedPath      = args(1)
      throughputPath = args(2)
      turnPath       = args(3)
      dynamicPath    = args(4)

      //SpeedBenchmarks.run(speedFinished)
      DynamicBenchmarks.run(dynamicFinished)
    }
    else{
      speedPath = "/Users/flo/Desktop/graphTests/Speed"
      throughputPath = "/Users/flo/Desktop/graphTests/Throughput"
      turnPath = "/Users/flo/Desktop/graphTests/Turn"
      dynamicPath = "/Users/flo/Desktop/graphTests/Dynamic"
      //SpeedBenchmarks.run(speedFinished)
      //DynamicBenchmarks.run(dynamicFinished)
      DynamicBenchmarks.run(dynamicFinished)
    }    
  }
  
}