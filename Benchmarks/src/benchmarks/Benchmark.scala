package benchmarks

abstract class Benchmark {
  
  def start() : Unit 
}

object Benchmark {
  val warmUps                       : Int         = 100
  val runs                          : Int         = 30
  val sizes                         : List[Int]   = List[Int](100,500,1000,2000)
  val events                        : List[Int]   = List[Int](100,1000,10000,50000)
  val dynDependencies               : List[Int]   = List[Int](1,5,10,20)
  val dynamicSize                   : Int         = 200
  val dynamicEvents                 : Int         = 20
  val defaultSize                   : Int         = 200
  val defaultSources                : Int         = 2
  val defaultComplex                : Range       = 1 to 2
  val defaultEvents                 : Int         = 1
  //Confidence coefficient for 95% confidence interval
  private val confidenceCoefficient : Double    = 1.96
  
  def calcMean(results : List[Long]) : Double = {
    (results.foldLeft[Long](0)((acc : Long, res : Long) => acc + res)) / results.length.toDouble
  }
  
  private def calcStDev(results : List[Long]) : Double = {
    val mean     : Double = calcMean(results)
    val variance : Double = results.foldLeft[Double](0){(acc : Double, res : Long) =>  scala.math.pow((res - mean),2) +  acc} / results.length.toDouble
    scala.math.sqrt(variance)
  }
  
  private def calcErrorMargin(results : List[Long]) : Double = {
    confidenceCoefficient * (calcStDev(results) / scala.math.sqrt(results.length).toDouble)
  }
  
  //Returns lower bound of error margin for given sample of results
  def calcLowerBound(results : List[Long]) : Double = {
    val mean : Double = calcMean(results)
    mean - calcErrorMargin(results)
  }
  
  //Returns upper bound of error margin for given sample of results
  def calcUpperBound(results : List[Long]) : Double = {
    val mean : Double = calcMean(results)
    mean + calcErrorMargin(results)
  }
  
}