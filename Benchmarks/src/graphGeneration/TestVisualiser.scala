package graphGeneration


object TestVisualiser extends App {
  //Chain test
  val chain   : Graph = new Chain(30,2).generate()
  Visualiser.visualise(chain,"/Users/flo/Desktop/graphTests","chainTest")
  //Fan test
  val fan     : Graph = new Fan(10,1).generate()
  Visualiser.visualise(fan, "/Users/flo/Desktop/graphTests","fanTest")
  //Complex test
  val complex : Graph = new Complex(1 to 5, 10,1).generate()
  println("Sources in generated : " + complex.getSources())
  Visualiser.visualise(complex, "/Users/flo/Desktop/graphTests","complexTest")
  
}