package graphGeneration
import java.io._
import sys.process._

//Visualises graphs using DOT, used for debugging
object Visualiser {
  
  
  def stringify(graph : Graph) : String = {
    var result : String = ""
    result              = result + "digraph { \n"
    graph.edges.foreach { case Edge(from :Graph.NodeId, to : Graph.NodeId)  =>
      var edge : String = from + " -> " + to + ";" + "\n"
      result            = result + edge
    }
    result              = result + "}"
    result
  }
  
  def visualise(graph : Graph, directory : String, fileName : String) : Unit = {
    val writer       : PrintWriter = new PrintWriter(new File(directory + "/" + fileName + ".dot"))
    val stringGraph  : String      = stringify(graph)
    writer.write(stringGraph)
    writer.close()
    //"cd " + directory !
    ("/usr/local/bin/dot -Tpdf " + directory + "/" + fileName + ".dot -o " + directory + "/" + fileName + ".pdf") !
  }
  
}