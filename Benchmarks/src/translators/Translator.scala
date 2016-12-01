package translators

abstract class Translator[APINodeType](val intermediateLambda : List[_] => _, val sourceLambda : List[_] => _, val sinkLambda : List[_] => _) {
  
  def translate(graph : graphGeneration.Graph) : (List[APINodeType],List[_]) => _
  
  def mapNode(graphId : graphGeneration.Graph.NodeId) : APINodeType
  
  def cleanUp() : Unit
  
}