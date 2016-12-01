package graphGeneration
import scala.collection.mutable.Map

case class CycleException(msg : String)

class Graph(val nodes : Map[Graph.NodeId,Node] = Map[Graph.NodeId,Node](), var edges : Set[Edge] = Set[Edge]()) {
  var idPool : Graph.NodeId               = 0
  
  def newNode() : Graph.NodeId = {
    val nodeId  : Graph.NodeId = idPool
    val newNode : Node = new Node(idPool)
    nodes += (nodeId -> newNode)
    idPool            += 1
    nodeId
  }
  
  def copy() : Graph = {
    var nodeCopies : Map[Graph.NodeId,Node] = Map[Graph.NodeId,Node]()
    nodes.foreach(entry => nodeCopies.put(entry._1, entry._2))
    var edgeCopies : Set[Edge] = Set[Edge]()
    edges.foreach { edge => edgeCopies = edgeCopies + new Edge(edge.from,edge.to) }
    new Graph(nodeCopies,edgeCopies)
  }
  
  //Checks whether both nodes exist and whether the introduction of this edge would cause a cycle 
  def legalEdge(from : Graph.NodeId, to : Graph.NodeId) : Boolean = {
    if(from > idPool || to > idPool){
      false
    }
    else{
      checkCycle(edges + new Edge(from,to))
    }
  }
  
  def addEdge(from : Graph.NodeId,to : Graph.NodeId) : Unit = {
    edges = edges + new Edge(from,to)
  }
  
  def removeEdge(edge : Edge) : Unit = {
    edges = edges.filter { e => !(e == edge) }
  }
  
  def hasEdge(from : Graph.NodeId,to : Graph.NodeId) : Boolean = {
    edges.contains(new Edge(from,to))
  }
  
  def getSources() : List[Graph.NodeId] = {
    var sources : List[Graph.NodeId] = List[Graph.NodeId]()
    edges.foreach { case Edge(from : Graph.NodeId, to : Graph.NodeId) =>
      var parentFound : Boolean = false
      //Check if from has a parent
      edges.foreach { case Edge(from2 : Graph.NodeId, to2: Graph.NodeId) =>
        if(to2 == from){
          parentFound = true
        }
      }
      if(!parentFound){
        if(!(sources.contains(from))){
          sources = from +: sources          
        }
      }
    }
    sources
  }
  
  def getSinks() : List[Graph.NodeId] = {
    var sinks : List[Graph.NodeId] = List[Graph.NodeId]()
    edges.foreach { case Edge(from : Graph.NodeId, to : Graph.NodeId) =>
      var childFound : Boolean = false
      //Check if from has a child
      edges.foreach { case Edge(from2 : Graph.NodeId, to2: Graph.NodeId) =>
        if(to == from2){
          childFound = true
        }
      }
      if(!childFound){
        if(!(sinks.contains(to))){
          sinks = to +: sinks        
        }
      }
    }
    sinks
  }
  
   def getChildren(parent : Graph.NodeId) : List[Graph.NodeId] = {
    edges.foldLeft(List[Graph.NodeId]())((acc,edge) => 
      if(edge.from == parent){
        edge.from +: acc
      }
      else{
        acc
      }
    )
  }
   
  def orMap[E](lst : List[E], func :  E => Boolean) : Boolean = {
    val mapped : List[Boolean] = lst.map(func)
    mapped.foldLeft(false)((acc : Boolean,el : Boolean) => acc || el)
  }
  
  def size() : Int = {
    nodes.size
  }
  
  private def checkCycle(edges : Set[Edge]) : Boolean = {
    val sources    : List[Graph.NodeId] = getSources()
    var allVisited : List[Graph.NodeId] = List[Graph.NodeId]()
    def walk(currentNode : Graph.NodeId, visited : List[Graph.NodeId]) : Boolean = {
      if(!allVisited.contains(currentNode)){allVisited = currentNode +: allVisited}
      val children : List[Graph.NodeId] = getChildren(currentNode)
      if(children.isEmpty){
        false
      }
      else{
        if(!(children.intersect(visited)).isEmpty){
          true
        }
        else{
          orMap(children, (child : Graph.NodeId) => walk(child, currentNode +: visited))
        }
      }
    }
    if(sources.isEmpty){
      true
    }
    else{
      val cycleDetected : Boolean = orMap(sources,(source : Graph.NodeId) => walk(source, List[Graph.NodeId]()))
      val visitedAll    : Boolean = allVisited.length == size()
      visitedAll || cycleDetected
    }
  }
  
  
  
}


object Graph {
  type NodeId = Integer
}