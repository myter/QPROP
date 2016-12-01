package examples
import QPROPS.algorithm._

//TODO partial dream-code, need to add new source and way of getting source actor ref directly
object MarathonApp extends App{
  //Create all necessary nodes
  /*val gpsId   = API.newSource(getCoordinate())
  val hrId    = API.newSource(getHR())
  val scoreId = API.newNode({stream => server.getPosition(stream(0))}, Postion.Last)
  val guiId   = API.newNode({stream => updateGUI(stream(0),stream(1))}, null)
  val medicId = API.newNode({stream => if(stream(1) > DangerThreshold){medicServer.sendAlert(stream(0))}},null)
  //Link nodes
  API.-=>(gpsId, List(scoreId,medicId))
  API.-=>(hrId,List(guiId,medicId))
  API.->(scoreId,guiId)
  //Create graph
  API.<>()
  //Produce values
  val gpsRef = API.getRef(gpsId)
  val hrRef  = API.getRef(hrId)*/
}