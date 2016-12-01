package benchmarks

import com.opencsv.CSVWriter
import scala.collection.JavaConversions._
import java.io.FileWriter
import java.io.BufferedWriter

object ResultWriter{
  def write(path : String, schema : Array[String], results : List[Array[String]]) : Unit = {
    val out     : BufferedWriter       = new BufferedWriter(new FileWriter(path));
    val writer  : CSVWriter            = new CSVWriter(out)
    var records : List[Array[String]]  = List[Array[String]](schema)    
    results.foreach { result : Array[String] =>  
      records = records :+ result
    }
    writer.writeAll(records)
    out.close()
  }
  
  def writeSimple(path : String,results : List[Long]) : Unit = {
    val out     : BufferedWriter       = new BufferedWriter(new FileWriter(path));
    results.foreach { res => out.write(res.asInstanceOf[Int]) }
    out.newLine()
    out.close()
  }
}