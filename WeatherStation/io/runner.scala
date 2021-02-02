package io

object runner extends App {
  val inputFile = "weatherdata1.txt"
  val targetFile = "weatherdata2.txt"
  
  println("Started...")
  val station = new WeatherStation
  println("Created Weather Station.")
  def generateDataPoint(inputFile: String) = station.readDataToDataPoint(inputFile)
  
  val dataPoint = generateDataPoint(inputFile)
  dataPoint.map(println)
  println
  
  val encoded = dataPoint.map(station.encodeInputData)
  encoded.map(println)
  println
  
  dataPoint.map(station.writeEntry(targetFile, _))
  println("Wrote data on the file: " + targetFile)
}