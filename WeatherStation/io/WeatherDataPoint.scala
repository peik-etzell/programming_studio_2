package io

case class WeatherDataPoint (
  var date: Int,
  var timestamp: Int,
  var temperature: Int,
  var pressure: Int,
  var humidity: Int,
  var precipitation: Int
  )
  
object WeatherDataPoint {
  def emptyPoint = new WeatherDataPoint(0, 0, 0, 0, 0 ,0)
}