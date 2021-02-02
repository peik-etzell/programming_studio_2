package io

import java.io.FileReader
import java.io.BufferedReader
import java.io.FileNotFoundException
import java.io.IOException
import java.io.FileWriter
import java.io.BufferedWriter

class WeatherStation {
  /**
   * Length of a header block.
   */
  val headerBlock = 4

  /**
   * The number of blocks used in the file format
   */
  val numOfBlocks = 7

  /**
   * Here you should write a method that reads a weather data file
   * and creates a WeatherDataPoint object out of it.
   *
   * The structure of the data in the file is separated into blocks
   * that start with a "#" symbol and a new block always starts from a new line.
   *
   * The data blocks that are included in the Weather Data file are
   *
   * Data type			|	Header			| Length   	| Unit
   * ------------------------------------------------------------------
   * Version data		|	CLI					|	3					| Version no.
   * Date						|	DAY					| 8					| DDMMYYYY
   * Time stamp			|	TIM					| 6					| HHMMSS
   * Air Pressure		|	APR					|	4					| hPA
   * Temperature		|	TMP					|	3					| Celsius
   * Humidity				|	HUM					|	3					| %
   * Precipitation	|	PRC					|	3					| mm
   *
   * The Data type tells you what type of data the data block represents. There
   * are corresponding variables for each of them in the WeatherDataPoint class
   * except for the Version data.
   *
   * There is a unique three character header associated with each data block.
   * Each header is proceeded with the symbol "#" and a new data block always starts
   * from a new line.
   *
   * The length tells you how many characters there are after the header that represent
   * the actual data associated with that header. The unused spaces in the front will be
   * filled with extra zeros (0) e.g. if the data block has length 3 and the data input
   * is 10 (only two digits) the resulting data block should be "010".
   *
   * For this first part you should read through a data file
   * and return a corresponding WeatherDataPoint object. You can assume
   * that the file you are reading has the correct structure.
   *
   * We will use FileReader and BufferedReader to read through the file
   * line by line (see the examples in the materials). You can use the helper
   * methods found at the end of this class to separate a data block into its
   * header and data portions.
   *
   * You can use the "weatherdata1.txt" to test out your solution. The Runner object
   * has already some simple code to try out your methods. And as always,
   * don't forget to run your local unit tests before submitting to the server.
   *
   */
  def readDataToDataPoint(dataFile: String): Option[WeatherDataPoint] = {

    var data: Seq[(String, Int)] = null
    try {
      val source = new FileReader(dataFile)
      val buffer = new BufferedReader(source)

      var line: String = ""
      try {
        data = for {
          i <- 0 until numOfBlocks; line = buffer.readLine()
        } yield (readHeader(line), takeData(line))
      }

      finally {
        buffer.close()
        source.close()
      }

    }
    Some(new WeatherDataPoint(data(1)._2, data(2)._2, data(3)._2, data(4)._2, data(5)._2, data(6)._2 ))
  }

  /**
   * This method takes a WeatherDataPoint object and encodes the data
   * into the Weather Data file format explained above.
   *
   * The result should be a String sequence (Seq[String]) where each
   * entry represents a line in the encoded file.
   *
   * You should keep in mind that each data block has its own appropriate
   * length with extra zeros (0) in front of it for the unused spaces.
   * E.g. if the data block has length 3 and the data input is 10 (only two digits)
   * the resulting string should be "010". You can use the convenience method
   * dataLength to check the length of an integer data point.
   *
   * You can approach the exercise by doing each line individually and then putting all
   * the results inside a sequence Seq[String].
   */
  def encodeInputData(dataPoint: WeatherDataPoint): Seq[String] = {
    var result = Seq[String]()

    val format: Seq[(WeatherHeader, Int)] = Seq(
      (Version, Version.number),
      (Date, dataPoint.date),
      (TimeStamp, dataPoint.timestamp),
      (Temperature, dataPoint.temperature),
      (AirPressure, dataPoint.pressure),
      (Humidity, dataPoint.humidity),
      (Precipitation, dataPoint.precipitation)
    )

    for {
      (weatherHeader, number) <- format
    } yield {
      "#" + weatherHeader.header + {
        "0" * (weatherHeader.blockLength - dataLength(number)) + number.toString
      }
    }
  }


  /**
   * Writes the encoded output into a file. The method simply uses
   * a buffered write to write the encoded String sequence
   * into the file line by line using FileWriter and BufferedWriter.
   */
  def writeEntry(fileName: String, dataPoint: WeatherDataPoint) = {
    val arr = this.encodeInputData(dataPoint)
    try {
      val fw = new FileWriter(fileName);
      val buffWriter = new BufferedWriter(fw)

      try {
        for(str <- arr) {
          buffWriter.write(str)
          buffWriter.newLine()
        }
      } finally {
        buffWriter.close()
      }
    } catch {
      case e: FileNotFoundException =>
        println("File not found");
      case ioe: IOException => println("Issue with IO.");
      case _: Throwable => println("Unexpected Exception");
    }
  }



  /**
   * Convenience method for reading out the header of the data block.
   */
  def readHeader(inputLine: String): String = {
    inputLine.take(headerBlock).drop(1) //Take the header (4 chars) and drop the '#' from the start
  }

  /**
   * Reads the data portion of a data block.
   * Tries to parse the file content into an integer from the string input.
   *
   * If the data portion of the block has unexpected character (e.g. not numbers)
   * ,for example if the is something wrong in the file format,
   * an exception is encountered and the method should return -1.
   */
  def takeData(inputLine: String): Int = {
    try {
      inputLine.drop(headerBlock).toInt
    } catch {
      case e: Exception => -1
    }
  }

  /**
   * Convenience method for calculating the length
   * (e.g. the number of digits) of an integer
   * .
   */
  def dataLength(data: Int) = data.toString.length()
}

/**
 * Helper objects that have the header names and block lengths
 * for each type of data.
 */
sealed trait WeatherHeader {
  val header: String
  val blockLength: Int
}
case object Version extends WeatherHeader {
  val header = "CLI"
  val blockLength = 3
  val number = 12
}
case object TimeStamp extends WeatherHeader {
  val header = "TIM"
  val blockLength = 6
}
case object Date extends WeatherHeader {
  val header = "DAY"
  val blockLength = 8
}
case object Temperature extends WeatherHeader {
  val header = "TMP"
  val blockLength = 3
}
case object Humidity extends WeatherHeader {
  val header = "HUM"
  val blockLength = 3
}
case object Precipitation extends WeatherHeader {
  val header = "PRC"
  val blockLength = 3
}
case object AirPressure extends WeatherHeader {
  val header = "APR"
  val blockLength = 4
}
