package observatory

import java.time.LocalDate

import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    def toCelsius(temperatureInFahrenheit: Double) = (temperatureInFahrenheit - 32) / 1.8
    def loadStations = {

      val resourceStream = this.getClass.getResourceAsStream(stationsFile)
      val lines: Iterator[String] = Source.fromInputStream(resourceStream).getLines

      val stationsCoordinates: MutableMap[(String, String), Location] = MutableMap()

      for {
        line <- lines
        tokens = line.split(",").map(_.trim)
        if tokens.length == 4
        Array(stnIdentifier, wbanIdentifier, latitudeString, longitudeString) = tokens
        if longitudeString.nonEmpty && latitudeString.nonEmpty
      } {
        val longitude = longitudeString.toDouble
        val latitude = latitudeString.toDouble
        stationsCoordinates += (stnIdentifier, wbanIdentifier) -> Location(lat = latitude, lon = longitude)
      }

      stationsCoordinates
    }

    val stationsLocations = loadStations

    val temperaturesFileResourceStream = this.getClass.getResourceAsStream(temperaturesFile)
    val lines = Source.fromInputStream(temperaturesFileResourceStream).getLines()
    var result: List[(LocalDate, Location, Double)] = Nil

    for {
      line <- lines
      tokens = line.split(",").map(_.trim)
      if tokens.length == 5
      Array(stnIdentifier, wbanIdentifier, monthString, dayString, temperatureInFahrenheit) = tokens
      station = (stnIdentifier, wbanIdentifier)
      if stationsLocations.contains(station) && monthString.nonEmpty && dayString.nonEmpty &&
        temperatureInFahrenheit.nonEmpty && temperatureInFahrenheit != "9999.9"
    } {
      val month = Integer.parseInt(monthString)
      val dayOfMonth = Integer.parseInt(dayString)
      val temperature = toCelsius(temperatureInFahrenheit.toDouble)

      val localDate = LocalDate of (year, month, dayOfMonth)

      result = (localDate, stationsLocations(station), temperature) :: result
    }

    result
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val recordsGroupedByLocation = records.groupBy(_._2)
    val sumAndCountPerLocation = recordsGroupedByLocation.mapValues(_.foldLeft((0.0, 0)) { (sumAndCount, record) =>
      val (sum, count) = sumAndCount
      val (_, _, temperature) = record

      (sum + temperature, count + 1)
    })
    val yearlyAveragePerLocation = sumAndCountPerLocation.mapValues { case (sum, count) => sum / count}

    yearlyAveragePerLocation
  }
}