package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Math.{abs, sin, acos, cos, pow, round}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val p = 6
    def greatCircleDistance(firstPoint: Location, secondPoint: Location): Double = {
      val earthRadius = 6371  // In KMs.
      val deltaLatitude = abs(firstPoint.lat - secondPoint.lat)
      val deltaLongitude = abs(firstPoint.lon - secondPoint.lon)
      val centralAngle = acos(sin(firstPoint.lat) * sin(secondPoint.lat) + cos(firstPoint.lat) * cos(secondPoint.lat) * cos(deltaLongitude))

      earthRadius * centralAngle
    }

    def weight(known: Location, unknown: Location) = 1 / pow(greatCircleDistance(known, unknown), p)
    val temperaturesAndDistances = temperatures.map {
      case (knownLocation, temperature) => (knownLocation, temperature, greatCircleDistance(knownLocation, location))
    }

    temperaturesAndDistances.find(_._3 <= 1) match {  // Use the temperature of a point within 1 km
      case Some((_, temperature, _)) => temperature
      case None =>
        val numerator = temperaturesAndDistances.map { case (knownLocation, temperature, _) => temperature * weight(knownLocation, location) }.sum
        val denominator = temperaturesAndDistances.map { case (knownLocation, _, _ ) => weight(knownLocation, location) }.sum

        numerator / denominator
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    val sortedPoints = points.toSeq.sortBy(_._1)

    sortedPoints.find { case (pointValue, _) => pointValue == value } match {
      case Some((_, color)) => color
      case None =>
        val minPoint = sortedPoints.head
        val maxPoint = sortedPoints.last

        if (value < minPoint._1) minPoint._2
        else if (value > maxPoint._1) maxPoint._2
        else {
          val ((lowerValue, lowerColor), (upperValue, upperColor)) = sortedPoints.zip(sortedPoints.tail).find {
            case ((leftPointValue, _), (rightPointValue, _)) => leftPointValue <= value && value <= rightPointValue
          }.get

          val rangeSize = upperValue - lowerValue
          val lowerEndWeight = 1 - (value - lowerValue) / rangeSize
          val upperEndWeight = 1 - (upperValue - value) / rangeSize

          Color(
            red = round(lowerColor.red * lowerEndWeight  + upperColor.red * upperEndWeight).toInt,
            blue = round(lowerColor.blue * lowerEndWeight + upperColor.blue * upperEndWeight).toInt,
            green = round(lowerColor.green * lowerEndWeight + upperColor.green * upperEndWeight).toInt)
        }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
  }
}

