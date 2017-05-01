package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Math.{abs, sin, acos, cos, pow, round}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param knownTemperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(knownTemperatures: Iterable[(Location, Double)], location: Location): Double = {
    val p = 6

    def greatCircleDistance(firstPoint: Location, secondPoint: Location): Double = {
      val earthRadius = 6371
      val firstPointInRadians = firstPoint.inRadians
      val secondPointInRadians = secondPoint.inRadians
      val deltaLongitude = abs(firstPointInRadians.lon - secondPointInRadians.lon)
      val centralAngleInRadians =
        acos(
          sin(firstPointInRadians.lat) * sin(secondPointInRadians.lat) +
            cos(firstPointInRadians.lat) * cos(secondPointInRadians.lat) * cos(deltaLongitude))

      earthRadius * centralAngleInRadians
    }

    def weight(distance: Double) =
      1.0 / pow(distance, p)

    def interpolateTemperature(temperatures: List[(Location, Double)],
                               weightsSum: Double = 0,
                               weightsTimesValuesSum: Double = 0): Double = {
      temperatures match {
        case Nil => weightsTimesValuesSum / weightsSum
        case (knownLocation, knownTemperature) :: remainingTemperatures =>
          val distance = greatCircleDistance(knownLocation, location)

          if (distance <= 0.001) {
            knownTemperature
          } else {
            val currentWeight = weight(distance)
            interpolateTemperature(remainingTemperatures, weightsSum + currentWeight, weightsTimesValuesSum + currentWeight * knownTemperature)
          }
      }
    }

    interpolateTemperature(knownTemperatures.toList)
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
    def gpsLocationToPixelLocation(location: Location): (Int, Int) = {
      assert(-180.0 <= location.lat && location.lat <= 180.0, s"Latitude = ${location.lat} is not in range [-180, 180]")
      assert(-90.0 <= location.lon && location.lon <= 90, s"Longitude = ${location.lon} is not in range [-90, 90]")

      val (x, y) = ((location.lon + 180).toInt, (90 - location.lat).toInt)

      assert(0 <= x && x <= 359, s"X = $x is not in range [0, 359]")
      assert(0 <= y && y <= 179, s"Y = $y is not in range [0, 179]")

      (x, y)
    }

    def pixelLocationToGpsLocation(x: Int, y: Int) = {
      val longitude = x - 180
      val latitude = 90 - y

      Location(lat = latitude, lon = longitude)
    }

    def colorToPixel(color: Color, alpha: Int) = Pixel(r = color.red, g = color.green, b = color.blue, alpha = alpha)

    val alpha = 127
    val width = 360
    val height = 180

    val pixels: Array[Pixel] = new Array[Pixel](width * height)

    for {
      x <- 0 until width
      y <- 0 until height
    } {
      val temperature = predictTemperature(temperatures, pixelLocationToGpsLocation(x, y))
      val newColor = interpolateColor(colors, temperature)
      pixels(y * width + x) = colorToPixel(newColor, alpha)
    }

    temperatures foreach {
      case (location, temperature) =>
        val color = interpolateColor(colors, temperature)
        val (x, y) = gpsLocationToPixelLocation(location)
        val pixel = colorToPixel(color, alpha)

        pixels(y * width + x) = pixel
    }

    Image(w = width, h = height, pixels = pixels)
  }
}

