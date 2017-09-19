package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Math.{PI, toDegrees, atan, exp, pow, sinh}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val n = pow(2, zoom)

    // In degrees.
    val longitude = x / n * 360 - 180
    val latitude = atan(sinh(PI - 2 * PI * y / n)) * 180 / PI

    Location(latitude, longitude)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zoom`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
//    val coordinates = for{
//      yCoordinate <- x until 256
//      xCoordinate <- y until 256
//    } yield {
//      val newLocation = tileLocation(zoom + 8, xCoordinate, yCoordinate)
//      val temperature = Visualization.predictTemperature(temperatures, newLocation)
//
//      (newLocation, temperature)
//    }
//
//    Visualization.visualize(temperatures.toSet ++ coordinates.toSet, colors, 256, 256)
    ???
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 to (pow(2, zoom) - 1).toInt
      y <- 0 to (pow(2, zoom) - 1).toInt
    }
      generateImage(year, zoom, x, y, data)
  }

}
