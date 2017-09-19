package observatory


/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    val gridTemperatures = (for {
      lat <- -89 to 90
      lon <- -180 to 179
    } yield {
      val location = Location(lat, lon)
      (location, Visualization.predictTemperature(temperatures, location))
    }).toMap

    (lt, ln) => gridTemperatures(Location(Math.round(lt), Math.round(ln)))
  }

  /**
    * @param temperatures Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperatures: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    val numberOfYears = temperatures.size
    val grids = temperatures.map(makeGrid)
    val averageMaps: Map[Location, Double] = (for {
      lat <- -89 to 90
      lon <- -180 to 179 }
      yield Location(lat, lon) -> grids.map(g => g(lat, lon)).sum / numberOfYears).toMap

    (lt, lon) => averageMaps(Location(Math.round(lt), Math.round(lon)))
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A sequence of grids containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    val grid = makeGrid(temperatures)
    (lt, ln) => grid(lt, ln) - normals(lt, ln)
  }
}

