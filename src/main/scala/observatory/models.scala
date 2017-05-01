package observatory

case class Location(lat: Double, lon: Double) {
  def inRadians = Location(Math.toRadians(lat), Math.toRadians(lon))
}

case class Color(red: Int, green: Int, blue: Int)