package scala.geotree.map


import scala.geotree.DistUnits.Meter
import scala.geotree.{Distance, Latitude, Longitude}
import scala.geotree.map.tree.FractalArea
import scala.geotree.map.tree.FractalArea.Coordinates

trait World[Latitude, Longitude] {

  /**
    * Converter from Latitude and Longitude to Coordinates in GeoTree.
    *
    * @param latitude  - latitude to convert
    * @param longitude - longitude
    * @return - Coordinates = ([0, Int.MaxValue], [0, Int.MaxValue]).
    */
  def convertCoordinates(latitude: Latitude, longitude: Longitude): FractalArea.Coordinates

  /**
    * Converter from Coordinates in GeoTree to Latitude and Longitude.
    * Must: convertCoordinatesBack(convertCoordinates(latitude, longitude)) == (latitude, longitude)
    *
    * @param coordinates
    * @return
    */
  def convertCoordinatesBack(coordinates: Coordinates): (Latitude, Longitude)

  def distance(latitude1: Latitude, longitude1: Longitude, latitude2: Latitude, longitude2: Longitude): Distance

  def distance(coordinates1: Coordinates, coordinates2: Coordinates): Distance

  def getOrderingCoordinates(from: Coordinates): Ordering[Coordinates] = Ordering.by(c => distance(from, c))

}

object World {

  object Earth extends World[Latitude, Longitude] {

    private val MaxLatitude = 88.0
    private val MaxLongitude = 180.0
    val EarthRadius = Distance(6372800, Meter)

    override def convertCoordinates(latitude: Latitude, longitude: Longitude): Coordinates = {
      if (Math.abs(latitude) > MaxLatitude) throw new IllegalArgumentException("Illegal latitude value")
      if (Math.abs(longitude) > MaxLongitude) throw new IllegalArgumentException("Illegal longitude value")

      val intLatitude = ((latitude + MaxLatitude) / (2 * MaxLatitude) * Int.MaxValue).intValue()
      val intLongitude = ((longitude + MaxLongitude) / (2 * MaxLongitude) * Int.MaxValue).intValue()
      Coordinates(intLatitude, intLongitude)
    }

    override def convertCoordinatesBack(coordinates: Coordinates): (Latitude, Longitude) = {
      val intLatitude = coordinates.x
      val intLongitude = coordinates.y

      val latitude: Latitude = intLatitude.doubleValue() * 2 * MaxLatitude / Int.MaxValue - MaxLatitude
      val longitude: Longitude = intLongitude.doubleValue() * 2 * MaxLongitude / Int.MaxValue - MaxLongitude

      (latitude, longitude)
    }

    override def distance(latitude1: Latitude, longitude1: Longitude, latitude2: Latitude, longitude2: Longitude): Distance =
      Distance(EarthRadius.toMeters * greatCirlceDistance(latitude1, longitude1, latitude2, longitude2), Meter)


    /**
      * @param latitude1 - in degrees
      * @param latitude2
      * @param longitude1
      * @param longitude2
      * @return distance in radians
      */
    def greatCirlceDistance(latitude1: Latitude, longitude1: Longitude, latitude2: Latitude, longitude2: Longitude): Double = {
      import Math._
      val lat1Rad = latitude1 * PI / 180
      val lon1Rad = longitude1 * PI / 180
      val lat2Rad = latitude2 * PI / 180
      val lon2Rad = longitude2 * PI / 180
      val Δlatitude = abs(lat1Rad - lat2Rad)
      val Δlongitude = abs(lon1Rad - lon2Rad)

      val a = pow(sin(Δlatitude / 2), 2) + cos(lat2Rad) * cos(lat1Rad) * pow(sin(Δlongitude / 2), 2)
      2 * atan2(sqrt(a), sqrt(1 - a))

    }

    override def distance(coordinates1: Coordinates, coordinates2: Coordinates): Distance = {
      val (lat1, lon1) = convertCoordinatesBack(coordinates1)
      val (lat2, lon2) = convertCoordinatesBack(coordinates2)
      distance(lat1, lon1, lat2, lon2)
    }
  }

}
