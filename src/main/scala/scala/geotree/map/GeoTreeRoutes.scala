package scala.geotree.map


import scala.geotree.map.tree.FractalArea.Coordinates
import scala.concurrent.{ExecutionContext, Future}
import scala.geotree.{Place, WithCoordinates}
import scala.geotree.map.tree.GeoTree

class GeoTreeRoutes[T](tree: GeoTree[T], config: GeoTreeConfig)(implicit ex: ExecutionContext) {
  type Longitude = geotree.Longitude
  type Latitude = geotree.Latitude
  type Distance = geotree.Distance
  type Place = geotree.Place

  private val world: World[Latitude, Longitude] = config.world

  import geotree.distanceOrdering._

  def nearby(latitude: Latitude, longitude: Longitude, radius: Distance, limit: Int): Future[Seq[Place]] = {
    Future {
      val coordinates = world.convertCoordinates(latitude, longitude)
      implicit val ordering: Ordering[Coordinates] = world.getOrderingCoordinates(coordinates)
      tree.getClosestPoints(coordinates)(Ordering.by(c => world.distance(c, coordinates)))
        .map(getPoint(coordinates))
        .takeWhile(place => world.distance(latitude, longitude, place.latitude, place.longitude) <= radius)
        .take(limit)
        .toSeq
    }
  }

  def closest(latitude: Latitude, longitude: Longitude, limit: Int): Future[Seq[Place]] = {
    Future {
      val coordinatesToSearch = world.convertCoordinates(latitude, longitude)
      implicit val ordering: Ordering[Coordinates] = world.getOrderingCoordinates(coordinatesToSearch)
      tree.getClosestPoints(coordinatesToSearch).take(limit).map(getPoint(coordinatesToSearch)).toSeq
    }
  }

  private def getPoint(coordinatesToSearch: Coordinates)(result: WithCoordinates[T]) = {
    val WithCoordinates(coord, name) = result
    val (latitude, longitude) = world.convertCoordinatesBack(coord)
    val distance = world.distance(coordinatesToSearch, coord)
    Place(name.toString, distance, longitude, latitude)
  }
}
