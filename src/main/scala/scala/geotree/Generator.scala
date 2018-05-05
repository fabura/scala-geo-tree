package scala.geotree


import scala.geotree.map.tree.FractalArea.Coordinates
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.geotree.map.World
import scala.geotree.map.tree.GeoTree
import scala.util.Random

object Generator {

  def generateRandomTree(): GeoTree[String] = {

    val cities = 10
    val points = 1000000
    val tree = new GeoTree[String]
    Random.setSeed(42)

    for (city <- 1 to cities) {
      val cityName = s"city_$city"
      val cityLat = Random.nextInt(Int.MaxValue - 100000) + 50000
      val cityLng = Random.nextInt(Int.MaxValue - 100000) + 50000
      val locs = for (j <- Iterator.range(0, points)) yield {
        Tuple3(cityLat + Random.nextInt(100000) - 50000, cityLng + Random.nextInt(100000) - 50000, s"${cityName}_$j")
      }

      println(s"$cityName: $cityLat, $cityLng, (${World.Earth.convertCoordinatesBack(Coordinates(cityLat, cityLng))})")
      locs.foreach(point => tree.insert(Coordinates(point._1, point._2), point._3))
    }

    tree
  }

}
