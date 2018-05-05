package scala.geotree.map.tree

import org.scalatest.{FlatSpec, Matchers}

import scala.geotree.map.tree.FractalArea.Coordinates
import scala.geotree.map.tree.GeoTree.WithCoordinates
import scala.util.Random


class GeoTreeTest extends FlatSpec with Matchers {

  "A GeoTree" should "can be created" in {
    val tree = new GeoTree[String]

    tree.insert(Coordinates(1, 1), "a")
    tree.insert(Coordinates(1, 1), "c")
    tree.insert(Coordinates(1, 2), "d")
    tree.insert(Coordinates(Int.MaxValue, Int.MaxValue), "b")
    tree.get(Coordinates(1, 1)) should be(Some(List(WithCoordinates(Coordinates(1, 1), "c"), WithCoordinates(Coordinates(1, 1), "a"))))
    tree.get(Coordinates(Int.MaxValue, Int.MaxValue)) should be(Some(List(WithCoordinates(Coordinates(Int.MaxValue, Int.MaxValue), "b"))))

  }

  def ordering(coordinates: Coordinates): Ordering[Coordinates] = Ordering.by(c =>
    (c.x - coordinates.x).longValue() * (c.x - coordinates.x)
      + (c.y - coordinates.y).longValue() * (c.y - coordinates.y))

  it should "find closes points" in {
    val tree = new GeoTree[String]
    tree.insert(Coordinates(1, 1), "c")
    tree.insert(Coordinates(1, 2), "d")
    tree.insert(Coordinates(Int.MaxValue, Int.MaxValue), "b")
    val coordinates = Coordinates(1, 3)
    implicit val ord = ordering(coordinates)
    tree.getClosestPoints(coordinates).map(_.obj).toSeq should be(Seq("d", "c", "b"))
  }

  it should "store millions of points" in {

    val cities = 10
    val points = 10000
    val tree = new GeoTree[String]

    for (city <- 1 to cities) {
      val cityName = s"city_$city"
      val cityLat = Random.nextInt(Int.MaxValue - 100000) + 50000
      val cityLng = Random.nextInt(Int.MaxValue - 100000) + 50000
      val locs = for (j <- Iterator.range(0, points)) yield {
        Tuple3(cityLat + Random.nextInt(100000) - 50000, cityLng + Random.nextInt(100000) - 50000, s"${cityName}_$j")
      }

      println(s"$cityName: $cityLat, $cityLng")
      locs.foreach(point => tree.insert(Coordinates(point._1, point._2), point._3))

      val startTime = System.currentTimeMillis()
      implicit val ord = ordering(Coordinates(cityLat, cityLng))
      val str = tree.getClosestPoints(Coordinates(cityLat, cityLng)).take(10).mkString(",")
      val endTime = System.currentTimeMillis()
      println(endTime - startTime)
      println(str)
    }

    println(tree.toString)

  }

  it should "return nearest points in increasing order" in {
    val tree = new GeoTree[Int]
    Random.setSeed(42)
    val startX = 1000000000
    val startY = 1000000000
    val points = for (angle <- 0 to 3600) yield {
      val newX = startX + (100d * angle * Math.cos(angle * Math.PI / 180)).toInt
      val newY = startY - (100d * angle * Math.sin(angle * Math.PI / 180)).toInt
      Coordinates(newX, newY) -> angle
    }

    points.foreach { x => tree.insert(x._1, x._2) }

    implicit val ord: Ordering[Coordinates] = ordering(Coordinates(startX, startY))
    tree.getClosestPoints(Coordinates(startX, startY)).zipWithIndex.zip(points.iterator).foreach {
      case ((WithCoordinates(receivedCoord, angleReceived), index), (initialCoord, angle)) => {
        assert(receivedCoord == initialCoord, s"angle = $angle, $angleReceived, $receivedCoord, $initialCoord")
//        assert(angleReceived == index)
        assert(angleReceived == angle)
      }
    }

  }
}
