package scala.geotree.map.tree

import org.scalatest._
import FractalArea._

import scala.util.Random

class FractalAreaTest extends FlatSpec with Matchers {

  "A FractalArea" should "convert index to coordinates" in {
    indexToCoordinates(0) should be(Coordinates(0, 0))
    indexToCoordinates(4) should be(Coordinates(2, 0))
    indexToCoordinates(19) should be(Coordinates(5, 1))
    indexToCoordinates(63) should be(Coordinates(7, 7))
    indexToCoordinates(60) should be(Coordinates(6, 6))
    indexToCoordinates(44) should be(Coordinates(2, 6))
    indexToCoordinates(11) should be(Coordinates(1, 3))
    indexToCoordinates((Long.MaxValue - 1) / 2) should be(Coordinates(Int.MaxValue, Int.MaxValue))
    assertThrows[AssertionError](indexToCoordinates(-100))
    assertThrows[AssertionError](indexToCoordinates(Long.MaxValue / 2 + 1))
  }

  it should "convert coordinates to index" in {
    coordinatesToIndex(0, 0) should be(0l)
    coordinatesToIndex(1, 0) should be(1l)
    coordinatesToIndex(0, 1) should be(2l)
    coordinatesToIndex(1, 1) should be(3l)
    coordinatesToIndex(2, 0) should be(4l)
    coordinatesToIndex(2, 6) should be(44l)
    coordinatesToIndex(1, 3) should be(11l)
    coordinatesToIndex(7, 7) should be(63)
    coordinatesToIndex(6, 6) should be(60)

    coordinatesToIndex(Int.MaxValue, Int.MaxValue) should be(Long.MaxValue / 2)
  }

  it should "be reversable convertation" in {
    Random.setSeed(42)
    for (i <- 1 to 10000) {
      val x = Random.nextInt(Int.MaxValue)
      val y = Random.nextInt(Int.MaxValue)
      indexToCoordinates(coordinatesToIndex(x, y)) should be(Coordinates(x, y))
    }
  }

  it should "can calculate common parent" in {
    commonParent(1, 3) should be(0)
    commonParent(1, 5) should be(0)
    commonParent(4, 6) should be(4)
    commonParent(17, 23) should be(16)
  }

  it should "can calculate distance" in {
    getDistance(3314627652424861858l, 3314627652422205440l) should be(0, -1709)
  }

  it should "can calculate squareSize correctly" in {
    squareSize(4) should be (2)
    squareSize(16) should be (4)
    squareSize(64) should be (8)
    squareSize(256) should be (16)
    squareSize(1024) should be (32)
    squareSize(4096) should be (64)
    squareSize(1l << 32) should be (65536)
    squareSize(1l << 62) should be (Int.MaxValue.longValue() + 1)
  }
}
