package scala.geotree.map.tree

import java.lang.Long.numberOfTrailingZeros

import scala.annotation.tailrec

/**
  * Place data in veb-tree. Indexes can be converted to coordinates and back as following:
  * ____0   1   2   3   4   5   6   7 ...
  * ____________________________________
  * 0  |0   1   4   5   16  17  20  21
  * 1  |2   3   6   7   18  19  22  23
  * 2  |8   9   12  13  24  25  28  29
  * 3  |10  11  14  15  26  27  30  31
  * 4  |32  33  36  37  48  49  52  53
  * 5  |34  35  38  39  50  51  54  55
  * 6  |40  41  44  45  56  57  60  61
  * 7  |42  43  46  47  58  59  62  63
  * ...
  */

object FractalArea {
  type Index = Long

  case class Coordinates(x: Int, y: Int){
    override def toString: String = s"[x:$x, y: $y]"
  }

  //it works! I have tests for that :-)
  def indexToCoordinates(idx: Index): Coordinates = {
    assert(idx >= 0, "Index must be >= 0!")
    assert(idx <= Long.MaxValue / 2, "Index must be <= Long.MaxValue/2!")

    @tailrec
    @inline def inner(idx: Long, counter: Int, x: Int, y: Int): Coordinates = {
      if (idx == 0) Coordinates(x, y) else {
        val temp = (idx % 4).intValue()
        inner(idx >> 2, counter * 2, x + counter * (temp % 2), y + counter * (temp >> 1))
      }
    }

    inner(idx, 1, 0, 0)
  }

  def coordinatesToIndex(coord: Coordinates): Index = coordinatesToIndex(coord.x, coord.y)

  def coordinatesToIndex(x: Int, y: Int): Index = {

    // convert 1111 -> 10101010
    @tailrec
    @inline def shift(rest: Int, result: Index = 0l, sh: Int = 1): Index = {
      if (rest == 0) result else shift(rest >> 1, result + ((rest % 2).longValue() << sh), sh + 2)
    }

    (shift(x) >> 1) + shift(y)
  }

  def commonParent(ind1: Index, ind2: Index): Index = {
    @tailrec
    @inline
    def inner(fst: Index, snd: Index, mult: Index = 1): Index = {
      if (fst == snd) fst * mult else inner(fst >>> 2, snd >>> 2, mult << 2)
    }

    inner(ind1, ind2)
  }

  def commonCapacity(ind1: Index, ind2: Index): Index = {
    val parent = commonParent(ind1, ind2)

    @tailrec
    @inline
    def inner(fst: Index, snd: Index, capacity: Index = 4): Index = {
      if (fst < capacity && snd < capacity) capacity else inner(fst, snd, capacity * 4)
    }

    inner(ind1 - parent, ind2 - parent)
  }

  def getDistance(ind1: Index, ind2: Index): (Int, Int) = {

    def distance(ind1: Int, ind2: Int): (Int, Int) = (ind1, ind2) match {
      case (a: Int, b: Int) if a == b => (0, 0)
      case (0, 1) | (2, 3) => (1, 0)
      case (0, 2) | (1, 3) => (0, 1)
      case (0, 3) => (1, 1)
      case (1, 2) => (-1, 1)
      case (2, 1) => (1, -1)
      case (3, 0) => (-1, -1)
      case (2, 0) | (3, 1) => (0, -1)
      case (1, 0) | (3, 2) => (-1, 0)
    }

    def inner(current: (Int, Int), mult: Int, rest1: Long, rest2: Long): (Int, Int) = {
      if (rest1 == 0 && rest2 == 0) current
      else {
        val (dx, dy) = distance((rest1 % 4).intValue(), (rest2 % 4).intValue())
        inner((current._1 + dx * mult) -> (current._2 + dy * mult), mult * 2, rest1 >>> 2, rest2 >>> 2)
      }
    }

    inner((0, 0), 1, ind1, ind2)
  }

  def squareSize(capacity: Long): Long = capacity >>> (numberOfTrailingZeros(capacity) / 2)
}
