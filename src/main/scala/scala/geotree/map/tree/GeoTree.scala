package scala.geotree.map.tree

import scala.geotree.WithCoordinates
import scala.geotree.map.tree.FractalArea.{Coordinates, Index}

class GeoTree[T] {

  private val rootNode: GeoTreeNode[T] = new RootNode[T]

  final def insert(coordinates: Coordinates, obj: T): Unit = {
    rootNode.insert(FractalArea.coordinatesToIndex(coordinates), WithCoordinates(coordinates, obj))
  }

  final def get(coordinates: Coordinates): Option[List[WithCoordinates[T]]] =
    rootNode.get(FractalArea.coordinatesToIndex(coordinates))

  final def getClosestPoints(coordinates: Coordinates)(implicit ord: Ordering[Coordinates]): Iterator[WithCoordinates[T]] = {
    rootNode.getClosestPoints(FractalArea.coordinatesToIndex(coordinates))
  }

  final def size(): Index = rootNode.size()

  override def toString: String = s"GeoTree[size = ${size()}]"
}

sealed trait GeoTreeNode[T] {

  def start: Index

  def capacity: Index

  def insert(index: Index, obj: WithCoordinates[T]): GeoTreeNode[T]

  def getClosestPoints(index: Index)(implicit ordering: Ordering[Coordinates]): FastCheckIterator[WithCoordinates[T]]

  def get(index: Index): Option[List[WithCoordinates[T]]]

  def size(): Index

  override def toString: String = s"GeoTreeNode[start = $start (${FractalArea.indexToCoordinates(start)}), " +
    s"capacity = $capacity, size = ${size()}]"
}

class RootNode[T] extends GeoTreeNode[T] {
  private var inner: GeoTreeNode[T] = new IntermediateNode[T](0, Long.MaxValue / 2 + 1)

  override def start: Index = inner.start

  override def capacity: Index = inner.capacity

  override def insert(index: Index, obj: WithCoordinates[T]): GeoTreeNode[T] = {
    if (index >= capacity) throw new IllegalArgumentException(s"Cannot insert points with index bigger that $capacity")

    this.inner = inner.insert(index, obj)
    this
  }

  override def getClosestPoints(index: Index)(implicit ordering: Ordering[Coordinates]): FastCheckIterator[WithCoordinates[T]]
  = inner.getClosestPoints(index)

  override def get(index: Index): Option[List[WithCoordinates[T]]] = inner.get(index)

  override def size(): Index = inner.size()
}


class IntermediateNode[T](val start: Index, val capacity: Index) extends GeoTreeNode[T] {

  protected var children: Array[GeoTreeNode[T]] = Array.ofDim(4)

  def insert(index: Index, obj: WithCoordinates[T]): GeoTreeNode[T] = {

    if (index >= start && index < start + capacity) {
      val indexInChildren = getIndexInChildren(index)
      val newChild = if (children(indexInChildren.intValue()) != null) {
        children(indexInChildren.intValue()).insert(index, obj)
      } else {
        new LeafGeoTreeNode[T](index, obj)
      }

      children(indexInChildren) = newChild

      this
    } else {
      val commonParent = FractalArea.commonParent(start, index)
      val commonCapacity = FractalArea.commonCapacity(start, index)
      new IntermediateNode[T](commonParent, commonCapacity)
        .insertNode(this)
        .insert(index, obj)
    }

  }

  def insertNode(node: GeoTreeNode[T]): GeoTreeNode[T] = {
    assert(!children.exists(_ != null)) // check correctness of insert

    val indexInChildren = getIndexInChildren(node.start)
    children(indexInChildren) = node

    this
  }

  private def getIndexInChildren(index: Index): Int = ((index - start) / (capacity >>> 2)).intValue()

  override def getClosestPoints(index: Index)(implicit ordering: Ordering[Coordinates]): FastCheckIterator[WithCoordinates[T]] = {

    import ordering._

    def childIterator(child: Int): FastCheckIterator[WithCoordinates[T]] =
      if (children(child) == null) FastCheckIterator.empty else children(child).getClosestPoints(index)

    val (dx, dy) = FractalArea.getDistance(start, index)
    val squareSize: Long = FractalArea.squareSize(capacity)

    val closestPointInSquare: Coordinates = {
      val startCoordinates = FractalArea.indexToCoordinates(start)
      val distanceFromStartToClosestPoint: (Int, Int) = if (index - start < capacity && (index - start) >= 0) (dx, dy)
      else if (dx >= 0 && dx < squareSize && dy < 0) (dx, 0) // upper
      else if (dx < 0 && dy >= 0 && dy < squareSize) (0, dy) // left
      else if (dx >= 0 && dx < squareSize && dy > 0) (dx, squareSize.intValue()) // bottom
      else if (dx > 0 && dy >= 0 && dy < squareSize) (squareSize.intValue(), dy) // right
      else if (dx < 0 && dy < 0) (0, 0) //left-top-corner
      else if (dx < 0 && dy > 0) (0, squareSize.intValue()) //left-bottom corner
      else if (dx > 0 && dy > 0) (squareSize.intValue(), squareSize.intValue()) //right-bottom corner
      else if (dx > 0 && dy < 0) (squareSize.intValue(), 0) //right-top corner
      else throw new IllegalStateException(s"Must not be here! $dx, $dy, $squareSize, $index, $start, $capacity")

      Coordinates(startCoordinates.x + distanceFromStartToClosestPoint._1, startCoordinates.y + distanceFromStartToClosestPoint._2)
    }

    def childrenIterator =
      if (index - start < capacity && (index - start) >= 0) { // within our square
        val nearest = getIndexInChildren(index)
        val other = Set(0, 1, 2, 3) - nearest
        childIterator(nearest)
          .combine(other.foldLeft(FastCheckIterator.empty[WithCoordinates[T]]) { case (it, child) => it.combine(childIterator(child)) })
      } else if (dx >= 0 && dx < squareSize && dy < 0) { // upper
        childIterator(0).combine(childIterator(1)).combine(childIterator(2)).combine(childIterator(3))
      } else if (dx < 0 && dy >= 0 && dy < squareSize) { // left
        childIterator(0).combine(childIterator(2)).combine(childIterator(1)).combine(childIterator(3))
      } else if (dx >= 0 && dx < squareSize && dy > 0) { // bottom
        childIterator(2).combine(childIterator(3)).combine(childIterator(0)).combine(childIterator(1))
      } else if (dx > 0 && dy >= 0 && dy < squareSize) { // right
        childIterator(1).combine(childIterator(3)).combine(childIterator(0)).combine(childIterator(2))
      } else if (dx < 0 && dy < 0) { //left-top-corner
        (childIterator(0) ++ childIterator(3)).combine(childIterator(1).combine(childIterator(2)))
      } else if (dx < 0 && dy > 0) { //left-bottom corner
        (childIterator(2) ++ childIterator(1)).combine(childIterator(0).combine(childIterator(3)))
      } else if (dx > 0 && dy > 0) { //right-bottom corner
        (childIterator(3) ++ childIterator(0)).combine(childIterator(1).combine(childIterator(2)))
      } else if (dx > 0 && dy < 0) { //right-top corner
        (childIterator(1) ++ childIterator(2)).combine(childIterator(0).combine(childIterator(3)))
      } else {
        throw new IllegalStateException("Must not be here! $dx, $dy, $squareSize, $index, $start, $capacity")
      }

    FastCheckIterator.wrap(childrenIterator, prev => prev > closestPointInSquare)
  }

  def get(index: Index): Option[List[WithCoordinates[T]]] = Option(children(getIndexInChildren(index))).flatMap(_.get(index))

  override def size(): Index = children.filter(_ != null).map(_.size()).sum
}


class LeafGeoTreeNode[T](val start: Index, firstT: WithCoordinates[T]) extends GeoTreeNode[T] {
  var items: List[WithCoordinates[T]] = firstT :: Nil

  override def capacity: Index = 1

  override def insert(index: Index, obj: WithCoordinates[T]): GeoTreeNode[T] = {
    if (start == index) {
      items = obj :: items
      this
    } else {
      val commonParent = FractalArea.commonParent(start, index)
      val commonCapacity = FractalArea.commonCapacity(start, index)
      new IntermediateNode[T](commonParent, commonCapacity)
        .insertNode(this)
        .insert(index, obj)
    }
  }

  override def size(): Index = items.size

  override def get(index: Index): Option[List[WithCoordinates[T]]] = Some(items)

  override def getClosestPoints(index: Index)(implicit ordering: Ordering[Coordinates]): FastCheckIterator[WithCoordinates[T]] = FastCheckIterator.wrap(
    items.iterator, prev => items.headOption.exists(x => ordering.lt(x.coordinates, prev))
  )
}