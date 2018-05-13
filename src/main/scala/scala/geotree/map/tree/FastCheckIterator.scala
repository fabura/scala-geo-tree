package scala.geotree.map.tree

import scala.geotree.WithCoordinates
import scala.geotree.map.tree.FractalArea.Coordinates

trait FastCheckIterator[T] extends Iterator[T] {

  def canContainElementsBetterThan(prevBest: Coordinates)(implicit ordering: Ordering[Coordinates]): Boolean

  def ++(that: FastCheckIterator[T])(implicit ordering: Ordering[Coordinates]): FastCheckIterator[T] = FastCheckIterator.wrap(super.++(that), (prev: Coordinates) =>
    if (hasNext) {
      this.canContainElementsBetterThan(prev)
    } else if (that.hasNext) {
      that.canContainElementsBetterThan(prev)
    } else false)

  def combine[A](iterator2: FastCheckIterator[WithCoordinates[A]])
                (implicit cmp: Ordering[Coordinates], ev: T =:= WithCoordinates[A]): FastCheckIterator[WithCoordinates[A]] = {
    val _this = this
    new FastCheckIterator[WithCoordinates[A]] {

      import cmp._

      private val bufIterator1: BufferedIterator[WithCoordinates[A]] = _this.map(ev.apply).buffered
      private val bufIterator2: BufferedIterator[WithCoordinates[A]] = iterator2.buffered

      def hasNext: Boolean = bufIterator1.hasNext || bufIterator2.hasNext

      def next(): WithCoordinates[A] =
        if (bufIterator1.hasNext) {
          val fstHead = bufIterator1.head
          if (iterator2.canContainElementsBetterThan(fstHead.coordinates) && bufIterator2.hasNext) {
            val sndHead = bufIterator2.head
            if (fstHead.coordinates < sndHead.coordinates) bufIterator1.next() else bufIterator2.next()
          } else {
            bufIterator1.next()
          }
        } else if (bufIterator2.hasNext) {
          bufIterator2.next()
        }
        else {
          throw new UnsupportedOperationException("Cannot call next on an exhausted iterator!")
        }

      override def canContainElementsBetterThan(prevBest: Coordinates)(implicit ordering: Ordering[Coordinates]): Boolean =
        _this.canContainElementsBetterThan(prevBest)(ordering) || iterator2.canContainElementsBetterThan(prevBest)(ordering)

    }
  }
}

object FastCheckIterator {

  private object Empty extends FastCheckIterator[Nothing] {
    override def canContainElementsBetterThan(prevBest: Coordinates)(implicit ordering: Ordering[Coordinates]): Boolean = false

    override def hasNext: Boolean = false

    override def next(): Nothing = throw new UnsupportedOperationException("Cannot call next on an empty iterator!")
  }

  def empty[T]: FastCheckIterator[T] = Empty.asInstanceOf[FastCheckIterator[T]]

  def wrap[T](iterator: => Iterator[T], predicate: Coordinates => Boolean): FastCheckIterator[T] = new FastCheckIterator[T] {
    private lazy val it = iterator

    override def canContainElementsBetterThan(prevBest: Coordinates)(implicit ordering: Ordering[Coordinates]): Boolean = predicate(prevBest)

    override def hasNext: Boolean = it.hasNext

    override def next(): T = it.next()
  }

}
