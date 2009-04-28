package com.richdougherty.binary.rope

import java.nio.ByteBuffer

private[binary] object TreeRope {

  /**
   * Precalculated array containing all positive Int values of the
   * Fibonacci sequence.
   */
  private val fibonacci: Array[Int] = {
    def stream: Stream[Long] = {
      def tailAfter(a: Long, b: Long): Stream[Long] = {
        val c = a + b
        Stream.cons(c, tailAfter(b, c))
      }
      val first: Long = 0
      val second: Long = 1
      Stream.cons(first, Stream.cons(second, tailAfter(first, second)))
    }
    stream takeWhile { _ <= Integer.MAX_VALUE } map { _.asInstanceOf[Int] } toArray
  }

}

/**
 * A Rope object composed of two child Rope objects.
 *
 * @author <a href="http://www.richdougherty.com/">Rich Dougherty</a>
 */
final case class TreeRope(val left: Rope, val right: Rope) extends Rope {

  override val length = left.length + right.length

  override def apply(i: Int) = if (i < left.length) left(i) else right(i - left.length)

  // Cache, because calculation would otherwise be O(log n).
  // Use Byte to save space since max depth is 47 (or 48 before rebalancing).
  private var cachedDepth: Byte = (Math.max(left.depth, right.depth) + 1).asInstanceOf[Byte]

  private[binary] def depth = cachedDepth.asInstanceOf[Int]

  private[binary] def isBalanced: Boolean = {
    import TreeRope.fibonacci
    ((depth + 2) >= fibonacci.length) || (length >= fibonacci(depth + 2))
  }

  private[binary] def rebalance: Rope = {
    def process(array: Array[Rope]): Rope = {
      array.length match {
        case 0 => Rope.empty
        case 1 => array(0)
        case 2 => new TreeRope(array(0), array(1))
        case n => {
          val midPoint = n / 2
          val left = array.slice(0, midPoint)
          val right = array.slice(midPoint, n)
          new TreeRope(process(left), process(right))
        }
      }
    }
    // XXX: Avoid iterating over arrays twice.
    process(unsafe_arrays(0, length).toList.toArray)
  }

  protected def forcedSlice0(from: Int, until: Int): Rope = {
    if (until <= left.length) {
      // Slice is completely contained by left component.
      left.forcedSlice(from, until)
    } else if (from >= left.length) {
      // Slice is completely contained by right component.
      right.forcedSlice(from - left.length, until - left.length)
    } else {
      new Rope {

        override def length = until - from

        override private[binary] def depth = TreeRope.this.depth

        override def apply(i: Int) = {
          if (i < 0 || i >= length) throw new IndexOutOfBoundsException
          else TreeRope.this.apply(from + i)
        }

        protected def copyToArray0(from2: Int, until2: Int, dest: Array[Byte], destFrom: Int): Unit = {
          TreeRope.this.copyToArray0(from + from2, from + until2, dest, destFrom)
        }

        protected def arrays0(from2: Int, until2: Int): Iterable[LeafRope] = {
          TreeRope.this.arrays0(from + from2, from + until2)
        }

        protected def forcedSlice0(from2: Int, until2: Int): Rope = {
          TreeRope.this.forcedSlice0(from + from2, from + until2)
        }
      } 
    }
  }

  protected def copyToArray0(from: Int, until: Int, dest: Array[Byte], destFrom: Int): Unit = {
    //println("TreeRope"+(left.length, right.length)+".copyToArray0"+(from: Int, until: Int, dest: Array[Byte], destFrom: Int))
    import Math._
    if (from < left.length) {
      left.copyToArray(from, min(until, left.length), dest, destFrom)
    }
    if (until > left.length) {
      right.copyToArray(max(from, left.length) - left.length, until - left.length, dest, destFrom + left.length)
    }
  }

  protected def arrays0(from: Int, until: Int): Iterable[LeafRope] = {
    import Math._
    val leftArrays = if (from < left.length) {
      left.unsafe_arrays(from, min(until, left.length))
    } else {
      Nil
    }
    val rightArrays = if (until > left.length) {
      right.unsafe_arrays(max(from, left.length) - left.length, until - left.length)
    } else {
      Nil
    }
    leftArrays ++ rightArrays
  }

  override def elements: Iterator[Byte] = (left.elements ++ right.elements)

}
