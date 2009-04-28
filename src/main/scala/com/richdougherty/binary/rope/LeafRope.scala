package com.richdougherty.binary.rope

import java.nio.ByteBuffer

/**
 * A Rope object backed by an Array.
 *
 * @author <a href="http://www.richdougherty.com/">Rich Dougherty</a>
 */
final case class LeafRope(val array: Array[Byte], val offset: Int, val length: Int) extends Rope {

  override private[binary] def depth = 0

  override def apply(i: Int) = {
    if (i < 0 || i >= length) throw new IndexOutOfBoundsException
    array(offset + i)
  }

  protected def forcedSlice0(from: Int, until: Int): LeafRope = {
    new LeafRope(array, offset + from, until - from)
  }

  protected def copyToArray0(from: Int, until: Int, dest: Array[Byte], destFrom: Int): Unit = {
    //println("LeafRope"+(this.array, offset, length)+".copyToArray0"+(from: Int, until: Int, dest: Array[Byte], destFrom: Int))
    //println("Array.copy" + (this.array, offset + from, array, destFrom, until - from))
    Array.copy(this.array, offset + from, dest, destFrom, until - from)
  }

  protected def arrays0(from: Int, until: Int): Iterable[LeafRope] = forcedSlice0(from, until) :: Nil

  def unsafe_wrappingByteBuffer: ByteBuffer = ByteBuffer.wrap(array, offset, length)

  override def elements: Iterator[Byte] = array.slice(offset, offset + length).elements

}
