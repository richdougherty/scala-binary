package com.richdougherty.binary.rope

import java.io.IOException
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable
import java.nio.ByteBuffer

/**
 * A Rope object backed by an Array.
 *
 * @author <a href="http://www.richdougherty.com/">Rich Dougherty</a>
 */
@serializable
@SerialVersionUID(-8770946590245372965L)
private[binary] final case class LeafRope private[binary] (private[binary] var array: Array[Byte], private[binary] var offset: Int, private var _length: Int) extends Rope with Serializable {

  override private[binary] def depth = 0

  def length: Int = _length

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

  private def writeObject(out: ObjectOutputStream): Unit = {
    out.writeInt(length)
    out.write(array, offset, length)
  }

  private def readObject(in: ObjectInputStream): Unit = {
    offset = 0
    _length = in.readInt()
    array = new Array[Byte](length)
    var remaining = length
    while (remaining > 0) {
      val readLength = in.read(array, length - remaining, remaining)
      if (readLength == -1) {
        throw new IOException("Expected " + remaining + " more bytes.")
      }
      remaining -= readLength
    }
  }

}