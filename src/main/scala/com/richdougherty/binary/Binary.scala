package com.richdougherty.binary

import com.richdougherty.binary.rope._
import java.io._
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset._

/**
 * Useful methods for working Binary objects, including Binary
 * creation.
 *
 * @author <a href="http://www.richdougherty.com/">Rich Dougherty</a>
 */
object Binary {

  private val _empty = fromArray(new Array[Byte](0))

  /**
   * Gets an empty Binary.
   */
  def empty: Binary = _empty

  /**
   * Creates a Binary containing the given bytes.
   */
  def apply(bytes: Byte*): Binary = fromArray(bytes.toArray) // XXX: Could avoid copy?

  /**
   * Creates a Binary from a copy of the given sequence.
   */
  def fromSeq(seq: Seq[Byte]): Binary = seq match {
    case binary: Binary => binary
    case array: Array[Byte] => fromArray(array)
    case rope: Rope => new Binary(rope.copy)
    case _ => fromArray(seq.toArray)
  }

  /**
   * Creats a Binary from a copy of the given bytes.
   */
  def fromArray(array: Array[Byte]): Binary =
    fromArray(array, 0, array.length)
  
  /**
   * Creates a Binary from a copy of a slice of the given bytes.
   * 
   * @param array The bytes to create the Binary from.
   * @param offset The first byte to include in the Binary.
   * @param length The length of the Binary.
   */
  def fromArray(array: Array[Byte], offset: Int, length: Int): Binary = {
    val copy = new Array[Byte](length)
    Array.copy(array, offset, copy, 0, length)
    val rope = new LeafRope(copy, 0, length)
    val binary = new Binary(rope)
    binary
  }

  /**
   * Creates a Binary from a copy of the given rope.
   */
  def fromRope(rope: Rope): Binary = new Binary(rope.copy)
  
  /**
   * Creates a Binary from a String, using the platform's default character
   * encoding.
   */
  def fromString(string: String): Binary =
    new Binary(Rope.fromString(string).copy)

  /**
   * Creates a Binary from a String, using the given character encoding.
   */
  def fromString(string: String, charsetName: String): Binary =
    new Binary(Rope.fromString(string, charsetName).copy)

  def readFromInputStream(in: InputStream): Binary =
    new Binary(Rope.readFromInputStream(in).copy)

  trait BinaryLike extends RandomAccessSeq[Byte] {
    def force: Binary
  }

  trait Projection extends RandomAccessSeq.Projection[Byte] with BinaryLike {
    protected def rp: Rope.Projection
    protected def wrap(rp0: Rope.Projection): Binary.Projection
    override def force: Binary = new Binary(rp.force)
    override def drop(from: Int): Binary.Projection = new ProjectionImpl(rp.slice(from, length))
    override def take(until: Int): Binary.Projection = new ProjectionImpl(rp.slice(0, until))
    override def dropWhile(p: Byte => Boolean): Binary.Projection = new ProjectionImpl(rp.dropWhile(p))
    override def takeWhile(p: Byte => Boolean): Binary.Projection = new ProjectionImpl(rp.takeWhile(p))
    override def slice(from0: Int, until0: Int): Binary.Projection = new ProjectionImpl(rp.slice(from0, until0))
    override def slice(from0: Int): Binary.Projection = new ProjectionImpl(rp.slice(from0))
    override def reverse: Binary.Projection = new ProjectionImpl(rp.reverse)
    override def length: Int = rp.length
  }

  private class ProjectionImpl(protected val rp: Rope.Projection) extends Binary.Projection {
    protected def wrap(rp0: Rope.Projection): Binary.Projection = new ProjectionImpl(rp0)
    def apply(i: Int) = rp(i)
  }

}

/**
 * An immutable, randomly-accessible sequence of bytes. The current
 * implementation is a concatenation tree, or "rope". Compared to a simple
 * <code>Array</code> of bytes, this data structure emphasises the performance
 * of concatenation and slice operations over that of random access. Iteration
 * is still fast.
 *
 * @see http://www.cs.ubc.ca/local/reading/proceedings/spe91-95/spe/vol25/issue12/spe986.pdf
 */
@serializable
@SerialVersionUID(-8314444781809819999L)
final class Binary private(private var rope: Rope) extends RandomAccessSeq[Byte] with Binary.BinaryLike with Serializable {
  // rope is var rather than val, to allow deserialization to work
  
  def apply(i: Int) = rope(i)

  /**
   * The length of the Binary in bytes.
   */
  def length: Int = rope.length

  /**
   * The size of the Binary in bytes.
   */
  override def size = length

  override final def hashCode = rope.hashCode

  /**
   * Checks if another object is equal to this object. They will be
   * equal if and only if the other object is a Binary containing the
   * same bytes in the same order.
   */
  override final def equals(that: Any): Boolean = {
    if (this eq that.asInstanceOf[AnyRef]) return true
    if (!(that.isInstanceOf[Binary])) return false
    val thatBinary = that.asInstanceOf[Binary]
    rope.equals(thatBinary.rope)
  }
  
  override def projection: Binary.Projection = new Binary.ProjectionImpl(rope.projection)
  override def slice(from: Int, until: Int): Binary.Projection = projection.slice(from, until)
  override def slice(from: Int): Binary.Projection = projection.slice(from)
  override def take(until: Int): Binary.Projection = projection.take(until)
  override def drop(from: Int): Binary.Projection = projection.drop(from)
  override def dropWhile(p: Byte => Boolean) = projection.dropWhile(p)
  override def takeWhile(p: Byte => Boolean) = projection.takeWhile(p)
  override def reverse = projection.reverse
  override def force = this
  def splitAt(n: Int): (Binary, Binary) = (new Binary(rope.forcedSlice(0, n)), new Binary(rope.forcedSlice(n, length)))

  /**
   * Append another Binary to this Binary, returning the resulting aggregation.
   *
   * @param other The Binary to append.
   */
  final def ++(other: Binary): Binary =
    new Binary(rope ++ other.rope)

  /**
   * Copy this object's bytes into a given array.
   */
  final def copyToArray(from: Int, until: Int, dest: Array[Byte], destFrom: Int): Unit =
    rope.copyToArray(from, until, dest, destFrom)

  /**
   * Get a copy of this object's bytes, stored in an array.
   */
  def toArray: Array[Byte] = rope.toArray

  /**
   * Get a textual representation of this object.
   */
  override def toString = {
    val builder = new StringBuilder()
    builder.append("Binary(")
    val maxLength = 16
    take(maxLength).addString(builder, ", ")
    if (length > maxLength) {
      val remaining = length - maxLength
      builder.append(", [")
      builder.append(remaining)
      builder.append(" more]")
    }
    builder.append(")")
    builder.toString
  }

  /**
   * Get a hexadecimal representation of this object's bytes.
   */
  def toHexString: String = {
    val builder = new StringBuilder(length * 2)
    for (byte <- this) {
      val unsigned = (256 + byte) % 256
      val upper = unsigned >> 4
      val lower = unsigned & 0xf
      builder.append(Character.forDigit(upper, 16))
      builder.append(Character.forDigit(lower, 16))
    }
    builder.toString
  }

  /**
   * Decodes the Binary into a String, using a given charset.
   *
   * @throws java.nio.BufferUnderflowException If the Binary contains too few bytes.
   * @throws java.nio.charset.MalformedInputException If the input is malformed.
   * @throws java.nio.charset.UnmappableCharacterException If the output cannot be represented in the charset.
   */
  def decodeString(charsetName: String): String =
    rope.copy.decodeString(charsetName)

  /**
   * Decodes the Binary into a String using the given decoder and output buffer.
   *
   * @return None if decoding succeeds, or Some(coderResult) with the
   * error causing failure.
   */
  def decodeStringWith(decoder: CharsetDecoder, charBuffer: CharBuffer): Option[CoderResult] =
    rope.copy.decodeStringWith(decoder, charBuffer)

  /**
   * Gets a big-endian-encoded Long from the given index.
   */
  def getBELong(index: Int): Long =
    rope.getBELong(index)

  // TODO: Write more conversion functions...

  def writeToOutputStream(out: OutputStream) =
    rope.copy.writeToOutputStream(out)
  
  private def writeObject(out: ObjectOutputStream): Unit = {
    out.writeInt(length)
    for (arrayRope <- rope.unsafe_arrays) {
      out.write(arrayRope.array, arrayRope.offset, arrayRope.length)
    }
  }

  private def readObject(in: ObjectInputStream): Unit = {
    val offset = 0
    val length = in.readInt()
    val array = new Array[Byte](length)
    var remaining = length
    while (remaining > 0) {
      val readLength = in.read(array, length - remaining, remaining)
      if (readLength == -1) {
        throw new IOException("Expected " + remaining + " more bytes.")
      }
      remaining -= readLength
    }
    rope = new LeafRope(array, 0, length)
  }

}
