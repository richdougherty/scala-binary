package com.richdougherty.binary

import com.richdougherty.binary.rope._
import java.io.InputStream
import java.io.ObjectInput
import java.io.ObjectOutput
import java.io.OutputStream
import java.io.Serializable
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

  /**
   * Gets an empty Binary.
   */
  def empty: Binary = new Binary(new LeafRope(new Array[Byte](0), 0, 0))

  /**
   * Creates a Binary containing the given bytes.
   */
  def apply(bytes: Byte*): Binary = new Binary(Rope.unsafe_wrapArray(bytes.toArray))

  /**
   * Creates a Binary from a copy of the given sequence.
   */
  def fromSeq(seq: Seq[Byte]): Binary = seq match {
    case binary: Binary => binary
    case array: Array[Byte] => fromArray(array)
    case _ => fromArray(seq.toArray)
  }

  /**
   * Creats a Binary from a copy of the given bytes.
   */
  def fromArray(array: Array[Byte]): Binary =
    new Binary(Rope.fromArray(array, 0, array.length))
  
  /**
   * Creates a Binary from a copy of a slice of the given bytes.
   * 
   * @param array The bytes to create the Binary from.
   * @param offset The first byte to include in the Binary.
   * @param length The length of the Binary.
   */
  def fromArray(array: Array[Byte], offset: Int, length: Int): Binary =
    new Binary(Rope.fromArray(array, offset, length))
  
  /**
   * UNSAFE: Creates a Binary by wrapping the given array.
   *
   * <p>This method exposes internal implementation details, allowing callers
   * to violate the immutability of Binary objects. Nevertheless, it is made
   * available to permit certain optimisations.
   */
  def unsafe_wrapArray(array: Array[Byte]): Binary =
    new Binary(Rope.unsafe_wrapArray(array))

  /**
   * UNSAFE: Creates a Binary by wrapping the given array.
   *
   * <p>This method exposes internal implementation details, allowing callers
   * to violate the immutability of Binary objects. Nevertheless, it is made
   * available to permit certain optimisations.
   * 
   * @param array The bytes to create the Binary from.
   * @param offset The first byte to include in the Binary.
   * @param length The length of the Binary.
   */
  def unsafe_wrapArray(array: Array[Byte], offset: Int, length: Int): Binary =
    new Binary(new LeafRope(array, offset, length))

  /**
   * Creates a Binary from a String, using the platform's default character
   * encoding.
   */
  def fromString(string: String): Binary =
    new Binary(Rope.fromString(string))

  /**
   * Creates a Binary from a String, using the given character encoding.
   */
  def fromString(string: String, charsetName: String): Binary =
    new Binary(Rope.fromString(string, charsetName))

  def readFromInputStream(in: InputStream): Binary =
    new Binary(Rope.readFromInputStream(in))

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
final class Binary private(private val rope: Rope) extends RandomAccessSeq[Byte] with Binary.BinaryLike with Serializable {

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
   * Gets the ArrayBinary leaves of this Binary in a given range.
   *
   * <p>This method exposes internal implementation details of Binary objects,
   * and so is only made available to the 'scala' package, in order to permit
   * certain optimisations.
   */
  final def unsafe_arrays(from: Int, until: Int): Iterable[Binary] =
    for (leaf <- rope.unsafe_arrays) yield { new Binary(leaf) }

  /**
   * Gets all the ArrayBinary leaves of this Binary.
   *
   * <p>This method exposes internal implementation details of Binary objects,
   * and so is only made available to the 'scala' package, in order to permit
   * certain optimisations.
   */
  final def unsafe_arrays: Iterable[Binary] =
    unsafe_arrays(0, length)

  /**
   * UNSAFE: Get a list of ByteBuffers containing this object's
   * content. It is important not to modify the content of any buffer,
   * as this will alter the content of this Binary - which must not
   * happen.
   *
   * <p>This method exposes internal implementation details, allowing callers
   * to violate the immutability of Binary objects. Nevertheless, it is made
   * available to the 'scala' package to permit certain optimisations.
   */
  final def unsafe_byteBuffers(from: Int, until: Int): Iterable[ByteBuffer] =
    rope.unsafe_byteBuffers(from, until)

  final def unsafe_byteBuffers: Iterable[ByteBuffer] =
    rope.unsafe_byteBuffers

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
    rope.decodeString(charsetName)

  /**
   * Decodes the Binary into a String using the given decoder and output buffer.
   *
   * @return None if decoding succeeds, or Some(coderResult) with the
   * error causing failure.
   */
  def decodeStringWith(decoder: CharsetDecoder, charBuffer: CharBuffer): Option[CoderResult] =
    rope.decodeStringWith(decoder, charBuffer)

  /**
   * Gets a big-endian-encoded Long from the given index.
   */
  def getBELong(index: Int): Long =
    rope.getBELong(index)

  // TODO: Write more conversion functions...

  def writeToOutputStream(out: OutputStream) =
    rope.writeToOutputStream(out)

}
