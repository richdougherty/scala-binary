package com.richdougherty.binary.rope

import java.io.InputStream
import java.io.ObjectInput
import java.io.ObjectOutput
import java.io.OutputStream
import java.io.Serializable
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset._

/**
 * Useful methods for working Rope objects, including Rope
 * creation.
 *
 * @author <a href="http://www.richdougherty.com/">Rich Dougherty</a>
 */
object Rope {

  /**
   * Gets an empty Rope.
   */
  def empty: Rope = EmptyRope

  /**
   * Creates a Rope containing the given bytes.
   */
  def apply(bytes: Byte*): Rope = unsafe_wrapArray(bytes.toArray)

  /**
   * Creates a Rope from a copy of the given sequence.
   */
  def fromSeq(seq: Seq[Byte]): Rope = seq match {
    case binary: Rope => binary
    case array: Array[Byte] => fromArray(array)
    case _ => fromArray(seq.toArray)
  }

  /**
   * Creats a Rope from a copy of the given bytes.
   */
  def fromArray(array: Array[Byte]): Rope = fromArray(array, 0, array.length)
  
  /**
   * Creates a Rope from a copy of a slice of the given bytes.
   * 
   * @param array The bytes to create the Rope from.
   * @param offset The first byte to include in the Rope.
   * @param length The length of the Rope.
   */
  def fromArray(array: Array[Byte], offset: Int, length: Int): Rope = {
    val copy = new Array[Byte](length)
    Array.copy(array, offset, copy, 0, length)
    new LeafRope(copy, 0, length)
  }
  
  /**
   * UNSAFE: Creates a Rope by wrapping the given array.
   *
   * <p>This method exposes internal implementation details, allowing callers
   * to violate the immutability of Rope objects. Nevertheless, it is made
   * available to permit certain optimisations.
   */
  def unsafe_wrapArray(array: Array[Byte]): Rope =
    unsafe_wrapArray(array, 0, array.length)

  /**
   * UNSAFE: Creates a Rope by wrapping the given array.
   *
   * <p>This method exposes internal implementation details, allowing callers
   * to violate the immutability of Rope objects. Nevertheless, it is made
   * available to permit certain optimisations.
   * 
   * @param array The bytes to create the Rope from.
   * @param offset The first byte to include in the Rope.
   * @param length The length of the Rope.
   */
  def unsafe_wrapArray(array: Array[Byte], offset: Int, length: Int): Rope = {
    new LeafRope(array, offset, length)
  }

  /**
   * Creates a Rope from a String, using the platform's default character
   * encoding.
   */
  def fromString(string: String): Rope = {
    val bytes = string.getBytes
    unsafe_wrapArray(bytes)
  }

  /**
   * Creates a Rope from a String, using the given character encoding.
   */
  def fromString(string: String, charsetName: String): Rope = {
    val bytes = string.getBytes(charsetName)
    unsafe_wrapArray(bytes)
  }

  def readFromInputStream(in: InputStream): Rope = {
    val buffer = new Array[Byte](512)
    def read0(acc: Rope): Rope = {
      val lengthRead = in.read(buffer)
      if (lengthRead == -1) acc
      else read0(acc ++ Rope.fromArray(buffer, 0, lengthRead))
    }
    read0(Rope.empty)
  }

  trait RopeLike extends RandomAccessSeq[Byte] {
    def force: Rope
  }


  trait Projection extends RandomAccessSeq.Projection[Byte] with RopeLike {
    override def force = fromSeq(this)
    override def drop( from: Int) = slice(from, length)
    override def take(until: Int) = slice(0, until)
    override def dropWhile(p: Byte => Boolean) = {
      val c = length + 1
      drop((findIndexOf(!p(_)) + c) % c)
    }
    override def takeWhile(p: Byte => Boolean) = {
      val c = length + 1
      take((findIndexOf(!p(_)) + c) % c)
    }
    private[binary] def forcedSlice(from0: Int, until0: Int) =
      slice(from0, until0).force
    override def slice(from0: Int, until0: Int): Projection = new Projection {
      def from = from0
      def until = until0
      def underlying = Projection.this
      def length = {
        val length0 = underlying.length
        if (from >= until || from >= length0) 0
        else (if (until >= length0) length0 else until) - (if (from < 0) 0 else from)
      }
      def apply(idx : Int) = if (idx < 0 || idx >= length) throw new Predef.IndexOutOfBoundsException
	                     else underlying.apply((if (from < 0) 0 else from) + idx)
      override def slice(from0: Int, until0: Int) =
        Projection.this.slice(from + from0, from + until0)
      override def force = underlying.forcedSlice(from, until)
    }
    override def slice(from0: Int) = slice(from0, length)
    override def reverse: Projection = new Projection {
      def apply(i: Int) = Projection.this.apply(length - i - 1)
      def length = Projection.this.length
    }
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
trait Rope extends RandomAccessSeq[Byte] with Rope.RopeLike {

  /**
   * The length of the Rope in bytes.
   */
  def length: Int

  /**
   * The size of the Rope in bytes.
   */
  override def size = length

  /**
   * The depth of this Rope's tree, where leaves have depth 0.
   */
  private[binary] def depth: Int

  // XXX: Safe to leave unsynchronized? Can a thread ever see a partially-written value?
  private var cachedHashCode = 0

  // same algorithm as java.lang.String, except starting with 4321
  //  4321 + s[0]*31^(n-1) + s[1]*31^(n-2) + ... + s[n-1]
  override final def hashCode: Int = {
    if (cachedHashCode == 0) {
      var hashCode = 4321
      var i = 0
      while (i < length) {
        hashCode = hashCode * 31 + this(i)
        i += 1
      }
      cachedHashCode = hashCode
    }
    cachedHashCode
  }

  /**
   * Checks if another object is equal to this object. They will be
   * equal if and only if the other object is a Rope containing the
   * same bytes in the same order.
   */
  override final def equals(o: Any): Boolean = {
    if (this eq o.asInstanceOf[AnyRef]) return true
    if (!(this.isInstanceOf[Rope])) return false
    val ob = o.asInstanceOf[Rope]
    if (length != ob.length) return false
    var i = 0
    while (i < length) {
      if (this(i) != ob(i)) return false
      i += 1
    }
    true
  }
  
  override def projection: Rope.Projection = new Rope.Projection {
    def length = Rope.this.length
    def apply(i: Int) = Rope.this.apply(i)
    override def force = Rope.this
    override def forcedSlice(from0: Int, until0: Int) =
      Rope.this.forcedSlice(from0, until0)
  }
  override def slice(from: Int, until: Int): Rope.Projection = projection.slice(from, until)
  override def slice(from: Int): Rope.Projection = projection.slice(from)
  override def take(until: Int): Rope.Projection = projection.take(until)
  override def drop(from: Int): Rope.Projection = projection.drop(from)
  override def dropWhile(p: Byte => Boolean) = projection.dropWhile(p)
  override def takeWhile(p: Byte => Boolean) = projection.takeWhile(p)
  override def reverse = projection.reverse
  override def force = this
  def splitAt(n: Int): (Rope, Rope) = (forcedSlice(0, n), forcedSlice(n, length))

  /**
   * Combine two Binaries into a single LeafRope. Used by ++.
   */
  private def combine(left: Rope, right: Rope): Rope = {
    val combinedArray = (new TreeRope(left, right)).toArray
    Rope.unsafe_wrapArray(combinedArray)
  }

  /**
   * Combine to Binaries into a single, balanced TreeRope. Used by ++.
   */
  private def balance(left: Rope, right: Rope): Rope = {
    val composedArray = new TreeRope(left, right)
    if (composedArray.isBalanced) composedArray
    else composedArray.rebalance
  }

  /**
   * Append another Rope to this Rope, returning the resulting aggregation.
   *
   * @param other The Rope to append.
   */
  final def ++(other: Rope): Rope = {
    if (isEmpty) {
      other
    } else if (other.isEmpty) {
      this
    } else {
      val newLength = length.asInstanceOf[Long] + other.length.asInstanceOf[Long]
      if (newLength > Integer.MAX_VALUE) {
        throw new IllegalArgumentException("Combined length too long: " + newLength)
      }
      val maxCombineLength = 16
      (this, other) match {
        case (l: Rope, r @ TreeRope(rl, rr)) if (l.length + rl.length) <= maxCombineLength => {
          balance(combine(l, rl), rr)
        }
        case (l @ TreeRope(ll, lr), r: Rope) if (lr.length + r.length) <= maxCombineLength => {
          balance(ll, combine(lr, r))
        }
        case (l: Rope, r: Rope) => {
          balance(l, r)
        }
      }
    }
  }

  /**
   * Gets a slice of this binary, returning a new Rope as the
   * result.
   */
  private[binary] final def forcedSlice(from: Int, until: Int): Rope = {
    if (from == 0 && until == length) this
    else if (from < 0 || until > length) throw new IndexOutOfBoundsException
    else if (from > until) throw new IllegalArgumentException("Argument 'from' was > 'until'.")
    else forcedSlice0(from, until)
  }

  /**
   * Gets a slice of this binary, returning a new Rope as the
   * result.
   */
  private[binary] final def forcedSlice(from: Int) = slice(from, length)

  /**
   * Internal implementation of slice operation. Called by slice after bounds
   * checking and some simple optimisations have been performed.
   */
  protected def forcedSlice0(from: Int, until: Int): Rope

  /**
   * Copy this object's bytes into a given array. This method will
   * always return an unaliased copy of the rope's bytes.
   */
  final def copyToArray(from: Int, until: Int, dest: Array[Byte], destFrom: Int): Unit = {
    if (from < 0 || until > length) throw new IndexOutOfBoundsException
    else if ((until - from) > (dest.length - destFrom)) throw new IndexOutOfBoundsException
    else if (from == until) ()
    else copyToArray0(from, until, dest, destFrom)
  }

  /**
   * Internal implementation of copyToArray operation. Called by copyToArray
   * after bounds checking and some simple optimisations have been performed.
   */
  protected def copyToArray0(from: Int, until: Int, dest: Array[Byte], destFrom: Int): Unit

  /**
   * Get a copy of this object's bytes, stored in an array.  This
   * method will always return an unaliased copy of the rope's bytes.
   */
  final def toArray: Array[Byte] = {
    val array = new Array[Byte](length)
    copyToArray(0, length, array, 0)
    array
  }

  /**
   * Get a copy of this object's bytes, stored in a wrope.  This
   * method will always return an unaliased copy of the rope's bytes.
   */
  final def copy: Rope = Rope.unsafe_wrapArray(toArray)

  /**
   * Gets the LeafRope leaves of this Rope in a given range.
   *
   * <p>This method exposes internal implementation details of Rope objects,
   * and so is only made available to the 'scala' package, in order to permit
   * certain optimisations.
   */
  final def unsafe_arrays(from: Int, until: Int): Iterable[LeafRope] = {
    if (from < 0 || until > length) throw new IndexOutOfBoundsException
    else if (from == until) Nil
    else arrays0(from, until)
  }

  /**
   * Gets all the LeafRope leaves of this Rope.
   *
   * <p>This method exposes internal implementation details of Rope objects,
   * and so is only made available to the 'scala' package, in order to permit
   * certain optimisations.
   */
  final def unsafe_arrays: Iterable[LeafRope] =
    unsafe_arrays(0, length)

  /**
   * Internal implementation of arrays operation. Called by arrays
   * after bounds checking and some simple optimisations have been performed.
   */
  protected def arrays0(from: Int, until: Int): Iterable[LeafRope]

  /**
   * UNSAFE: Get a list of ByteBuffers containing this object's
   * content. It is important not to modify the content of any buffer,
   * as this will alter the content of this Rope - which must not
   * happen.
   *
   * <p>This method exposes internal implementation details, allowing callers
   * to violate the immutability of Rope objects. Nevertheless, it is made
   * available to the 'scala' package to permit certain optimisations.
   */
  final def unsafe_byteBuffers(from: Int, until: Int): Iterable[ByteBuffer] =
    arrays0(from, until) map { _.unsafe_wrappingByteBuffer }

  final def unsafe_byteBuffers: Iterable[ByteBuffer] =
    unsafe_byteBuffers(0, length)

  /**
   * Get a textual representation of this object.
   */
  override def toString = {
    val builder = new StringBuilder()
    builder.append("Rope(")
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
   * Decodes the Rope into a String, using a given charset.
   *
   * @throws java.nio.BufferUnderflowException If the Rope contains too few bytes.
   * @throws java.nio.charset.MalformedInputException If the input is malformed.
   * @throws java.nio.charset.UnmappableCharacterException If the output cannot be represented in the charset.
   */
  def decodeString(charsetName: String): String = {
    val charset = Charset.forName(charsetName)
    val decoder = charset.newDecoder()
    val maxChars = (decoder.maxCharsPerByte().asInstanceOf[Double] * length).asInstanceOf[Int]
    val charArray = new Array[Char](maxChars)
    val charBuffer = CharBuffer.wrap(charArray)
    val result = decodeStringWith(decoder, charBuffer)
    result match {
      case None => {
        decoder.decode(ByteBuffer.allocate(0), charBuffer, true)
        decoder.flush(charBuffer)
        new String(charArray, 0, charBuffer.position)
      }
      case Some(coderResult) => {
        coderResult.throwException
        throw new AssertionError("Unreachable code")
      }
    }
  }

  /**
   * Decodes the Rope into a String using the given decoder and output buffer.
   *
   * @return None if decoding succeeds, or Some(coderResult) with the
   * error causing failure.
   */
  def decodeStringWith(decoder: CharsetDecoder, charBuffer: CharBuffer): Option[CoderResult] = {
    for (bb <- unsafe_byteBuffers) {
      val result = decoder.decode(bb, charBuffer, false)
      if (result.isError) return Some(result)
    }
    None
  }

  /**
   * Gets a big-endian-encoded Long from the given index.
   */
  def getBELong(index: Int): Long = {
    this(index+0).asInstanceOf[Long] << 56 |
    this(index+1).asInstanceOf[Long] << 48 |
    this(index+2).asInstanceOf[Long] << 40 |
    this(index+3).asInstanceOf[Long] << 32 |
    this(index+4).asInstanceOf[Long] << 24 |
    this(index+5).asInstanceOf[Long] << 16 |
    this(index+6).asInstanceOf[Long] << 8 |
    this(index+7).asInstanceOf[Long] << 0
  }

  // TODO: Write more conversion functions...

  def writeToOutputStream(out: OutputStream) = {
    for (arrayRope <- unsafe_arrays) {
      out.write(arrayRope.array, arrayRope.offset, arrayRope.length)
    }
  }

}
