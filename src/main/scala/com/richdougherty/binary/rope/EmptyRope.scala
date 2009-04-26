package com.richdougherty.binary.rope

import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable

/**
 * A Rope containing no bytes. Accessible through public 'Rope.empty' method.
 *
 * This object was previously defined as a private member of the Array
 * object.  It has been moved to its own class to ensure it has a
 * stable name for serialization purposes.
 *
 * @author <a href="http://www.richdougherty.com/">Rich Dougherty</a>
 */
@serializable
@SerialVersionUID(3284836654983022412L)
private[binary] object EmptyRope extends Rope with Serializable {

  override def length = 0

  override private[binary] def depth = 0

  override def apply(i: Int) = throw new IndexOutOfBoundsException

  protected def copyToArray0(from: Int, until: Int, dest: Array[Byte], destFrom: Int): Unit = ()

  protected def arrays0(from: Int, until: Int): Iterable[LeafRope] = Nil

  protected def forcedSlice0(from: Int, until: Int): Rope = this

  private def writeObject(out: ObjectOutputStream): Unit = ()

  private def readObject(in: ObjectInputStream): Unit = ()

}
