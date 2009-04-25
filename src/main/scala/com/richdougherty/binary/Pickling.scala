package com.richdougherty.binary

trait Pickler[T] {
  def pickle(x: T): Binary
  def unpickle(b: Binary): (T, Binary)
}

trait Pickling {

  def pickling[T](x: T)(implicit p: Pickler[T]) = p.pickle(x)
  def unpickling[T](b: Binary)(implicit p: Pickler[T]) = p.unpickle(b)

}

object Pickling extends Pickling

trait StandardPickling extends Pickling {

  trait Encoder[T,U] {
    def encode(t: T): U
    def decode(u: U): T
  }

  def encoderPickler[T,U](encoder: Encoder[T,U])(implicit pickler: Pickler[U]) = new Pickler[T] {
    def pickle(decoded: T) = {
      val encoded = encoder.encode(decoded)
      pickling(encoded)
    }
    def unpickle(b: Binary): (T, Binary) = {
      val (encoded, b2) = unpickling[U](b)
      val decoded = encoder.decode(encoded)
      (decoded, b2)
    }
  }

  implicit val unitPickler = new Pickler[Unit] {
    def pickle(x: Unit) = Binary.empty
    def unpickle(b: Binary) = ((), b)
  }

  implicit val nothingPickler = new Pickler[Nothing] {
    def pickle(x: Nothing) = error("Should not be callable.")
    def unpickle(b: Binary) = error("Should not be callable.")
  }

  implicit val bytePickler = new Pickler[Byte] {
    def pickle(x: Byte) = Binary(x)
    def unpickle(b: Binary) = (b(0), b.slice(1).force)
  }

  implicit val booleanPickler: Pickler[Boolean] = encoderPickler(
    new Encoder[Boolean,Byte] {
      def encode(boolean: Boolean) = if (boolean) 1 else 0
      def decode(byte: Byte) = byte match {
        case 0 => false
        case 1 => true
      }
    }
  )

  implicit val intPickler = new Pickler[Int] {
    def pickle(x: Int) = {
      val b0 = (x >>> 24).toByte
      val b1 = (x >>> 16).toByte
      val b2 = (x >>> 8).toByte
      val b3 = (x >>> 0).toByte
      Binary(b0, b1, b2, b3)
    }
    def unpickle(b: Binary) = {
      val i0 = b(0) & 0xff
      val i1 = b(1) & 0xff
      val i2 = b(2) & 0xff
      val i3 = b(3) & 0xff
      val i = (i0 << 24) + (i1 << 16) + (i2 << 8) + (i3 << 0)
      (i, b.slice(4).force)
    }
  }

  implicit val charPickler = new Pickler[Char] {
    def pickle(x: Char) = {
      val b0 = (x >>> 8).toByte
      val b1 = (x >>> 0).toByte
      Binary(b0, b1)
    }
    def unpickle(b: Binary) = {
      val i0 = b(0) & 0xff
      val i1 = b(1) & 0xff
      val c = ((i0 << 8) + (i1 << 0)).toChar
      (c, b.slice(2).force)
    }
  }

  implicit def tuple1Pickler[T1](implicit pickler1: Pickler[T1]) = new Pickler[Tuple1[T1]] {
    def pickle(x: Tuple1[T1]) = pickling(x._1)
    def unpickle(b: Binary) = {
      val (_1, b2) = unpickling[T1](b)
      (Tuple1(_1), b2)
    }
  }

  implicit def tuple2Pickler[T1,T2](implicit pickler1: Pickler[T1], pickler2: Pickler[T2]) = new Pickler[Tuple2[T1,T2]] {
    def pickle(x: Tuple2[T1,T2]) = {
      pickling(x._1) ++ pickling(x._2)
    }
    def unpickle(b: Binary) = {
      val (_1, b2) = unpickling[T1](b)
      val (_2, b3) = unpickling[T2](b2)
      ((_1, _2), b3)
    }
  }

  implicit def tuple3Pickler[T1,T2,T3](implicit pickler1: Pickler[T1], pickler2: Pickler[T2], pickler3: Pickler[T3]) = new Pickler[Tuple3[T1,T2,T3]] {
    def pickle(x: Tuple3[T1,T2,T3]) = {
      pickling(x._1) ++ pickling(x._2) ++ pickling(x._3)
    }
    def unpickle(b: Binary) = {
      val (_1, b2) = unpickling[T1](b)
      val (_2, b3) = unpickling[T2](b2)
      val (_3, b4) = unpickling[T3](b3)
      ((_1, _2, _3), b4)
    }
  }

  implicit def tuple4Pickler[T1,T2,T3,T4](implicit pickler1: Pickler[T1], pickler2: Pickler[T2], pickler3: Pickler[T3], pickler4: Pickler[T4]) = new Pickler[Tuple4[T1,T2,T3,T4]] {
    def pickle(x: Tuple4[T1,T2,T3,T4]) = {
      pickling(x._1) ++ pickling(x._2) ++ pickling(x._3) ++ pickling(x._4)
    }
    def unpickle(b: Binary) = {
      val (_1, b2) = unpickling[T1](b)
      val (_2, b3) = unpickling[T2](b2)
      val (_3, b4) = unpickling[T3](b3)
      val (_4, b5) = unpickling[T4](b4)
      ((_1, _2, _3, _4), b5)
    }
  }

  trait Builder[C,E] {
    def append(element: E): Unit
    def build: C
  }

  trait ContainerPickler[C,E] extends Pickler[C] {
    protected def length(c: C): Int
    protected def elements(c: C): Iterator[E]
    protected def builder(length: Int): Builder[C,E]
    protected def elementPickler: Pickler[E]
    final def pickle(c: C) = {
      elements(c).foldLeft(pickling(length(c)))((b: Binary, element: E) => b ++ elementPickler.pickle(element))
    }
    final def unpickle(b: Binary) = {
      val (length, b2) = unpickling[Int](b)
      val builder0 = builder(length)
      def unpickle0(i: Int, b0: Binary): (C, Binary) = {
        if (i >= length) (builder0.build, b0)
        else {
          val (element, b1) = elementPickler.unpickle(b0)
          builder0.append(element)
          unpickle0(i + 1, b1)
        }
      }
      unpickle0(0, b2)
    }
  }

  implicit def listPickler[T](implicit elementPickler0: Pickler[T]) = new ContainerPickler[List[T],T] {
    protected def length(c: List[T]) = c.length
    protected def elements(c: List[T]) = c.elements
    protected def elementPickler = elementPickler0
    protected def builder(length: Int) = new Builder[List[T],T] {
      val buffer = new Array[T](length)
      var i = 0
      def append(element: T) = {
        buffer(i) = element
        i += 1
      }
      def build: List[T] = buffer.toList
    }
  }

  // Not implicit - interferes with String
  def mapPickler[T,U](implicit keyPickler: Pickler[T], valuePickler: Pickler[U]) = new ContainerPickler[Map[T,U],(T,U)] {
    protected def length(c: Map[T,U]) = c.size
    protected def elements(c: Map[T,U]) = c.elements
    protected def elementPickler = tuple2Pickler[T,U]
    protected def builder(length: Int) = new Builder[Map[T,U],(T,U)] {
      var map = Map.empty[T,U]
      def append(element: (T,U)) = {
        map = (map(element._1) = element._2)
      }
      def build: Map[T,U] = map
    }
  }

  implicit def arrayPickler[T](implicit elementPickler0: Pickler[T]) = new ContainerPickler[Array[T],T] {
    protected def length(c: Array[T]) = c.length
    protected def elements(c: Array[T]) = c.elements
    protected def elementPickler = elementPickler0
    protected def builder(length: Int) = new Builder[Array[T],T] {
      val array = new Array[T](length)
      var i = 0
      def append(element: T) = {
        array(i) = element
        i += 1
      }
      def build: Array[T] = array
    }
  }

  implicit def binaryPickler[T] = new Pickler[Binary] {
    def pickle(x: Binary) = {
      pickling(x.length) ++ x
    }
    def unpickle(b: Binary) = {
      val (length, b2) = unpickling[Int](b)
      (b2.slice(0,length).force, b2.slice(length).force)
    }
  }

  implicit val stringPickler = encoderPickler(
    new Encoder[String,Array[Char]] {
      def encode(decoded: String) = decoded.toCharArray
      def decode(encoded: Array[Char]) = new String(encoded)
    }
  )

  implicit def optionPickler[T](implicit elementPickler: Pickler[T]) = new Pickler[Option[T]] {
    def pickle(x: Option[T]) = x match {
      case None => pickling(false)
      case Some(element) => pickling(true) ++ pickling(element)
    }
    def unpickle(b: Binary) = unpickling[Boolean](b) match {
      case (false, b2) => (None, b2)
      case (true, b2) => unpickling[T](b2) match {
        case (element, b3) => (Some(element), b3)
      }
    }
  }
}
