package com.richdougherty.binary

import org.testng.annotations.{Test, BeforeMethod}

import org.scalatest.testng.TestNGSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.scalatest._

class PicklingSuite extends Suite with StandardPickling with Checkers {

  private def bipickle[A](x: A)(implicit p: Pickler[A]): Unit = {
    def bipickle0(suffix: Binary): Unit = {
      //println("Pickling: " + x)
      val b = p.pickle(x)
      //println("Pickle: " + b)
      val (x0, b0) = p.unpickle(b ++ suffix)
      assert(x == x0)
      assert(suffix == b0)
    }
    bipickle0(Binary.empty)
    bipickle0(Binary(1))
    bipickle0(Binary.fromString("hello"))
  }

  def checkBipickle[A](implicit p: Pickler[A], arb: Arbitrary[A]): Unit = {
    check { (x: A) =>
      //println(x)
      val b = p.pickle(x)
      val (x0, b0) = p.unpickle(b)
      x == x0
    }
  }

  @Test
  def testPickling: Unit = {
    bipickle(Math.MAX_BYTE)
    bipickle(Math.MIN_BYTE)
    bipickle(Math.MAX_INT)
    bipickle(Math.MIN_INT)
    checkBipickle[Int]
    checkBipickle[Byte]
    checkBipickle[String]
    checkBipickle[Boolean]
    bipickle((1,))
    bipickle(("abc",))
    checkBipickle[Tuple2[String,Char]]
    checkBipickle[Tuple2[List[Byte],Byte]]
    checkBipickle[Tuple3[Int,Int,Int]]
    checkBipickle[Tuple4[Int,Int,Int,Int]]

    checkBipickle[List[Char]]
    checkBipickle[List[Byte]]
    bipickle[List[Int]](Nil)
    bipickle[List[Nothing]](Nil)

    bipickle[Option[Nothing]](None)
    bipickle[Option[Int]](Some(1))

    // assertEquals fails because of Scala bug with Array boxing (I think)
    //bipickle(Array(1, 2, 3, 4).asInstanceOf[Array[Int]])
    //bipickle(new Array[String](0))
    //bipickle(new Array[String](2))

    bipickle(Binary.empty)
    bipickle(Binary(1, 2, 3, 4))
  }

}
