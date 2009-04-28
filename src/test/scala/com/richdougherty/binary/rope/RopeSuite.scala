package com.richdougherty.binary.rope

import org.testng.annotations.{Test, BeforeMethod}

import org.scalatest.testng.TestNGSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.scalatest._

/**
 * Tests for Rope objects.
 *
 * @author <a href="http://www.richdougherty.com/">Rich Dougherty</a>
 */
class RopeSuite extends Suite with Checkers {

  implicit def arbRope: Arbitrary[Rope] = Arbitrary {
    def genLeafRopeMul(multiplier: Int) = 
      for (prefix <- Arbitrary.arbitrary[Array[Byte]];
           content <- Arbitrary.arbitrary[Array[Byte]];
           suffix <- Arbitrary.arbitrary[Array[Byte]]) yield {
        val ropes = List(prefix) ++ (for (_ <- 0 until multiplier) yield { content }) ++ List(suffix)
        val combined = ropes.foldLeft(new Array[Byte](0))((a: Array[Byte], b: Array[Byte]) => a ++ b)
        Rope.unsafe_wrapArray(combined, prefix.length, content.length * multiplier)
      }

    def genLeafRope: Gen[Rope] = Gen.frequency(
      (1, Gen.value(Rope.empty)),
      (8, genLeafRopeMul(1)),
      (3, genLeafRopeMul(10)),
      (2, genLeafRopeMul(100))
    )

    def genRope(maxDepth: Int): Gen[Rope] = {
      if (maxDepth == 0) {
        genLeafRope
      } else {
        Gen.frequency(
          (1, genLeafRope),
          (2, for (rope1 <- genRope(maxDepth - 1);
                   rope2 <- genRope(maxDepth - 1)) yield {
              rope1 ++ rope2
            })
        )
      }
    }
    Gen.frequency(
      (1, genRope(0)),
      (1, genRope(1)),
      (1, genRope(2)),
      (1, genRope(3)),
      (1, genRope(10))
    )
  }

  def element(seq: RandomAccessSeq[Byte], i: Int): Option[Byte] = try {
    Some(seq(i))
  } catch {
    case _: IndexOutOfBoundsException => None
  }

  // Workaround for bug #1309.
  def fixRAS(any: Any) = any match {
    case a: Array[Byte] => a
    case ras: RandomAccessSeq[_] => ras.asInstanceOf[RandomAccessSeq[Byte]]
  }

  def same(aThunk: => RandomAccessSeq[Byte], bThunk: => RandomAccessSeq[Byte]): Boolean = {
    import java.lang.Exception
    def getResult(thunk: => RandomAccessSeq[Byte]): Either[Class[Exception], RandomAccessSeq[Byte]] = try {
      Right(thunk)
    } catch {
      case e: Exception => {
        val eClass = e.getClass
        //println(eClass)
        Left(eClass.asInstanceOf[Class[Exception]])
      }
    }
    val aResult = getResult(aThunk)
    val bResult = getResult(bThunk)
    if (aResult.isLeft && bResult.isLeft) {
      aResult.left.get == bResult.left.get
    } else if (aResult.isRight && bResult.isRight) {
      //println(aResult.right.get.getClass)
      val a: RandomAccessSeq[Byte] = fixRAS(aResult.right.get)
      val b: RandomAccessSeq[Byte] = fixRAS(bResult.right.get)
      if (a.size != b.size) return false
      for (i <- -5 until (a.size + 5)) { if (element(a, i) != element(b, i)) return false }
      true
    } else {
      false
    }
  }

  @Test
  def testCreate = {
    check((array: Array[Byte]) =>
      same(Rope.fromArray(array), array))
    check((array: Array[Byte]) =>
      same(Rope.fromArray(array), array))
    check { (b0: Byte) =>
      val rope = Rope(b0)
      b0 == rope(0) && rope.length == 1
    }
    check { (b0: Byte, b1: Byte) =>
      val rope = Rope(b0, b1)
      b0 == rope(0) && b1 == rope(1) && rope.length == 2
    }
    check { (b0: Byte, b1: Byte, b2: Byte) =>
      val rope = Rope(b0, b1, b2)
      b0 == rope(0) && b1 == rope(1) && b2 == rope(2) && rope.length == 3
    }
  }

  @Test
  def testCreateWithOffset = {
    check((array: Array[Byte], pre: Array[Byte], post: Array[Byte]) => {
      val joined = pre ++ array ++ post
      Rope.fromArray(joined, pre.length, array.length) == Rope.fromArray(array)
    })
  }

  @Test
  def testToArray = {
    check((array: Array[Byte]) =>
      same(Rope.fromArray(array).toArray, array))
  }

  @Test
  def testEquals = {
    check((rope: Rope) =>
      rope == rope)
    check((rope: Rope) =>
      rope == Rope.fromArray(rope.toArray))
  }

  @Test
  def testHashCode = {
    check((rope: Rope) =>
      rope.hashCode == rope.hashCode)
    check((rope: Rope) =>
      rope.hashCode == Rope.fromArray(rope.toArray).hashCode)
  }

  @Test
  def testImmutable = {
    check((array: Array[Byte]) =>
      (array.length >= 1) ==> {
        val rope = Rope.fromArray(array)
        for (i <- 0 until array.length) array(i) = (array(i) + 1).asInstanceOf[Byte]
        !same(rope, array)
    })
  }

  @Test
  def testAppend = {
    check((array1: Array[Byte], array2: Array[Byte]) =>
        same(array1 ++ array2, Rope.fromArray(array1) ++ Rope.fromArray(array2)))
    check((array1: Array[Byte], array2: Array[Byte]) =>
        same(array1 ++ array2, (Rope.fromArray(array1) ++ Rope.fromArray(array2)).toArray))
    check((array1: Array[Byte], array2: Array[Byte]) =>
        same(array1, (Rope.fromArray(array1) ++ Rope.fromArray(array2)).slice(0, array1.length)))
    check((array1: Array[Byte], array2: Array[Byte]) =>
        same(array2, (Rope.fromArray(array1) ++ Rope.fromArray(array2)).slice(array1.length, array1.length + array2.length)))
    check((array1: Array[Byte], array2: Array[Byte]) =>
        same(array2, (Rope.fromArray(array1) ++ Rope.fromArray(array2)).slice(array1.length).force))
    check { (arrays: List[Array[Byte]]) =>
      val arrayAppend = arrays.foldLeft(new Array[Byte](0)) { (_: Array[Byte]) ++ (_: Array[Byte]) }
      val ropeAppend = arrays.foldLeft(Rope.empty) { (_: Rope) ++ Rope.fromArray((_: Array[Byte])) }
      //println(arrayAppend)
      //println(ropeAppend)
      same(arrayAppend, ropeAppend)
    }
  }
  
  @Test
  def testSlice = {
    val array = new Array[Byte](0)
    same(array.take(5), Rope.fromArray(array).take(5))
    check((array: Array[Byte]) =>
        same(array.take(5), Rope.fromArray(array).take(5)))
  }
  
  @Test
  def testTake = {
    val array = new Array[Byte](0)
    same(array.take(5), Rope.fromArray(array).take(5))
    check((array: Array[Byte]) =>
        same(array.take(5), Rope.fromArray(array).take(5)))
  }

  @Test
  def testDecodeString = {
    // FIXME: Make this test a bit more comprehensive!
    assert(Rope.fromString("hello world").decodeString("UTF8") == ("hello world"))
  }

}
