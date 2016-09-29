package scalashop

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

@RunWith(classOf[JUnitRunner])
class BlurSuite extends FunSuite {

  def checkEquals(left: Img, right: Img) = {
    assert(left.width === right.width, "differ in width")
    assert(left.height === right.height, "differ in height")
    for (x <- 0 until left.width; y <- 0 until left.height)
      assert(left(x, y) == right(x, y), s"differ at ($x, $y)")
  }

  trait src3by3 {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    val radius = 1
    val expct = new Img(w, h)
    expct.update(0, 0, 2); expct.update(1, 0, 2); expct.update(2, 0, 3)
    expct.update(0, 1, 3); expct.update(1, 1, 4); expct.update(2, 1, 4)
    expct.update(0, 2, 5); expct.update(1, 2, 5); expct.update(2, 2, 6)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    def doChecks(): Unit = {
      checkEquals(dst, expct)
    }

//    def doChecksOld() = {
//      check(0, 0, 2)
//      check(1, 0, 2)
//      check(2, 0, 3)
//      check(0, 1, 3)
//      check(1, 1, 4)
//      check(2, 1, 4)
//      check(0, 2, 0)
//      check(1, 2, 0)
//      check(2, 2, 0)
//    }
  }

  trait src4by3 {
    val w = 4
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 9
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5; src(3, 1) = 10
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8; src(3, 2) = 11

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    def doChecks() = {
      check(0, 0, 4)
      check(1, 0, 5)
      check(2, 0, 5)
      check(3, 0, 6)
      check(0, 1, 4)
      check(1, 1, 5)
      check(2, 1, 5)
      check(3, 1, 6)
      check(0, 2, 4)
      check(1, 2, 5)
      check(2, 2, 5)
      check(3, 2, 6)
    }
  }

  test("boxBlurKernel should correctly handle radius 0 - square") {
    val src = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5)
      src(x, y) = rgba(x, y, x + y, math.abs(x - y))

    for (x <- 0 until 5; y <- 0 until 5)
      assert(boxBlurKernel(src, x, y, 0) === rgba(x, y, x + y, math.abs(x - y)),
        "boxBlurKernel(_,_,0) should be identity.")
  }

  test("boxBlurKernel should correctly handle radius 0 - non-square") {
    val w = 4; val h = 5
    val src = new Img(w, h)

    for (x <- 0 until w; y <- 0 until h)
      src(x, y) = rgba(x, y, x + y, math.abs(x - y))

    for (x <- 0 until w; y <- 0 until h)
      assert(boxBlurKernel(src, x, y, 0) === src(x, y),
        "boxBlurKernel(_,_,0) should be identity.")
  }

  test("boxBlurKernel: interior pixel of a 3x4 image with radius 1") {
    val src = new Img(3, 4)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16

    assert(boxBlurKernel(src, 1, 2, 1) === 12,
      s"(boxBlurKernel(1, 2, 1) should be 12, " +
        s"but it's ${boxBlurKernel(src, 1, 2, 1)})")
  }

  test("boxBlurKernel: bottom right pixel of a 3x4 image with radius 1") {
    val w = 3; val h = 4; val src = new Img(w, h)
    src(0, 0) = 0;  src(1, 0) = 0;  src(2, 0) = 0
    src(0, 1) = 0;  src(1, 1) = 0;  src(2, 1) = 0
    src(0, 2) = 0;  src(1, 2) = 7;  src(2, 2) = 8
    src(0, 3) = 0;  src(1, 3) = 11; src(2, 3) = 14

    assert(boxBlurKernel(src, w - 1, h - 1, 1) === 10, "(boxBlurKernel(3, 4, 1) should be 10")
  }

  test("boxBlurKernel: radius 1, entire 3x3") {
    new src3by3 {
      for (x <- 0 until 3; y <- 0 until 3)
        dst.update(x, y, boxBlurKernel(src, x, y, 1))
      checkEquals(dst, expct)
    }
  }

  test("intervals: 15 in 4 ") {
    assert(intervals(15, 4) == Vector((0,4), (4,8), (8,12), (12,15)))
  }

  test("intervals: 16 in 4 ") {
    assert(intervals(16, 4) == Vector((0,4), (4,8), (8,12), (12,16)))
  }

  test("intervals: 13 in 4 ") {
    assert(intervals(13, 4) == Vector((0,4), (4,8), (8,12), (12,13)))
  }

  test("intervals: 4 in 1 ") {
    assert(intervals(4, 1) == Vector((0,4)))
  }


  test("VerticalBoxBlur.blur: radius 2, entire 4x3 image") {
    new src4by3 {
      VerticalBoxBlur.blur(src, dst, 0, 4, 2)
      doChecks()
    }
  }

  test("VerticalBoxBlur.parBlur: 1 task, radius 2, entire 4x3 image") {
    new src4by3 {
      VerticalBoxBlur.parBlur(src, dst, 1, 2)
      doChecks()
    }
  }

  test("VerticalBoxBlur.parBlur: 2 tasks, radius 2, entire 4x3 image") {
    new src4by3 {
      VerticalBoxBlur.parBlur(src, dst, 2, 2)
      doChecks()
    }
  }

  test("VerticalBoxBlur.parBlur: 3 tasks, radius 2, entire 4x3 image") {
    new src4by3 {
      VerticalBoxBlur.parBlur(src, dst, 3, 2)
      doChecks()
    }
  }

  test("HorizontalBoxBlur.blur: radius 1, entire 3x3 image") {
    new src3by3 {
      HorizontalBoxBlur.blur(src, dst, 0, 3, 1)
      doChecks()
    }
  }

  test("HorizontalBoxBlur.parBlur: 1 task, radius 1, entire 3x3 image") {
    new src3by3 {
      HorizontalBoxBlur.parBlur(src, dst, 1, 1)
      doChecks()
    }
  }

  test("HorizontalBoxBlur.parBlur: 2 task, radius 1, entire 3x3 image") {
    new src3by3 {
      HorizontalBoxBlur.parBlur(src, dst, 2, 1)
      doChecks()
    }
  }

  test("HorizontalBoxBlur.parBlur: 3 task, radius 1, entire 3x3 image") {
    new src3by3 {
      HorizontalBoxBlur.parBlur(src, dst, 3, 1)
      doChecks()
    }
  }
}
