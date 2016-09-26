package example

import org.scalatest.FunSuite

import ch.epfl.lamp.grading.GradingSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ListsSuite extends FunSuite {
  /**
   * Now we finally write some tests for the list functions that have to be
   * implemented for this assignment. We fist import all members of the
   * `List` object.
   */
  import Lists._

  test("sum of a few numbers") {
    assert(sum(List(1,2,0)) === 3)
  }

  test("max of empty list throw NoSuchElementException") {
    intercept[NoSuchElementException] {
      max(Nil)
    }
  }

  test("max of a few numbers") {
    assert(max(List(3, 7, 2)) === 7)
  }
}
