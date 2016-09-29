
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Restricts the integer into the specified range.
    * Exclusive above, which fits with all the other functions we write:-)
    */
  def clampExcAbove(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v >= max) max - 1
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val boxLeft = clampExcAbove(x - radius, 0, src.width)
    val boxRight = clampExcAbove(x + radius, 0, src.width)
    val boxWidth = boxRight - boxLeft + 1 // if radius == 0, width == 1
    val boxTop = clampExcAbove(y - radius, 0, src.height) // y-axis runs down
    val boxBottom = clampExcAbove(y + radius, 0, src.height)
    val boxHeight = boxBottom - boxTop + 1 // if radius == 0, height == 1
    val boxArea = boxWidth * boxHeight

    var rTotal = 0; var gTotal = 0; var bTotal = 0; var aTotal = 0
    var row = boxLeft
    while (row <= boxRight) {
      var col = boxTop
      while (col <= boxBottom) {
        val p = src(row, col)
        rTotal += red(p); gTotal += green(p); bTotal += blue(p); aTotal += alpha(p)
        col += 1
      }
      row += 1
    }
    rgba(rTotal / boxArea, gTotal / boxArea, bTotal / boxArea, aTotal / boxArea)
  }

  /** Utility: split a problem 1..size into buckets
    *
    * @param size size of problem
    * @param buckets to split into
    * @return vector of buckets, each described as (from, until) - inclusive below, exclusive above
    */
  def intervals(size: Int, buckets: Int) = {
    val rem = size % buckets
    if (rem == 0) {
      val bucketWidth = size / buckets
      val loLimits = 0 to size by bucketWidth
      val hiLimits = loLimits.tail
      loLimits zip hiLimits// discard final loLimit
    }
    else {
      val bucketWidth = size / buckets + 1
      val loLimits = 0 until size by bucketWidth
      val hiLimits = loLimits.tail :+ size
      loLimits zip hiLimits
    }
  }

}