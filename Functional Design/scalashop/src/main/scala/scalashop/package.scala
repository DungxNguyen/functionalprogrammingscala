
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

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */

  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    boxBlurKernelFunctional(src, x, y, radius)
  }

  def boxBlurKernelImperative(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    var redValue = 0
    var greenValue = 0
    var blueValue = 0
    var alphaValue = 0
    var xValue = clamp(x - radius, 0, src.width - 1)
    var numberOfPixel = 0
    while (xValue < src.width && xValue <= x + radius) {
      var yValue = clamp(y - radius, 0, src.height - 1)
      while (yValue < src.height && yValue <= y + radius) {
        val thisPixel = src(xValue, yValue)
        redValue += red(thisPixel)
        greenValue += green(thisPixel)
        blueValue += blue(thisPixel)
        alphaValue += alpha(thisPixel)
        numberOfPixel += 1
        yValue += 1
      }
      xValue += 1
    }
    rgba(redValue / numberOfPixel, greenValue / numberOfPixel, blueValue / numberOfPixel, alphaValue / numberOfPixel)
  }

  def minOf(a: Int, b: Int): Int = if (a < b) a else b
  def maxOf(a: Int, b: Int): Int = if (a < b) b else a

  def boxBlurKernelFunctional(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    (for {
      col <- maxOf(0, x - radius) to minOf(x + radius, src.width - 1)
      row <- maxOf(0, y - radius) to minOf(y + radius, src.height - 1)
    } yield (col, row)).toList.map { case (x, y) => src(x, y) }.
      map { x => List(red(x), green(x), blue(x), alpha(x)) }.transpose.
      map(x => x.sum / x.size) match {
        case List(r, g, b, a) => rgba(r, g, b, a)
      }
  }

}
