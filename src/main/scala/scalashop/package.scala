

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int
  type Accum = (Int, Int, Int, Int)

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

  def sumComponents(accum: Accum, pixel: RGBA): Accum = {
    (accum._1 + red(pixel), accum._2 + green(pixel), accum._3 + blue(pixel), accum._4 + alpha(pixel))
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val left = clamp(x - radius, 0, src.width - 1)
    val right = clamp(x + radius, 0, src.width - 1)
    val top = clamp(y - radius, 0, src.height - 1)
    val bottom = clamp(y + radius, 0, src.height - 1)
    implicit val pixels = Math.max(0, (right + 1 - left) * (bottom + 1 - top))

    if (pixels == 0) src(x, y)
    else {
      var currSum: Accum = (0, 0, 0, 0)

      for (i <- left to right if i < src.width) {
        for (j <- top to bottom if j < src.height) {
            currSum = sumComponents(currSum, src(i, j))
        }
      }

      def avg(a: Double) (implicit b: Int) = (a / b).asInstanceOf[Int]

      rgba(avg(currSum._1), avg(currSum._2), avg(currSum._3), avg(currSum._4))
    }
  }

}
