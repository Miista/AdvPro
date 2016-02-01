// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen

object MyModule {

  def square(n: Int): Int = n*n

  def abs(n: Int): Int = if (n < 0) -n else n

  private def formatAbs(x: Int) =
    s"The absolute value of $x is ${abs (x)}"

  private def formatSquare(x: Int) =
    s"The squared value of $x is ${square (x)}"

  val magic :Int = 42
  var result :Option[Int] = None

  def main(args: Array[String]): Unit = {
    assert (magic - 84 == magic.-(84))
    println (formatAbs (magic-100))
    println (formatSquare (magic-100))
  }
}