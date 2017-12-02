import common._

object day1 {
  val input = loadPackets(List("day1.txt")).head.toList.map(_.asDigit)

  def captcha(input: List[Int], offset: Int): Int = {
    val rotated = input.drop(offset) ::: input.take(offset)
    input
      .zip(rotated)
      .filter(x => x._1 == x._2)
      .map(_._1)
      .sum
  }

  captcha(input, 1)
  captcha(input, input.length/2)
}