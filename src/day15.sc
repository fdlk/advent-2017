import scala.annotation.tailrec

object day15 {
  val divisor: Int = 2147483647
  val judgeBits = Math.pow(2, 16).toInt

  @tailrec
  def generator(factor: Int, multipleOf: Int = 1)(value: Long): Long = {
    val candidate: Long = (value * factor) % divisor
    if (candidate % multipleOf == 0) candidate
    else generator(factor, multipleOf)(candidate)
  }

  @tailrec
  def count(a: Long => Long, b: Long => Long, prevA: Long, prevB: Long, left: Int, found: Int): Int =
    if (left == 0) found
    else {
      val nextA = a(prevA)
      val nextB = b(prevB)
      val judgement: Boolean = nextA % judgeBits == nextB % judgeBits
      count(a, b, nextA, nextB, left - 1, if (judgement) found + 1 else found)
    }

  val part1 = count(generator(16807), generator(48271), 703, 516, 40000000, 0)
  val part2 = count(generator(16807,4), generator(48271,8), 703, 516, 5000000, 0)
}