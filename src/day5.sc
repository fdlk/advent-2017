import common._

import scala.annotation.tailrec

object day4 {
  val rawInput = loadPackets(List("day5.txt"))
  val input = rawInput.map(_.toInt).toVector

  @tailrec
  def solve(instructions: Vector[Int], update: Int => Int, ip: Int = 0, counter: Long = 0): Long =
    if (instructions.indices contains ip) {
      val jump = instructions(ip)
      solve(instructions.updated(ip, update(jump)), update, ip + jump, counter + 1)
    } else counter

  solve(input, _ + 1)
  solve(input, jump => if (jump >= 3) jump - 1 else jump + 1)
}