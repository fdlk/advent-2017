import common._

import scala.annotation.tailrec

object day4 {
  val rawInput = loadPackets(List("day5.txt"))
  val input = rawInput.map(_.toInt).toVector

  @tailrec
  def solve(instructions: Vector[Int], ip: Int, counter: Long, update: Int => Int): Long =
    if (instructions.indices.contains(ip)) {
      val jump = instructions(ip)
      solve(instructions.updated(ip, update(jump)), ip + jump, counter + 1, update)
    } else counter

  solve(input, 0, 0, _ + 1)
  solve(input, 0, 0, jump => if (jump >= 3) jump - 1 else jump + 1)
}