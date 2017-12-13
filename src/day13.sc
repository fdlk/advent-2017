import common._

object day13 {
  val rawInput: List[String] = loadPackets(List("day13.txt"))
  val depths = rawInput.flatMap(
    _.split(", ").toList
      .map(_.split(": ").toList.map(_.toInt))
      .map({ case List(a, b) => a -> b }))
    .toMap

  def scanner(depth: Int, range: Int, delay: Int=0): Int = {
    val period = 2 * range - 2
    val i = (depth + delay) % period
    if (i <= range - 1) i
    else period - i
  }

  val part1 = depths.map({
    case (depth, range) if scanner(depth, range) == 0 => depth * range
    case _ => 0
  }).sum

  def safe(delay: Int): Boolean =
    depths.forall({case (depth, range) => scanner(depth, range, delay) != 0})

  val part2 = Stream.from(0).find(safe).get
}