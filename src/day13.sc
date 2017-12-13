import common._

object day13 {
  val rawInput: List[String] = loadPackets(List("day13.txt"))
  val depths = rawInput.flatMap(
    _.split(", ").toList
      .map(_.split(": ").toList.map(_.toInt))
      .map({ case List(a, b) => a -> b }))
    .toMap

  def detected(depth: Int, range: Int, delay: Int=0): Boolean =
    (depth + delay) % (2 * range - 2) == 0

  val part1 = depths.map({
    case (depth, range) if detected(depth, range) => depth * range
    case _ => 0
  }).sum

  def safe(delay: Int): Boolean =
    depths.forall({case (depth, range) => !detected(depth, range, delay)})

  val part2 = Stream.from(0).find(safe).get
}