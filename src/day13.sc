import common._

object day13 {
  val rawInput: List[String] = loadPackets(List("day13.txt"))
  val regex = """(\d+): (\d+)""".r
  val scanners = rawInput.map({case regex(depth, range) => Scanner(depth.toInt, range.toInt)})

  case class Scanner(depth: Int, range: Int) {
    def detected(delay: Int): Boolean = (depth + delay) % (2 * range - 2) == 0
    def severity: Option[Int] = if (detected(0)) Some(depth * range) else None
  }

  def safe(delay: Int): Boolean = scanners.forall(!_.detected(delay))

  val part1 = scanners.flatMap(_.severity).sum
  val part2 = Stream.from(0).find(safe).get
}