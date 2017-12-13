import common._

object day13 {
  val rawInput: List[String] = loadPackets(List("day13.txt"))
  val regex = """(\d+): (\d+)""".r
  val scanners = rawInput.map({case regex(depth, range) => Scanner(depth.toInt, range.toInt)})

  case class Scanner(depth: Int, range: Int) {
    def detected(delay: Int): Boolean = (depth + delay) % (2 * range - 2) == 0
    def severity: Int = if (detected(0)) depth * range else 0
  }

  def safe(delay: Int): Boolean = scanners.forall(!_.detected(delay))

  val part1 = scanners.map(_.severity).sum
  val part2 = Stream.from(0).find(safe).get
}