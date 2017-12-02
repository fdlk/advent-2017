import scala.io.Source._

object day1 {
  val rawInput = fromInputStream(getClass.getResourceAsStream("day2.txt")).getLines.toList
  val input: Seq[List[Int]] = rawInput.map(_.split("\\t").toList.map(_.toInt))

  val part1 = input.map(row => row.max - row.min).sum

  val part2 = input.map(
    _.combinations(2)
      .map(_.sorted)
      .find({ case List(a, b) => b % a == 0 })
      .map({ case List(a, b) => b / a })
      .get
  ).sum
}