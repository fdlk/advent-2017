import common._

object day4 {
  val rawInput = loadPackets(List("day4.txt"))
  val input = rawInput.map(_.split("\\s").toList)

  val part1 = input
    .map(_.groupBy(identity)
      .mapValues(_.length)
      .forall(_._2 == 1))
    .count(identity)

  val part2 = input
    .map(_.groupBy(word => word.sorted)
      .mapValues(_.length)
      .forall(_._2 == 1))
    .count(identity)
}