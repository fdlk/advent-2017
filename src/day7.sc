import common.loadPackets

object day7 {

  case class Program(name: String, weight: Int, children: List[String])

  val regex = """(\w+) \((\d+)\)( -> (.*))?""".r
  val input = loadPackets(List("day7.txt")).map({
    case regex(name, weight, _, children) => Program(name, weight.toInt, Option(children).map(_.split(", ").toList).getOrElse(Nil))
  })
  val allChildren = input.flatMap(_.children).toSet

  val part1 = input.map(_.name).find(!allChildren.contains(_)).get

  val lookup = input.map(program => program.name -> program).toMap

  def weight(program: Program): Int = program.weight + program.children.map(lookup).map(weight).sum

  def findNorm(weights: List[Int]): Option[Int] =
    if (weights.toSet.size <= 1) None
    else weights match {
      case x :: y :: z :: _ => if (x == y) Some(x) else Some(z)
    }

  def part2(program: Program, parentNorm: Int = 0): Int = {
    val children = program.children.map(lookup)
    findNorm(children.map(weight))
      .map(norm => part2(children.find(weight(_) != norm).get, norm))
      .getOrElse(program.weight + parentNorm - weight(program))
  }

  part2(lookup(part1))
}