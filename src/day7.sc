import common.loadPackets

object day7 {
  val rawInput = loadPackets(List("day7.txt"))
  val regex = """(\w+) \((\d+)\)( -> (.*))?""".r

  case class Program(name: String, weight: Int, children: List[String])

  def parse(line: String) = line match {
    case regex(name, weight, _, children) =>
      Program(name, weight.toInt, if (children != null) children.split(", ").toList else Nil)
  }

  val input = rawInput.map(parse)
  val programs = input.map(p => p.name -> p).toMap

  val allChildren = input.flatMap(_.children)
  val part1 = input.find(p => p.children.nonEmpty && !allChildren.contains(p.name)).get.name

  def weight(name: String): Int = {
    val p = programs(name)
    p.weight + p.children.map(weight).sum
  }

  def part2(name: String): Option[Int] = {
    val children = programs(name).children
    if (children.map(weight).toSet.size <= 1)
      Option.empty
    else {
      val balancedWeight = children.map(weight).groupBy(identity).maxBy(_._2.size)._1
      val problemChild = children.find(weight(_) != balancedWeight).get
      part2(problemChild)
        .orElse(Some(programs(problemChild).weight + balancedWeight - weight(problemChild)))
    }
  }

  part2(part1).get
}