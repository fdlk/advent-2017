object day24 {
  type Component = List[Int]

  case class Bridge(components: List[Component] = Nil, endPoint: Int = 0) {
    def strength: Int = components.flatten.sum
    def length: Int = components.length
    def connect(c: Component): Option[Bridge] = c match {
      case List(p1, p2) if p1 == endPoint => Some(Bridge(c :: components, p2))
      case List(p1, p2) if p2 == endPoint => Some(Bridge(c :: components, p1))
      case _ => None
    }
  }

  def buildBridges(base: Bridge, components: Set[Component]): Iterator[Bridge] = {
    val bridges = components.toIterator.flatMap(base.connect)
    if (bridges.isEmpty) Iterator(base)
    else bridges.flatMap(bridge => buildBridges(bridge, components - bridge.components.head))
  }

  val input = common.loadPackets(List("day24.txt"))
  val components = input.map(_.split("/").map(_.toInt).toList).toSet
  val part1 = buildBridges(Bridge(), components).map(_.strength).max
  val part2 = buildBridges(Bridge(), components).map(bridge => (bridge.length, bridge.strength)).max._2
}