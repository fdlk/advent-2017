
object day22 {
  case class Coordinates(x: Int, y: Int) {
    def step(d: Direction): Coordinates = d match {
      case Direction(dx, dy) => Coordinates(x + dx, y + dy)
    }
  }

  case class Direction(dx: Int, dy: Int) {
    def right: Direction = Direction(-dy, dx)

    def left: Direction = Direction(dy, -dx)
  }

  val Up = Direction(0, -1)
  val Down = Direction(0, 1)
  val Left = Direction(-1, 0)
  val Right = Direction(1, 0)

  val input = common.loadPackets(List("day22.txt"))
  val infected = input.zipWithIndex
    .flatMap({ case (line, y) =>
      line.zipWithIndex.filter(_._1 == '#').map({ case (_, x) => Coordinates(x, y) })
    }).toSet

  val center = input.head.length / 2

  case class State(location: Coordinates, facing: Direction, infected: Set[Coordinates], causedInfection:Int = 0) {
    def minX = infected.map(_.x).min
    def maxX = infected.map(_.x).max
    def minY = infected.map(_.y).min
    def maxY = infected.map(_.y).max
    def charAt(c: Coordinates): Char = if (c == location) 'o' else if (infected.contains(c)) '#' else '.'
    override def toString: String = (for (y <- minY to maxY) yield
      (for (x <- minX to maxX) yield charAt(Coordinates(x, y))).mkString).mkString("\n", "\n", "\n")

    def turn = if (infected.contains(location)) copy(facing = facing.right) else copy(facing = facing.left)
    def cleanOrInfect = if (infected.contains(location)) copy(infected = infected - location)
      else copy(infected = infected + location, causedInfection = causedInfection + 1)
    def forward: State = copy(location = location.step(facing))
    def next: State = turn.cleanOrInfect.forward
  }

  val start = State(Coordinates(center, center), Up, infected)
  val part1 = Iterator.iterate(start)(_.next).drop(10000).next.causedInfection
}