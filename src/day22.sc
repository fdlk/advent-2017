
object day22 {
  case class Coordinates(x: Int, y: Int) {
    def step(d: Direction): Coordinates = d match {
      case Direction(dx, dy) => Coordinates(x + dx, y + dy)
    }
  }

  case class Direction(dx: Int, dy: Int) {
    def right: Direction = Direction(-dy, dx)
    def left: Direction = Direction(dy, -dx)
    def reverse: Direction = Direction(-dx, -dy)
  }

  val Up = Direction(0, -1)
  val Down = Direction(0, 1)
  val Left = Direction(-1, 0)
  val Right = Direction(1, 0)

  sealed trait Infection
  case object Weakened extends Infection
  case object Infected extends Infection
  case object Flagged extends Infection

  val input = common.loadPackets(List("day22.txt"))
  val infected = input.zipWithIndex
    .flatMap({ case (line, y) =>
      line.zipWithIndex.filter(_._1 == '#').map({ case (_, x) => Coordinates(x, y) })
    }).map(c => (c, Infected)).toMap

  val center = input.head.length / 2

  case class State(location: Coordinates, facing: Direction, infected: Map[Coordinates, Infection], causedInfection:Int = 0) {
    def minX = infected.keys.map(_.x).min
    def maxX = infected.keys.map(_.x).max
    def minY = infected.keys.map(_.y).min
    def maxY = infected.keys.map(_.y).max
    def charAt(c: Coordinates): Char = if (c == location) 'o' else {
      infected.get(c) match {
        case Some(Weakened) => 'W'
        case Some(Infected) => '#'
        case Some(Flagged) => 'F'
        case None => '.'
      }
    }
    override def toString: String = (for (y <- minY to maxY) yield
      (for (x <- minX to maxX) yield charAt(Coordinates(x, y))).mkString).mkString("\n", "\n", "\n")

    def turn = infected.get(location) match {
      case Some(Weakened) => this
      case Some(Infected) => copy(facing = facing.right)
      case Some(Flagged) =>  copy(facing = facing.reverse)
      case None => copy(facing = facing.left)
    }
    def cleanOrInfect = infected.get(location) match {
      case Some(Weakened) => copy(infected = infected.updated(location, Infected), causedInfection = causedInfection + 1)
      case Some(Infected) => copy(infected = infected.updated(location, Flagged))
      case Some(Flagged) =>  copy(infected = infected - location)
      case None => copy(infected = infected.updated(location, Weakened))
    }
    def forward: State = copy(location = location.step(facing))
    def next: State = turn.cleanOrInfect.forward
  }

  val start = State(Coordinates(center, center), Up, infected)

  val part2 = Iterator.iterate(start)(_.next).drop(10000000).next.causedInfection
}