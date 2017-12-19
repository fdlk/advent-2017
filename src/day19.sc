object day19 {
  val maze = common.loadPackets(List("day19.txt"))

  case class Direction(dx: Int, dy: Int)
  val Up = Direction(0, -1)
  val Down = Direction(0, 1)
  val Left = Direction(-1, 0)
  val Right = Direction(1, 0)

  case class State(x: Int, y: Int, direction: Direction) {
    def char: Option[Char] = Some(maze)
      .filter(_.indices.contains(y))
      .map(_ (y))
      .filter(_.indices.contains(x))
      .map(_ (x))
      .filter(_ != ' ')

    def move(d: Direction): Option[State] = d match {
      case Direction(dx, dy) => Some(State(x + dx, y + dy, d)).filter(_.char.isDefined)
    }

    def next: Option[State] = char.get match {
      case '+' => direction match {
        case Up   | Down  => move(Left).orElse(move(Right))
        case Left | Right => move(Up).orElse(move(Down))
      }
      case _ => move(direction)
    }
  }

  val startX = maze.head.indexOf("|")
  val route: Stream[State] = Stream.iterate[Option[State]](Some(State(startX, 0, Down)))(_.flatMap(_.next))
    .takeWhile(_.isDefined).map(_.get)
  val part1 = route.map(_.char.get).filter(_.isLetter).mkString
  val part2 = route.length
}