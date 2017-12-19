object day19 {
  val maze = common.loadPackets(List("day19.txt"))

  val maxX = maze.map(_.length).max
  val maxY = maze.length

  abstract class Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction

  case class State(x: Int, y: Int, direction: Direction) {
    def char: Option[Char] = Some(maze(y))
        .filter(_.length > x)
        .map(_(x))
        .filter(_ != ' ')

    def hasPath: Boolean = char.isDefined

    def move(d: Direction): Option[State] = (d match {
        case Up    if y > 0        => Some(copy(y = y - 1, direction = Up))
        case Down  if y < maxY - 1 => Some(copy(y = y + 1, direction = Down))
        case Left  if x > 0        => Some(copy(x = x - 1, direction = Left))
        case Right if x < maxX - 1 => Some(copy(x = x + 1, direction = Right))
        case _ => None
      }).filter(_.hasPath)

    def next: Option[State] = char match {
      case Some('+') => direction match {
        case Up   | Down  => move(Left).orElse(move(Right))
        case Left | Right => move(Up).orElse(move(Down))
      }
      case Some(_) => move(direction)
    }
  }

  val initial: Option[State] = Some(State(maze.head.indexOf("|"), 0, Down))
  val path = Stream.iterate(initial)(_.flatMap(_.next))
  val route = path.takeWhile(_.isDefined).toList
  val part1 = route.map(_.get.char.get).filter(_.isLetter).mkString
  val part2 = route.length
}