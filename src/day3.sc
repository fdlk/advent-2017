object day3 {
  // Instructions
  sealed trait Instruction
  case object Turn extends Instruction
  case object Move extends Instruction

  // State
  sealed trait Direction {
    def turn: Direction = this match {
      case North => West
      case East => North
      case South => East
      case West => South
    }
  }

  case object North extends Direction
  case object East extends Direction
  case object South extends Direction
  case object West extends Direction

  case class Location(x: Int, y: Int) {
    def move(direction: Direction): Location = direction match {
      case North => Location(x, y + 1)
      case East => Location(x + 1, y)
      case South => Location(x, y - 1)
      case West => Location(x - 1, y)
    }
    def neighbors: List[Location] = List(
      move(East),
      move(East).move(North),
      move(North),
      move(West).move(North),
      move(West),
      move(West).move(South),
      move(South),
      move(East).move(South)
    )
    def distance(): Int = x.abs + y.abs
  }

  case class State(location: Location, facing: Direction, stored: Map[Location,Int]) {
    def follow(instruction: Instruction): State = instruction match {
      case Turn => State(location, facing.turn, stored)
      case Move =>
        val nextLocation = location.move(facing)
        val valueToStore = nextLocation.neighbors.map(location => stored.getOrElse(location, 0)).sum
        State(nextLocation, facing, stored.updated(nextLocation, valueToStore))
    }
    def distance: Int = location.distance()
  }

  // Simulation
  // create an infinite stream of instructions for the route
  val instructions: Stream[Instruction] = Stream.from(1)
    .flatMap(n => Stream(n, n)) // two edges of size n for each size
    .flatMap(n => Stream.fill(n)(Move) #::: Stream[Instruction](Turn)) // n moves and one turn per edge

  val initialState = State(Location(0,0), East, Map(Location(0,0) -> 1))
  val route: Stream[State] = instructions.scanLeft(initialState)(_.follow(_))

  // Solution
  val input = 265149
  val part1 = route
    .find(_.stored.size == input)
    .get
    .distance

  val part2 = route
    .map(_.stored.maxBy(_._2))
    .map(_._2)
    .find(_ > input)
    .get
}