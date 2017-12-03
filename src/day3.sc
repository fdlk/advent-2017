import spire.math.Complex
import spire.implicits._

object day3 {
  // Instructions
  sealed trait Instruction
  case object Turn extends Instruction
  case object Move extends Instruction

  // State
  type Direction = Complex[Float]
  type Location = Complex[Float]

  def neighbors(location: Location): List[Location] = {
    (for (dx <- -1 to 1; dy <- -1 to 1 if dx != 0 || dy != 0)
      yield location + Complex(dx.toFloat, dy.toFloat)).toList
  }

  case class State(location: Location, facing: Direction, stored: Map[Location,Int]) {
    def follow(instruction: Instruction): State = instruction match {
      case Turn => State(location, facing * Complex.i, stored)
      case Move =>
        val nextLocation = location + facing
        val valueToStore = neighbors(nextLocation).map(location => stored.getOrElse(location, 0)).sum
        State(nextLocation, facing, stored.updated(nextLocation, valueToStore))
    }
    def distance: Long = (location.real.abs + location.imag.abs).round
  }

  // Simulation
  // create an infinite stream of instructions for the route
  val instructions: Stream[Instruction] = Stream.from(1)
    .flatMap(n => Stream(n, n)) // two edges of size n for each size
    .flatMap(n => Stream.fill(n)(Move) #::: Stream[Instruction](Turn)) // n moves and one turn per edge

  val initialState = State(Complex.zero, Complex.one, Map(Complex.zero -> 1))
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