object day16 {
  val input = common.loadPackets(List("day16.txt")).head

  sealed trait Move {
    def apply(state: String): String
  }

  case class Spin(x: Int) extends Move {
    def apply(state: String) = state.drop(state.length - x) ++ state.take(state.length - x)
  }

  case class Exchange(a: Int, b: Int) extends Move {
    def apply(state: String) = state.updated(a, state(b)).updated(b, state(a))
  }

  case class Partner(a: Char, b: Char) extends Move {
    def apply(state: String) = Exchange(state.indexOf(a), state.indexOf(b)).apply(state)
  }

  val spinRegex = """s(\d+)""".r
  val exchangeRegex = """x(\d+)/(\d+)""".r
  val partnerRegex = """p(\w)/(\w)""".r
  val moves: List[Move] = input.split(",").map({
    case spinRegex(x) => Spin(x.toInt)
    case exchangeRegex(a, b) => Exchange(a.toInt, b.toInt)
    case partnerRegex(a, b) => Partner(a.charAt(0), b.charAt(0))
  }).toList

  def dance(state: String): String = moves.foldLeft(state)((state, move) => move(state))

  val initialState: String = ('a' to 'p').mkString
  val part1 = dance(initialState)
  val danceOn = Stream.iterate(initialState)(dance)
  val period = danceOn.indexOf(initialState, 1)
  val part2 = danceOn.drop(1000000000 % period).head
}