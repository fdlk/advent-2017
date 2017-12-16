object day16 {
  val input = common.loadPackets(List("day16.txt")).head

  def spin(x: Int)(state: String) = state.takeRight(x) ++ state.dropRight(x)
  def exchange(a: Int, b: Int)(state: String) = state.updated(a, state(b)).updated(b, state(a))
  def partner(a: Char, b: Char)(state: String) = exchange(state.indexOf(a), state.indexOf(b))(state)

  val spinRegex = """s(\d+)""".r
  val exchangeRegex = """x(\d+)/(\d+)""".r
  val partnerRegex = """p(\w)/(\w)""".r
  val moves: List[String => String] = input.split(",").map({
    case spinRegex(x) => spin(x.toInt) _
    case exchangeRegex(a, b) => exchange(a.toInt, b.toInt) _
    case partnerRegex(a, b) => partner(a(0), b(0)) _
  }).toList

  def dance(state: String): String = moves.foldLeft(state)((state, move) => move(state))

  val initialState: String = ('a' to 'p').mkString
  val part1 = dance(initialState)
  val danceOn = Stream.iterate(initialState)(dance)
  val period = danceOn.indexOf(initialState, 1)
  val part2 = danceOn.drop(1000000000 % period).head
}