import common.loadPackets

object day9 {

  case class Counters(score: Int=0, garbage: Int=0, nesting: Int=0){
    def collectGarbage: Counters = copy(garbage = garbage + 1)
    def startGroup: Counters = copy(nesting = nesting + 1)
    def endGroup: Counters = copy(nesting = nesting - 1, score = score + nesting)
  }

  sealed trait State {
    def process(c: Char): State
    def counters: Counters
  }

  case class Ignore(counters: Counters) extends State {
    def process(c: Char): State = Garbage(counters)
  }

  case class Garbage(counters: Counters) extends State {
    def process(c: Char): State = c match {
      case '!' => Ignore(counters)
      case '>' => Normal(counters)
      case _ => Garbage(counters.collectGarbage)
    }
  }

  case class Normal(counters: Counters) extends State {
    def process(c: Char): State = c match {
      case '<' => Garbage(counters)
      case '{' => Normal(counters.startGroup)
      case '}' => Normal(counters.endGroup)
      case _ => this
    }
  }

  def count(input: String): Counters = {
    val initialState: State = Normal(Counters())
    input.foldLeft(initialState)(_ process _).counters
  }

  val input: String = loadPackets(List("day9.txt")).head
  val result = count(input)
  val part1 = result.score
  val part2 = result.garbage
}