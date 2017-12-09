import common.loadPackets

object day9 {
  case class State(score: Int = 0,
                   garbageCount: Int = 0,
                   inGroup: Int = 0,
                   garbage: Boolean = false,
                   cancel: Boolean = false) {
    def process(c: Char): State = c match {
      case _ if cancel => copy(cancel = false)
      case '!' if garbage => copy(cancel = true)
      case '<' if !garbage => copy(garbage = true)
      case '>' if garbage => copy(garbage = false)
      case _ if garbage => copy(garbageCount = garbageCount + 1)
      case '{' => copy(inGroup = inGroup + 1)
      case '}' => copy(inGroup = inGroup - 1, score = score + inGroup)
      case _ => this
    }
  }

  val input: String = loadPackets(List("day9.txt")).head
  val result: State = input.foldLeft(State())(_ process _)
  val par1 = result.score
  val part2 = result.garbageCount
}