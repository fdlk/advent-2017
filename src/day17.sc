object day17 {
  val step: Int = 344

  case class State(buffer: List[Int] = List(0), pos: Int = 0) {
    val n = buffer.length
    def next: State = {
      val newPos = (pos + step) % n + 1
      State(buffer.patch(newPos, List(n), 0), newPos)
    }
    def elementAfterPos = buffer(pos + 1)
  }

  case class SimplifiedState(elementAfterZero: Int=0, n: Int=1, pos: Int=1) {
    def next: SimplifiedState = {
      val newPos = (pos + step) % n + 1
      SimplifiedState(if (newPos == 1) n else elementAfterZero, n + 1, newPos)
    }
  }

  val completed = Iterator.iterate(State())(_.next).drop(2017).next
  val part1 = completed.elementAfterPos
  val part2 = Iterator.iterate(SimplifiedState())(_.next).drop(50000000).next.elementAfterZero
}