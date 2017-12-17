object day17 {
  val step: Int = 344

  def ceilDiv(divident: Int, divisor: Int): Int = Math.ceil(divident.asInstanceOf[Double] / divisor).toInt

  case class State(buffer: List[Int] = List(0), pos: Int = 0) {
    def next: State = {
      val newPos = (pos + step) % buffer.length + 1
      copy(buffer.patch(newPos, List(buffer.length), 0), newPos)
    }
    def valueAfterPos = buffer(pos + 1)
    def simplify = SimplifiedState(buffer(1), buffer.length, pos)
  }

  case class SimplifiedState(elementAfterZero: Int, n: Int, pos: Int) {
    def next: SimplifiedState = {
      val i = ceilDiv(n - pos, step)
      val newN = n + i
      val newPos = (pos + i * (step + 1)) % (newN - 1)
      SimplifiedState(if (newPos == 1) newN - 1 else elementAfterZero, newN, newPos)
    }
  }

  val completed = Stream.iterate(State())(_.next)(2017)
  val part1 = completed.valueAfterPos
  val part2 = Stream.iterate(completed.simplify)(_.next).find(_.n >= 50000000).get.elementAfterZero
}