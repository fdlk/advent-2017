object day10 {

  case class Knot(modulo: Int, from: Int, length: Int) {
    // positive mod, between 0 and modulo for x >= -modulo
    def mod(x: Int): Int = (modulo + x) % modulo
    val to = mod(from + length - 1)
    def indexInOriginalList(i: Int): Int = {
      if (mod(i - from) >= length) i
      else mod(from + to - i)
    }
  }

  case class State(list: Seq[Int] = 0 until 256, position: Int = 0, skipSize: Int = 0) {
    def tieKnot(length: Int): State = {
      val knot = Knot(list.length, position, length)
      State(
        list.indices.map(knot.indexInOriginalList).map(list),
        (position + length + skipSize) % list.length,
        (skipSize + 1) % list.length
      )
    }

    def denseHash = list.grouped(16)
      .map(_.reduce(_ ^ _))
      .map(x => f"$x%02x")
      .mkString
  }

  def tieKnots(lengths: List[Int]): State = lengths.foldLeft(State())(_ tieKnot _)

  def hash(input: String): String = {
    val lengths = input.toList.map(_.toInt) ::: List(17, 31, 73, 47, 23)
    tieKnots(List.fill(64)(lengths).flatten).denseHash
  }

  val input = "18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188"
  val inputLengths = input.split(",").toList.map(_.toInt)
  val part1 = tieKnots(inputLengths).list.take(2).product
  val part2 = hash(input)
}