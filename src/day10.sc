object day10 {

  def rotateLeft(list: List[Int], by: Int) = list.drop(by) ::: list.take(by)

  case class State(list: List[Int] = (0 until 256).toList, position: Int = 0, skipSize: Int = 0) {
    def tieKnot(length: Int): State = {
      val rotated = rotateLeft(list, position)
      val reversed = rotated.take(length).reverse ::: rotated.drop(length)
      val knotted = rotateLeft(reversed, list.length - position)
      State(knotted, (position + length + skipSize) % list.length, (skipSize + 1) % list.length)
    }

    def denseHash = list.grouped(16)
      .map(_.reduce(_ ^ _))
      .map(x => f"$x%02x")
      .mkString
  }

  def tieKnots(lengths: Seq[Int]): State = lengths.foldLeft(State())(_ tieKnot _)

  def hash(input: String): String = {
    val lengths = input.toList.map(_.toInt) ::: List(17, 31, 73, 47, 23)
    tieKnots(List.fill(64)(lengths).flatten).denseHash
  }

  val input = "18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188"
  val inputLengths = input.split(",").map(_.toInt)
  val part1 = tieKnots(inputLengths).list.take(2).product
  val part2 = hash(input)

  hash("Quite some text that makes hashing it take a little bit longer, but shouldn't make much of a difference given the speed so far")
}