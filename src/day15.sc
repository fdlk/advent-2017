object day15 {
  def next(factor: Int)(value: Long) = value * factor % 2147483647

  def generator(start: Long, factor: Int) = Iterator.iterate(start)(next(factor)).drop(1)

  def a = generator(703L, 16807)

  def b = generator(516L, 48271)

  def judge(pair: (Long, Long)): Boolean = pair match {
    case (a, b) => a % 65536 == b % 65536
  }

  val part1 = a.zip(b).take(40000000).count(judge)
  val part2 = a.filter(_ % 4 == 0).zip(b.filter(_ % 8 == 0)).take(10000000).count(judge)
}