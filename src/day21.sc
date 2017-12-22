object day21 {

  case class Coordinates(x: Int, y: Int) {
    def rotate(n: Int) = Coordinates(n - 1 - y, x)

    def flip(n: Int) = Coordinates(n - 1 - x, y)

    def gridCoordinates(n: Int) = Coordinates(x / n, y / n)

    def toLocalCoordinates(gridCoordinates: Coordinates, n: Int) = Coordinates(x - gridCoordinates.x * n, y - gridCoordinates.y * n)

    def toGlobalCoordinates(gridCoordinates: Coordinates, n: Int) = Coordinates(x + gridCoordinates.x * n, y + gridCoordinates.y * n)
  }

  case class Grid(on: Set[Coordinates], n: Int) {
    def map(f: Coordinates => Coordinates) = Grid(on.map(f), n)

    def rotate: Grid = map(_.rotate(n))

    def flip: Grid = map(_.flip(n))

    def charAt(c: Coordinates): Char = if (on.contains(c)) '#' else '.'

    override def toString: String = (for (y <- 0 until n) yield
      (for (x <- 0 until n) yield charAt(Coordinates(x, y))).mkString).mkString("/")
  }

  def toMap(str: String) = str.split("/")
    .zipWithIndex
    .flatMap({ case (line, y) =>
      line.zipWithIndex.filter(_._1 == '#').map({ case (_, x) => Coordinates(x, y) })
    }).toSet

  def toGrid(str: String) = Grid(toMap(str), str.split("/").length)

  val rules = common.loadPackets(List("day21.txt"))
    .map(_.split(" => ").toList)
    .map({ case k :: v :: Nil => (toGrid(k), toGrid(v)) })
    .flatMap({ case (k, v) => Set((k, v), (k.flip, v)) })
    .flatMap({ case (k, v) => Set((k, v), (k.rotate, v), (k.rotate.rotate, v), (k.rotate.rotate.rotate, v)) })
    .toMap

  def next(grid: Grid): Grid = {
    val n = if (grid.n % 2 == 0) 2 else 3
    val perGroup = grid.on.groupBy(_.gridCoordinates(n))
    val gridCoords = for (y <- 0 until grid.n / n;
                          x <- 0 until grid.n / n) yield Coordinates(x, y)
    val on = gridCoords.map(c => (c, Grid(perGroup.getOrElse(c, Set())
      .map(_.toLocalCoordinates(c, n)), n)))
      .toMap
      .mapValues(rules)
      .flatMap({ case (c, g) => g.on.map(_.toGlobalCoordinates(c, n + 1)) })
      .toSet
    Grid(on, grid.n * (n + 1) / n)
  }

  val start = toGrid(".#./..#/###")
  val part1 = Stream.iterate(start)(next).drop(5).head.on.size
  val part2 = Stream.iterate(start)(next).drop(18).head.on.size
}