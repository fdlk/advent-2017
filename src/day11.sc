import common.loadPackets

object day11 {
  val input = loadPackets(List("day11.txt")).head.split(",").toList

  // http://keekerdc.com/2011/03/hexagon-grids-coordinate-systems-and-distance-calculations/
  case class Coordinates(x: Int = 0, y: Int = 0, z: Int = 0) {
    def move(direction: String) = direction match {
      case "se" => copy(x = x + 1, y = y - 1)
      case "ne" => copy(x = x + 1, z = z - 1)
      case "n" => copy(y = y + 1, z = z - 1)
      case "nw" => copy(x = x - 1, y = y + 1)
      case "sw" => copy(x = x - 1, z = z + 1)
      case "s" => copy(y = y - 1, z = z + 1)
    }
    def dist = List(x,y,z).map(_.abs).max
  }

  val part1 = input.foldLeft(Coordinates())(_ move _).dist
  val part2 = input.scanLeft(Coordinates())(_ move _).map(_.dist).max
}