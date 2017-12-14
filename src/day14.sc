import common.knotHash

import scala.annotation.tailrec

object day14 {

  case class Sector(x: Int, y: Int) {
    lazy val neighbors: Set[Sector] = Set(
      copy(x - 1, y),
      copy(x, y - 1),
      copy(x + 1, y),
      copy(x, y + 1)
    )
  }

  def toSectors(row: String, y: Int): Set[Sector] = row.zipWithIndex.map({
    case (col, x) if col == '1' => Set(Sector(x, y))
    case _ => Set.empty
  }).toSet.flatten

  val input = "amgozmfv"
  val sectors: Set[Sector] = (0 to 127)
    .map(x => f"$input-$x")
    .map(knotHash)
    .map(BigInt(_, 16).toString(2))
    .map(s => Stream.fill(128 - s.length)('0').mkString + s)
    .zipWithIndex
    .map({ case (row, y) => toSectors(row, y) })
    .toSet.flatten

  val part1 = sectors.size

  def floodFill(group: Set[Sector]): Set[Sector] = {
    val updated = group ++ group.flatMap(_.neighbors).intersect(sectors)
    if (updated == group) group
    else floodFill(updated)
  }

  @tailrec
  def numGroups(unchecked: Set[Sector], soFar: Int = 0): Int = {
    if (unchecked.isEmpty) soFar
    else numGroups(unchecked -- floodFill(Set(unchecked.head)), soFar + 1)
  }

  val part2 = numGroups(sectors)
}