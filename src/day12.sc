import common.loadPackets

import scala.annotation.tailrec

object day12 {
  val input = loadPackets(List("day12.txt"))
  val connected: Map[String, Set[String]] = input
    .map(line => line.split(" <-> ").toList)
    .map({ case List(from, to) => from -> to.split(", ").toSet })
    .toMap

  @tailrec
  def reachable(from: Set[String]): Set[String] = {
    val updated = from ++ from.flatMap(connected)
    if (updated == from) from
    else reachable(updated)
  }

  val part1 = reachable(Set("0")).size

  @tailrec
  def groups(keys: Set[String], soFar: Int = 0): Int =
    if (keys.isEmpty) soFar
    else groups(keys -- reachable(Set(keys.head)), soFar + 1)

  val part2 = groups(connected.keys.toSet)
}