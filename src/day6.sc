import java.lang.Math.floorMod

object day6 {
  val rawInput = "2\t8\t8\t5\t4\t2\t3\t1\t5\t5\t1\t2\t15\t13\t5\t14".split("\\s")
  type Configuration = List[Int]
  val input: Configuration = rawInput.map(_.toInt).toList

  case class State(conf: Configuration, visited: List[Configuration] = List()) {
    def done: Boolean = visited.contains(conf)
    def redis: State = {
      val numRedis = conf.max
      val whatAllGet = numRedis / conf.size
      val leftover = numRedis % conf.size
      val source = conf.indexOf(numRedis)
      val nextConf: Configuration = conf.zipWithIndex.map({
        case (current, index) =>
          val keep = if (index == source) 0 else current
          val distanceFromSource = floorMod(index - (source + 1), conf.size)
          val bonus = if (distanceFromSource < leftover) 1 else 0
          keep + whatAllGet + bonus
      })
      State(nextConf, conf :: visited)
    }
  }

  val finalState = Stream.iterate(State(input))(_.redis).find(_.done).get
  val part1 = finalState.visited.length
  val part2 = finalState.visited.indexOf(finalState.conf) + 1
}