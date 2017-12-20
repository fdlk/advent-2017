object day20 {
  val lineRegex = """p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>""".r
  val input = common.loadPackets(List("day20.txt"))

  type Particles = List[Particle]
  case class V3(x: Long, y: Long, z: Long){
    def add(other: V3) = V3(x + other.x, y + other.y, z + other.z)
    def manhattan: Long = x.abs + y.abs + z.abs
  }

  case class Particle(p: V3, v: V3, a: V3) {
    def tickV: Particle = copy(v = v.add(a))
    def tickP: Particle = copy(p = p.add(v))
    def next: Particle = tickV.tickP
  }

  val particles = input.map(_.trim).map({
    case lineRegex(p0, p1, p2, v0, v1, v2, a0, a1, a2) =>
      Particle(
        V3(p0.toLong, p1.toLong, p2.toLong),
        V3(v0.toLong, v1.toLong, v2.toLong),
        V3(a0.toLong, a1.toLong, a2.toLong)
      )
  })

  val part1 = particles
    .map(particle => (particle.a.manhattan, particle.v.manhattan, particle.p.manhattan))
    .zipWithIndex
    .minBy(_._1)
    ._2

  def next(particles: Particles): Particles = particles
    .map(_.next)
    .groupBy(_.p)
    .values
    .flatMap {
      case p :: Nil => Some(p)
      case _ => None
    }
    .toList

  val simulation: Iterator[Particles] = Iterator.iterate(particles)(next)
  val part2 = simulation.map(_.length).sliding(100).map(_.toSet).find(_.size == 1).get.head
}