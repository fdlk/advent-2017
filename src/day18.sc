import scala.util.parsing.combinator.JavaTokenParsers

object day18 {

  case class Regs(a: Long = 0, b: Long = 0, f: Long = 0, i: Long = 0, p: Long = 0, ip: Int = 0,
                  received: List[Long] = Nil, sent: List[Long] = Nil, waiting: Boolean = false) {
    def update(ab: Char, fun: Long => Long): Regs = ab match {
      case 'a' => copy(a = fun(a)).jmp(1)
      case 'b' => copy(b = fun(b)).jmp(1)
      case 'f' => copy(f = fun(f)).jmp(1)
      case 'i' => copy(i = fun(i)).jmp(1)
      case 'p' => copy(p = fun(p)).jmp(1)
    }

    def cpy(to: Char, from: String): Regs = update(to, (_) => read(from))

    def jmp(offset: Int): Regs = copy(ip = ip + offset)

    def read(arg: String): Long = arg match {
      case "a" => a
      case "b" => b
      case "f" => f
      case "i" => i
      case "p" => p
      case _ => arg.toLong
    }

    def set(arg: Char, value: String): Regs = update(arg, (x) => read(value))

    def jgz(arg: String, value: String): Regs = if (read(arg) > 0) jmp(read(value).toInt) else jmp(1)

    def add(arg: Char, value: String): Regs = update(arg, _ + read(value))

    def mul(arg: Char, value: String): Regs = update(arg, _ * read(value))

    def mod(arg: Char, value: String): Regs = update(arg, _ % read(value))

    def rcv(arg: Char): Regs = received match {
      case r :: rs => copy(received = rs).set(arg, r.toString)
      case Nil => copy(waiting = true)
    }

    def snd(arg: String): Regs = copy(sent = read(arg) :: sent).jmp(1)
  }

  type Instruction = Regs => Regs

  class InstructionParser extends JavaTokenParsers {
    def register: Parser[Char] = ("a" | "b" | "f" | "i" | "p") ^^ {_.charAt(0)}

    def offset: Parser[Int] = opt("+") ~> wholeNumber ^^ {_.toInt}

    def operand: Parser[String] = ("a" | "b" | "f" | "i" | "p" | offset) ^^ {_.toString}

    def parseSnd: Parser[Instruction] = "snd" ~> operand ^^ { value => state => state.snd(value) }

    def parseRcv: Parser[Instruction] = "rcv" ~> register ^^ { arg => state => state.rcv(arg) }

    def parseSet: Parser[Instruction] = "set" ~> register ~ operand ^^ { case arg ~ value => state => state.set(arg, value) }

    def parseJgz: Parser[Instruction] = "jgz" ~> operand ~ operand ^^ { case arg ~ value => state => state.jgz(arg, value) }

    def parseAdd: Parser[Instruction] = "add" ~> register ~ operand ^^ { case arg ~ value => state => state.add(arg, value) }

    def parseMul: Parser[Instruction] = "mul" ~> register ~ operand ^^ { case arg ~ value => state => state.mul(arg, value) }

    def parseMod: Parser[Instruction] = "mod" ~> register ~ operand ^^ { case arg ~ value => state => state.mod(arg, value) }

    def instruction: Parser[Instruction] = parseSet | parseSnd | parseAdd | parseMul | parseMod | parseRcv | parseJgz
  }

  object InstructionParser extends InstructionParser

  val program: List[Instruction] = common.loadPackets(List("day18.txt"))
    .map(InstructionParser.parseAll(InstructionParser.instruction, _).get)

  def step(state: Regs): Regs = program(state.ip)(state)

  val part1 = Stream.iterate(Regs())(step).find(_.waiting).get.sent.head

  case class State(p0: Regs = Regs(), p1: Regs = Regs(p = 1), numSentP1: Int = 0) {
    def deadlocked: Boolean = p0.waiting && p0.sent.isEmpty && p1.waiting && p1.sent.isEmpty

    def roll: State = copy(
      p0 = Stream.iterate(p0)(step).find(_.waiting).get,
      p1 = Stream.iterate(p1)(step).find(_.waiting).get
    )

    def exchange: State = copy(
      p0 = p0.copy(received = p1.sent.reverse, sent = Nil, waiting = false),
      p1 = p1.copy(received = p0.sent.reverse, sent = Nil, waiting = false),
      numSentP1 = numSentP1 + p1.sent.length
    )
  }

  val part2 = Iterator.iterate(State())(_.exchange.roll).find(_.deadlocked).get.numSentP1
}