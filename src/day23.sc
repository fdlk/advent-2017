import scala.util.parsing.combinator.JavaTokenParsers

object day23 {

  case class Regs(a: Long = 0,
                  b: Long = 0,
                  c: Long = 0,
                  d: Long = 0,
                  e: Long = 0,
                  f: Long = 0,
                  g: Long = 0,
                  h: Long = 0,
                  ip: Int = 0,
                  mulCount: Int = 0) {
    def update(ab: Char, fun: Long => Long): Regs = ab match {
      case 'a' => copy(a = fun(a)).jmp(1)
      case 'b' => copy(b = fun(b)).jmp(1)
      case 'c' => copy(c = fun(c)).jmp(1)
      case 'd' => copy(d = fun(d)).jmp(1)
      case 'e' => copy(e = fun(e)).jmp(1)
      case 'f' => copy(f = fun(f)).jmp(1)
      case 'g' => copy(g = fun(g)).jmp(1)
      case 'h' => copy(h = fun(h)).jmp(1)
    }

    def read(arg: String): Long = arg match {
      case "a" => a
      case "b" => b
      case "c" => c
      case "d" => d
      case "e" => e
      case "f" => f
      case "g" => g
      case "h" => h
      case _ => arg.toLong
    }
    def jmp(offset: Int): Regs = copy(ip = ip + offset)
    def set(arg: Char, value: String): Regs = update(arg, _ => read(value))
    def sub(arg: Char, value: String): Regs = update(arg, _ - read(value))
    def mul(arg: Char, value: String): Regs = copy(mulCount = mulCount + 1).update(arg, _ * read(value))
    def jnz(arg: String, value: String): Regs = if (read(arg) != 0) jmp(read(value).toInt) else jmp(1)
  }

  type Instruction = Regs => Regs

  class InstructionParser extends JavaTokenParsers {
    def register: Parser[Char] = ("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h") ^^ {_.charAt(0)}
    def offset: Parser[Int] = opt("+") ~> wholeNumber ^^ {_.toInt}
    def operand: Parser[String] = ("a" | "b" | "c" | "d" | "e"| "f" | "g" | "h" | offset) ^^ {_.toString}
    def parseSet: Parser[Instruction] = "set" ~> register ~ operand ^^ { case arg ~ value => state => state.set(arg, value) }
    def parseSub: Parser[Instruction] = "sub" ~> register ~ operand ^^ { case arg ~ value => state => state.sub(arg, value) }
    def parseMul: Parser[Instruction] = "mul" ~> register ~ operand ^^ { case arg ~ value => state => state.mul(arg, value) }
    def parseJnz: Parser[Instruction] = "jnz" ~> operand ~ operand ^^ { case arg ~ value => state => state.jnz(arg, value) }
    def instruction: Parser[Instruction] = parseSet | parseSub | parseMul | parseJnz
  }

  object InstructionParser extends InstructionParser

  val program: List[Instruction] = common.loadPackets(List("day23.txt"))
    .map(InstructionParser.parseAll(InstructionParser.instruction, _).get)

  case class State(regs: Regs = Regs()){
    def next = State(program(regs.ip)(regs))
    def done = !program.indices.contains(regs.ip)
  }

  Iterator.iterate(State())(_.next).find(_.done).get.regs.h
  val part1 = Iterator.iterate(State())(_.next).find(_.done).get.regs.mulCount

  /* Pseudocode
if(debug) {
  b = 99
  c = 99
} else {
  b = 109900
  c = 126900
}
while(b<>c) {
	f = 1
	for(d = 2; d <> b; d++){
		for(e = 2; e <> b; e++) {
			if d * e == b {
				f = 0 // not a prime
			}
		}
	}
	if f == 0 {
	  h = h + 1
	}
	b = b + 17
}
   */

  def isPrime(n: Int) = (2 until n - 1) forall (n % _ != 0)
  val halted = Iterator.iterate(State(Regs().copy(a=1)))(_.next).find(_.regs.ip==9).get.regs
  val b = halted.b.toInt
  val c = halted.c.toInt
  val part2 = (b to c by 17).count(!isPrime(_))
}