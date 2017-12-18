import scala.io.Source.fromInputStream
import scala.util.parsing.combinator.JavaTokenParsers

object day18 {

  case class Regs(a: Long=0, b: Long=0, f: Long=0, i: Long=0, p: Long=0, s: Long=0, ip: Int=0) {
    def update(ab: Char, fun: Long => Long): Regs = ab match {
      case 'a' => copy(a = fun(a)).jmp(1)
      case 'b' => copy(b = fun(b)).jmp(1)
      case 'f' => copy(f = fun(f)).jmp(1)
      case 'i' => copy(i = fun(i)).jmp(1)
      case 'p' => copy(p = fun(p)).jmp(1)
      case 's' => copy(s = fun(s)).jmp(1)
    }

    def cpy(to: Char, from: String): Regs = update(to, (_) => read(from))

    def jmp(offset: Int): Regs = copy(ip = ip + offset)

    def read(arg: String): Long = arg match {
      case "a" => a
      case "b" => b
      case "f" => f
      case "i" => i
      case "p" => p
      case "s" => s
      case _ => arg.toLong
    }
  }

  case class State(regs: Regs, recovered: Option[Long])
  type Instruction = Regs => State

  class InstructionParser extends JavaTokenParsers {
    def register: Parser[Char] = ("a" | "b" | "f" | "i" | "p") ^^ {_.charAt(0)}

    def offset: Parser[Int] = opt("+") ~> wholeNumber ^^ {_.toInt}

    def operand: Parser[String] = ("a" | "b" | "f" | "i" | "p" | offset) ^^ {_.toString}

    def set: Parser[Instruction] = "set" ~> register ~ operand ^^ { case arg ~ value => r: Regs => State(r.update(arg, (x) => r.read(value)), None) }

    def snd: Parser[Instruction] = "snd" ~> operand ^^ { value => r: Regs => State(r.update('s', (x) => r.read(value)), None) }

    def jgz: Parser[Instruction] = "jgz" ~> operand ~ operand ^^ { case arg ~ value => r: Regs => State(if (r.read(arg) > 0) r.jmp(r.read(value).toInt) else r.jmp(1), None)}

    def rcv: Parser[Instruction] = "rcv" ~> operand ^^ { arg => r: Regs => State(r.jmp(1), if (r.read(arg) > 0) Some(r.read("s")) else None) }

    def add: Parser[Instruction] = "add" ~> register ~ operand ^^ { case arg ~ value => r: Regs => State(r.update(arg, _ + r.read(value)), None) }

    def mul: Parser[Instruction] = "mul" ~> register ~ operand ^^ { case arg ~ value => r: Regs => State(r.update(arg, _ * r.read(value)), None) }

    def mod: Parser[Instruction] = "mod" ~> register ~ operand ^^ { case arg ~ value => r: Regs => State(r.update(arg, _ % r.read(value)), None) }

    def instruction: Parser[Instruction] = set | snd | add | mul | mod | rcv | jgz
  }

  object InstructionParser extends InstructionParser

  val input: List[String] = common.loadPackets(List("day18.txt"))

  val program: List[Instruction] = input.map(line => InstructionParser.parseAll(InstructionParser.instruction, line).get)

  def step(state: State): State = program(state.regs.ip)(state.regs)

  Stream.iterate(State(Regs(), None))(step).find(_.recovered.isDefined).get.recovered.get

}