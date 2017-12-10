import common.loadPackets

type Registers = Map[String, Long]

case class Processor(registers: Registers = Map(), max: Long = 0) {
  def lookup(register: String) = registers.getOrElse(register, 0)

  def process(instruction: Instruction): Processor = {
    if (instruction.shouldOperate(lookup(instruction.checkReg))) {
      val newValue = instruction.operate(lookup(instruction.register))
      Processor(registers.updated(instruction.register, newValue),
        Math.max(max, newValue))
    } else this
  }
}

case class Instruction(register: String,
                       operator: String,
                       operand: Int,
                       checkReg: String,
                       checkOperator: String,
                       checkOperand: Int) {
  def shouldOperate(checkValue: Long): Boolean = checkOperator match {
    case "<=" => checkValue <= checkOperand
    case "<" => checkValue < checkOperand
    case ">=" => checkValue >= checkOperand
    case "==" => checkValue == checkOperand
    case "!=" => checkValue != checkOperand
    case ">" => checkValue > checkOperand
  }

  def operate(oldValue: Long): Long = operator match {
    case "inc" => oldValue + operand
    case "dec" => oldValue - operand
  }
}

val input = loadPackets(List("day8.txt"))
val regex = """(\w+) (inc|dec) (-?\d+) if (\w+) ([\!<=>]+) (-?\d+)""".r
val instructions = input.map({
  case regex(register, operator, operand, checkReg, checkOp, checkOperand) =>
    Instruction(register, operator, operand.toInt, checkReg, checkOp, checkOperand.toInt)
})

val finalState = instructions.foldLeft(Processor())(_ process _)
val part1 = finalState.registers.values.max
val part2 = finalState.max