import common.loadPackets

type Registers = (Map[String, Long], Long)
case class Instruction(register: String,
                       operator: String,
                       operand: Int,
                       checkReg: String,
                       checkOperator: String,
                       checkOperand: Int)
{
  def apply(state: Registers): Registers = state match {
    case (registers:Map[String, Long], max: Long) =>
    val value: Long = registers.getOrElse(register, 0)
    val checkValue: Long = registers.getOrElse(checkReg, 0)
    val shouldOperate: Boolean = checkOperator match {
      case "<=" => checkValue <= checkOperand
      case "<" => checkValue < checkOperand
      case ">=" => checkValue >= checkOperand
      case "==" => checkValue == checkOperand
      case "!=" => checkValue != checkOperand
      case ">" => checkValue > checkOperand
    }
    if( shouldOperate ) {
      val multiplier = if( operator == "inc") 1 else -1
      val newValue = value + operand * multiplier
      val newRegisters = registers.updated(register, newValue)
      (newRegisters, Math.max(max, newValue))
    } else state
  }
}

val input = loadPackets(List("day8.txt"))
val regex = """(\w+) (inc|dec) (-?\d+) if (\w+) ([\!<=>]+) (-?\d+)""".r
val instructions = input.map({
  case regex(register, operator, operand, checkReg, checkOp, checkOperand) =>
    Instruction(register, operator, operand.toInt, checkReg, checkOp, checkOperand.toInt)
})

val initialState: Registers = (Map(), 0)
val finalState = instructions.foldLeft(initialState)((state, instruction) => instruction.apply(state))
val part1 = finalState._1.values.max
val part2 = finalState._2