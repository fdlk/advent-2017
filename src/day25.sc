object day25 {
  type Value = Int
  type Position = Int
  type Tape = Set[Position]

  case class Action(write: Value, increment: Int, nextState: Char)

  case class State(name: Char, actions: Map[Value, Action])

  case class TuringMachine(tape: Tape = Set(),
                           state: Char = 'A',
                           cursor: Position = 0) {
    lazy val action: Action = states(state).actions(value)
    lazy val value: Value = if (tape.contains(cursor)) 1 else 0

    def next: TuringMachine = TuringMachine(
      if (action.write == 1) tape + cursor else tape - cursor,
      action.nextState,
      cursor + action.increment)

    def diagnosticChecksum: Long = tape.size
  }

  val right = 1
  val left = -1
  val states: Map[Char, State] = List(
    State('A', Map(0 -> Action(1, right, 'B'), 1 -> Action(0, right, 'F'))),
    State('B', Map(0 -> Action(0, left,  'C'), 1 -> Action(1, left,  'C'))),
    State('C', Map(0 -> Action(1, left,  'D'), 1 -> Action(0, right, 'C'))),
    State('D', Map(0 -> Action(1, left,  'E'), 1 -> Action(1, right, 'A'))),
    State('E', Map(0 -> Action(1, left,  'F'), 1 -> Action(0, left,  'D'))),
    State('F', Map(0 -> Action(1, right, 'A'), 1 -> Action(0, left,  'E'))))
    .map(s => s.name -> s)
    .toMap

  val part1 = Iterator.iterate(TuringMachine())(_.next).drop(12425180).next.diagnosticChecksum
}