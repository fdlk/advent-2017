object day25 {
  type Value = Int
  type Position = Int
  type Tape = Set[Position]
  type State = Char

  case class Action(write: Value, increment: Int, nextState: State)

  val right = 1
  val left = -1
  val rules: Map[(Char, Value), Action] = Map(
    ('A', 0) -> Action(1, right, 'B'),
    ('A', 1) -> Action(0, right, 'F'),
    ('B', 0) -> Action(0, left, 'C'),
    ('B', 1) -> Action(1, left, 'C'),
    ('C', 0) -> Action(1, left, 'D'),
    ('C', 1) -> Action(0, right, 'C'),
    ('D', 0) -> Action(1, left, 'E'),
    ('D', 1) -> Action(1, right, 'A'),
    ('E', 0) -> Action(1, left, 'F'),
    ('E', 1) -> Action(0, left, 'D'),
    ('F', 0) -> Action(1, right, 'A'),
    ('F', 1) -> Action(0, left, 'E'))

  case class TuringMachine(tape: Tape = Set(),
                           state: Char = 'A',
                           cursor: Position = 0) {
    lazy val action: Action = rules((state, value))
    lazy val value: Value = if (tape.contains(cursor)) 1 else 0

    def next: TuringMachine = TuringMachine(
      if (action.write == 1)
        tape + cursor
      else
        tape - cursor,
      action.nextState,
      cursor + action.increment)

    def diagnosticChecksum = tape.size
  }

  val part1 = Iterator.iterate(TuringMachine())(_.next).drop(12425180).next.diagnosticChecksum
}