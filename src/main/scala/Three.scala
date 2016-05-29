import scalaz.State

class Three {
  type Position = (Int, Int)
  type Grid     = Set[Position]

  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object West  extends Direction
  case object East  extends Direction

  def visit(direction: Direction, visited: Grid): State[Position, Grid] =
    State[Position, Grid] {
      (current: Position) => {
        val newPosition = direction match {
          case North => current.copy(_2 = current._2 + 1)
          case South => current.copy(_2 = current._2 - 1)
          case East  => current.copy(_1 = current._1 + 1)
          case West  => current.copy(_1 = current._1 - 1)
        }

        (newPosition, visited + newPosition)
      }
    }

  def visitNoState(visited: Grid, current: Position)(direction: Direction): (Position, Grid) = {
    val newPosition = direction match {
      case North => current.copy(_2 = current._2 + 1)
      case South => current.copy(_2 = current._2 - 1)
      case East  => current.copy(_1 = current._1 + 1)
      case West  => current.copy(_1 = current._1 - 1)
    }

    (newPosition, visited + newPosition)
  }

  def simulateNoState(directions: Seq[Direction]): (Position, Grid) = {
    val initial = (0,0)
    val visited = Set(initial)
    val initialState = (initial, visited)

    directions.foldLeft(initialState) {
      (acc, d) => {
        val (pos, set) = acc
        visitNoState(set, pos)(d)
      }
    }
  }

  def simulate(directions: Seq[Direction]): (Position, Grid) = {
    val unit = State[Position, Grid] (initialPosition => (initialPosition, Set(initialPosition)))

    val visitAll: State[Position, Grid] = directions.foldLeft(unit) {
      (acc, dir) => acc.flatMap(grid => visit(dir, grid))
    }

    val initialPosition = (0,0) // always starting at (0,0)
    visitAll(initialPosition)
  }

  def mapDirection(c: Char): Direction = c match {
    case '^' => North
    case 'v' => South
    case '>' => East
    case '<' => West
    case _ => throw new Exception("Unexpected input")
  }

  def santa(directions: Seq[Direction]) =
    simulate(directions)._2.size


  // Exercise 2

  def santaWithRobotSanta(input: Seq[Direction]) = {
    // split every other between santa and robot
    val d = input.grouped(2)
      .map { case Seq(a, b) => (a, b) }
      .foldLeft((List[Direction](), List[Direction]())) {
        (acc, n) => (n._1 :: acc._1, n._2 :: acc._2)
      }

    val santaDirections = d._1.reverse
    val robotDirections = d._2.reverse

    val santaVisitedState = simulateNoState(santaDirections)
    val robotVisitedState = simulateNoState(robotDirections)

    val totalNumberHousesVisited = (santaVisitedState._2 ++ robotVisitedState._2).size

    totalNumberHousesVisited
  }
}
