class Three {
  type Position = (Int, Int)
  type State = (Position, Set[Position])

  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object West  extends Direction
  case object East  extends Direction

  def visit(current: Position, visited: Set[Position], direction: Direction): State = {
    val newState = direction match {
      case North => current.copy(_2 = current._2 + 1)
      case South => current.copy(_2 = current._2 - 1)
      case East  => current.copy(_1 = current._1 + 1)
      case West  => current.copy(_1 = current._1 - 1)
    }

    (newState, visited + newState)
  }

  def findNumberVisitedHouses(directions: List[Direction]): Int = {
    val current = (0,0)
    val visited = Set(current)
    val currentState = (current, visited)

    val solution: State = directions.foldLeft(currentState)((acc, dir) => {
      val (pos, set) = acc
      visit(pos, set, dir)
    })

    solution._2.size
  }
}
