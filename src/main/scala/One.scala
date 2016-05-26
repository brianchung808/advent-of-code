class One {
  /*
    ( = go up one floor
    ) = go down one floor
   */

  type Level = Int
  type Index = Int

  def determineFloor(l: List[Char]): Level =
    l.map(c => if (c == '(') 1 else -1).sum

  def findBasementCausingIndex(l: List[Char], t: (Index, Level)): Index = t match {
    case (a, -1) => a
    case _ => l match {
      case Nil => -1
      case x::xs if x == '(' => findBasementCausingIndex(xs, (t._1 + 1, t._2 + 1))
      case x::xs if x == ')' => findBasementCausingIndex(xs, (t._1 + 1, t._2 - 1))
    }
  }
}
