abstract class Game[B <: Board[_,_,_], P <: Player[B,_]](board: B, players: List[P], rules: Rules)

object Board {
  def fromXY(xSize: Int)(x: Int, y: Int) = y * xSize + x
  def toX(xSize: Int)(pos: Int) = pos % xSize
  def toY(xSize: Int)(pos: Int) = pos / xSize
  def distance(from: Int, to: Int) = math.abs(from - to)
  def sign(from: Int, to: Int) = math.signum(to - from)
}

abstract class Board[P <: Piece[_,_,_], M <: Movement[P], B <: Board[P,M,_]](val grid: Vector[Option[P]], xSize: Int) {
  type Cell = Option[P]
  type Location = Option[Cell]

  def fromXY = Board.fromXY(xSize) _
  def exists(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < xSize && fromXY(x, y) < grid.size
  def get(x: Int, y: Int): Location = if (exists(x, y)) Some(grid(fromXY(x, y))) else None
  def isPiece(l: Location): Boolean = l.flatten.nonEmpty
  def isEmptyCell(l: Location): Boolean = l.nonEmpty && l.flatten.isEmpty

  protected def between(xFrom: Int, yFrom: Int, xTo: Int, yTo: Int): Set[Location] = {
    val distanceX = Board.distance(xFrom, xTo)
    val distanceY = Board.distance(yFrom, yTo)
    val deltaX = Board.sign(xFrom, xTo)
    val deltaY = Board.sign(yFrom, yTo)

    if ((deltaX != 0 && deltaY != 0 && distanceX == distanceY && distanceX >= 2) ||
        (deltaX == 0 && deltaY != 0 && distanceY >= 2) ||
        (deltaX != 0 && deltaY == 0 && distanceX >= 2))
      betweenInclusive(xFrom + deltaX, yFrom + deltaY, xTo - deltaX, yTo - deltaY, deltaX, deltaY)
    else
      Set()
  }

  private def betweenInclusive(xFrom: Int, yFrom: Int, xTo: Int, yTo: Int, deltaX: Int, deltaY: Int): Set[Location] = {
    if ((xFrom, yFrom) == (xTo, yTo))
      Set(get(xFrom, yFrom))
    else
      Set(get(xFrom, yFrom)) ++ betweenInclusive(xFrom + deltaX, yFrom + deltaY, xTo, yTo, deltaX, deltaY)
  }

  def movement(fromX: Int, fromY: Int, dx: Int, dy: Int)(implicit rules: Rules): Option[M]

  def move(m: M)(implicit rules: Rules): B
}

abstract class Piece[P <: Player[B,M], M <: Movement[_], B <: Board[_, M, B]](val x: Int, val y: Int, val owner: P) {
  def movements(board: B)(implicit rules: Rules): Set[M]

  protected def allMovementsOfDelta(fromX: Int, fromY: Int, dx: Int, dy: Int, board: B, inc: Int = 1)(implicit rules: Rules): Set[M] = {
    Set.empty[M]
    val a: Option[M] = board.movement(fromX, fromY, dx * inc, dy * inc)
    val b: Option[Set[M]] = a map { m: M ⇒ Set(m) ++ allMovementsOfDelta(fromX, fromY, dx, dy, board, inc + 1) }
    val c: Set[M] = b getOrElse Set.empty[M]
    c
  }

  protected def movementOfDelta(fromX: Int, fromY: Int, dx: Int, dy: Int, board: B)(implicit rules: Rules): Option[M] = {
    board.movement(fromX, fromY, dx, dy)
  }
}

case class Movement[P <: Piece[_,_,_]](fromPiece: P, dx: Int, dy: Int)

class Player[B <: Board[_, _, _], M <: Movement[_]](val name: String) {
  
//  def pieces(board: B): Set[Piece[_,_,_]] = board.grid.flatten.filter(_.owner == this).toSet
//  def movements(board: B)(implicit rules: Rules): Set[M] = {
//    val movements = for {
//      piece: Piece[_,_,B] <- pieces(board).toSet: Set[Piece[_,_,B]]
//      movements <- piece.movements(board): Set[M]
//    } yield movements

//    val p = pieces(board)
//    p.flatMap { case piece: Piece[_,M,B] => piece.movements(board) }
//    movements
//  }
}

class Rules {}

