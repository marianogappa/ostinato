abstract class Game(board: Board, players: List[Player], rules: Rules)

object Board {
  def fromXY(xSize: Int)(x: Int, y: Int) = y * xSize + x
  def toX(xSize: Int)(pos: Int) = pos % xSize
  def toY(xSize: Int)(pos: Int) = pos / xSize
  def distance(from: Int, to: Int) = math.abs(from - to)
  def sign(from: Int, to: Int) = math.signum(to - from)
}

abstract class Board(val grid: Vector[Option[Piece]], xSize: Int) {
  type Cell = Option[Piece]
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

  def movement(fromX: Int, fromY: Int, dx: Int, dy: Int)(implicit rules: Rules): Option[Movement]

  def move(m: Movement)(implicit rules: Rules): Board
}

abstract class Piece(val x: Int, val y: Int, val owner: Player) {
  def movements(board: Board)(implicit rules: Rules): Set[Movement]

  protected def allMovementsOfDelta(fromX: Int, fromY: Int, dx: Int, dy: Int, board: Board, inc: Int = 1)(implicit rules: Rules): Set[Movement] = {
    board.movement(fromX, fromY, dx * inc, dy * inc) map (m ⇒ Set(m) ++ allMovementsOfDelta(fromX, fromY, dx, dy, board, inc + 1)) getOrElse Set()
  }

  protected def movementOfDelta(fromX: Int, fromY: Int, dx: Int, dy: Int, board: Board)(implicit rules: Rules): Option[Movement] = {
    board.movement(fromX, fromY, dx, dy)
  }
}

case class Movement(fromPiece: Piece, dx: Int, dy: Int)

class Player(val name: String) {
  def pieces(board: Board) = board.grid.flatten.filter(_.owner == this)
  def movements(board: Board)(implicit rules: Rules): Set[Movement] = pieces(board).flatMap(_.movements(board)).toSet
}

class Rules {}

