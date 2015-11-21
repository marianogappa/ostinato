package boardgame.core

abstract class Game[B <: Board[_,_,_,_], P <: Player[B,_,_, _]](board: B, players: List[P], rules: Rules)

abstract class Board[P <: Piece[_,_,_,_], M <: Movement[P], B <: Board[P,M,_,_], R <: Rules](val grid: Vector[Option[P]]) {
  type Cell = Option[P]
  type Location = Option[Cell]

  def get(pos: XY)(implicit boardSize: BoardSize): Location = if (pos.exists) Some(grid(pos.toI)) else None
  def isPiece(l: Location): Boolean = l.flatten.nonEmpty
  def isEmptyCell(l: Location): Boolean = l.nonEmpty && l.flatten.isEmpty
  def pieces = grid.flatten

  protected def between(from: XY, to: XY)(implicit boardSize: BoardSize): Set[Location] = xyBetween(from, to) map get

  protected def xyBetween(from: XY, to: XY)(implicit boardSize: BoardSize): Set[XY] = {
    val distance = from.distance(to)
    val delta = from.sign(to)

    if ((delta.x != 0 && delta.y != 0 && distance.x == distance.y && distance.x >= 2) ||
      (delta.x == 0 && delta.y != 0 && distance.y >= 2) ||
      (delta.x != 0 && delta.y == 0 && distance.x >= 2))
      betweenInclusive(from + delta, to - delta, delta)
    else
      Set()
  }

  private def betweenInclusive(from: XY, to: XY, delta: XY)(implicit boardSize: BoardSize): Set[XY] =
    Set(from) ++ (if (from == to) Set() else betweenInclusive(from + delta, to, delta))

  def movement(from: XY, delta: XY)(implicit rules: R): Option[M]

  def move(m: M)(implicit rules: R): B
}

object Piece {
  def toXYs(points: Set[(Int, Int)]): Set[XY] = points map (p => XY(p._1, p._2))
}

abstract class Piece[P <: Player[B,M, _, _], M <: Movement[_], B <: Board[_,M,B,R], R <: Rules](val pos: XY, val owner: P) {
  def movements(board: B)(implicit rules: R): Set[M]

  protected def allMovementsOfDelta(from: XY, delta: XY, board: B, inc: Int = 1)(implicit rules: R): Set[M] = {
    Set.empty[M]
    val a: Option[M] = board.movement(from, delta * inc)
    val b: Option[Set[M]] = a map { m: M ⇒ Set(m) ++ allMovementsOfDelta(from, delta, board, inc + 1) }
    val c: Set[M] = b getOrElse Set.empty[M]
    c
  }

  protected def movementOfDelta(from: XY, delta: XY, board: B)(implicit rules: R): Option[M] = {
    board.movement(from, delta)
  }
}

class Movement[P <: Piece[_,_,_,_]](fromPiece: P, delta: XY)

// TODO either implement movements or remove the M type parameter
abstract class Player[B <: Board[P,_,_,_], M <: Movement[_], P <: Piece[PL,_,_,_], PL <: Player[_,_,_,_]](val name: String) {
  def equals(that: PL): Boolean = { that.name == name }

  def pieces(board: B): Set[P] = {
    board.pieces.filter { a: P => a.owner.equals(this) }.toSet
  }
}

class Rules {} // TODO this should be abstract... and thought about

object XY {
  def fromI(i: Int)(implicit boardSize: BoardSize) = {
    XY(i % boardSize.x, i / boardSize.x)
  }
}

case class XY(x: Int, y: Int) {
  def toI(implicit boardSize: BoardSize): Int = y * boardSize.x + x
  def +(that: XY) = XY(x + that.x, y + that.y)
  def -(that: XY) = XY(x - that.x, y - that.y)
  def *(factor: Int) = XY(x * factor, y * factor)
  def exists(implicit boardSize: BoardSize) = x >= 0 && y >= 0 && x < boardSize.x && y < boardSize.y
  lazy val sign = XY(math.signum(x), math.signum(y))
  lazy val abs = XY(math.abs(x), math.abs(y))
  def distance(that: XY) = (this - that).abs
  def sign(that: XY): XY = (that - this).sign
}

case class BoardSize(x: Int, y: Int) // N.B. implementation defines an implicit BoardSize -> wouldn't make this a Point
