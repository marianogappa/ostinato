package ostinato.core

import scala.annotation.tailrec
import scala.util.Random

abstract class Game[B <: Board[B, A, PC, PL, R], A <: Action[B, A, PC, PL, R], PC <: Piece[B, A, PC, PL, R], PL <: Player[B, A, PC, PL, R], R <: Rules](
  val board: B, val players: List[PL], val rules: Rules)

abstract class Board[B <: Board[B, A, PC, PL, R], A <: Action[B, A, PC, PL, R], PC <: Piece[B, A, PC, PL, R], PL <: Player[B, A, PC, PL, R], R <: Rules](
    val grid: Vector[Option[PC]]) {

  type Cell = Option[PC]
  type Location = Option[Cell]

  def get(pos: XY)(implicit boardSize: BoardSize): Location = if (pos.exists) Some(grid(pos.toI)) else None
  def isPiece(l: Location): Boolean = l.flatten.nonEmpty
  def isEmptyCell(l: Location): Boolean = l.nonEmpty && l.flatten.isEmpty
  def pieces = grid.flatten
  def applyUpdate(grid: Vector[Option[PC]], update: (Int, Option[PC])) = grid.updated(update._1, update._2)

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

  def actions(implicit rules: R): Set[A]
  def action(from: XY, delta: XY)(implicit rules: R): Set[A]
  def doAction(a: A)(implicit rules: R): Option[B]
}

object Piece {
  def toXYs(points: Set[(Int, Int)]): Set[XY] = points map (p ⇒ XY(p._1, p._2))
}

abstract class Piece[B <: Board[B, A, PC, PL, R], A <: Action[B, A, PC, PL, R], PC <: Piece[B, A, PC, PL, R], PL <: Player[B, A, PC, PL, R], R <: Rules](
    val pos: XY, val owner: PL) {

  def actions(board: B)(implicit rules: R): Set[A]
  def movedTo(pos: XY): PC // N.B. unsafe (doesn't check bounds)

  @tailrec
  private final def doAllActionsOfDelta(from: XY, delta: XY, board: B, inc: Int = 1, acc: Set[A] = Set())(
    implicit rules: R): Set[A] =
    actionOfDelta(from, delta * inc, board) match {
      case ms if ms.isEmpty ⇒ acc
      case ms               ⇒ doAllActionsOfDelta(from, delta, board, inc + 1, acc ++ ms)
    }

  // N.B. this proxy serves no purpose other than enabling the tailrec optimisation
  protected def allActionsOfDelta(from: XY, delta: XY, board: B)(implicit rules: R) =
    doAllActionsOfDelta(from, delta, board)

  protected def actionOfDelta(from: XY, delta: XY, board: B)(implicit rules: R): Set[A] =
    board.action(from, delta)
}

abstract class Action[B <: Board[B, A, PC, PL, R], A <: Action[B, A, PC, PL, R], PC <: Piece[B, A, PC, PL, R], PL <: Player[B, A, PC, PL, R], R <: Rules](
    fromPiece: PC, delta: XY) {

  def gridUpdates: List[(Int, Option[PC])]
}

abstract class Player[B <: Board[B, A, PC, PL, R], A <: Action[B, A, PC, PL, R], PC <: Piece[B, A, PC, PL, R], PL <: Player[B, A, PC, PL, R], R <: Rules](
    val name: String) {

  def equals(that: PL): Boolean = { that.name == name }

  def pieces(board: B): Set[PC] = {
    board.pieces.filter { a: PC ⇒ a.owner.equals(this) }.toSet
  }

  def actions(board: B)(implicit rules: R): Set[A] = pieces(board) flatMap (_.actions(board))
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
  def chebyshevDistance(that: XY) = math.max(math.abs(this.x - that.x), math.abs(this.y - that.y))
  lazy val subtractXY = x - y
  def sign(that: XY): XY = (that - this).sign
}

case class BoardSize(x: Int, y: Int) // N.B. implementation defines an implicit BoardSize -> wouldn't make this a Point

abstract class Ai[B <: Board[B, A, PC, PL, R], A <: Action[B, A, PC, PL, R], PC <: Piece[B, A, PC, PL, R], PL <: Player[B, A, PC, PL, R], R <: Rules, G <: Game[B, A, PC, PL, R]](
    player: PL, seed: Option[Long] = None) {

  lazy val random = seed map (new Random(_)) getOrElse new Random()
  def nextAction(game: G)(implicit rules: R): Option[A]
}

abstract class RandomAi[B <: Board[B, A, PC, PL, R], A <: Action[B, A, PC, PL, R], PC <: Piece[B, A, PC, PL, R], PL <: Player[B, A, PC, PL, R], R <: Rules, G <: Game[B, A, PC, PL, R]](
  player: PL, seed: Option[Long] = None) extends Ai[B, A, PC, PL, R, G](player, seed) {

  def shuffleHead[T](list: List[T]): Option[T] = random.shuffle(list).headOption
  def nextAction(game: G)(implicit rules: R): Option[A] = shuffleHead(game.board.actions.toList)
}
