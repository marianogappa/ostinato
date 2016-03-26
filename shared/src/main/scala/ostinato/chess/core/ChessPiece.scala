package ostinato.chess.core

import ostinato.chess.core.CastlingSide.CastlingSide
import ostinato.core.{ XY, Piece }

import scala.util.control.NoStackTrace

abstract class ChessPiece(pos: XY, owner: ChessPlayer) extends Piece[ChessBoard, ChessAction, ChessPiece, ChessPlayer, ChessRules](pos, owner) {
  val (isRook, isKnight, isBishop, isQueen, isKing, isPawn) = (false, false, false, false, false, false)
  val toAn: String
  val toFen: Char
  val toIccf: Int
  val toDn: Set[String]
  val toFigurine: Char
  val pieceName: String

  def isThreatened(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Boolean = threatenedBy(board).nonEmpty
  def isDefended(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Boolean = defendedBy(board).nonEmpty

  def threatenedBy(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Option[ChessPiece] = {
    posThreatenedBy(pos, owner, board)
  }

  def defendedBy(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Option[ChessPiece] =
    withOwner(enemy).threatenedBy(board)

  def canMoveTo(to: XY, board: ChessBoard)(implicit rules: ChessRules = ChessRules.default) = {
    !cantMove(to, board) && actions(board).exists {
      m ⇒ (pos + m.delta) == to
    }
  }

  def cantMove(to: XY, board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Boolean

  val enemy: ChessPlayer = this.owner.enemy
  def withOwner(newOwner: ChessPlayer): ChessPiece
  def equals(that: ChessPiece) = pos == that.pos && owner == that.owner
  override def toString = s"${owner.name}'s $pieceName on (${pos.x}, ${pos.y})"

  protected val deltaPatterns: Set[XY]
  protected val hasRecursiveDeltas: Boolean

  private def concreteDeltas(board: ChessBoard, ds: Set[XY], accDeltas: Set[XY], inc: Int = 1)(
    implicit rules: ChessRules = ChessRules.default): Set[XY] = {

    ds.map(d ⇒ (d, board.get(pos + d * inc))).flatMap[XY, Set[XY]] {
      case (d: XY, Some(None)) if hasRecursiveDeltas ⇒ concreteDeltas(board, Set(d), accDeltas + (d * inc), inc + 1)
      case (d: XY, Some(None)) ⇒ accDeltas + (d * inc)
      case (d: XY, Some(Some(piece: ChessPiece))) if piece.owner != owner ⇒ accDeltas + (d * inc)
      case (_: XY, Some(Some(piece: ChessPiece))) ⇒ accDeltas
      case (_: XY, None) ⇒ accDeltas
    }
  }

  def deltas(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default) =
    concreteDeltas(board, deltaPatterns, Set())

  def actions(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Set[ChessAction] = {
    deltas(board).flatMap { case delta ⇒ movementsOfDelta(pos, delta, board) }
  }
}

case class ♜(override val pos: XY, override val owner: ChessPlayer) extends ChessPiece(pos, owner) {
  protected val hasRecursiveDeltas = true
  protected val deltaPatterns = Piece.toXYs(Set((-1, 0), (1, 0), (0, -1), (0, 1)))

  lazy val castlingSide =
    if (pos.x == 0)
      Some(CastlingSide.Queenside)
    else if (pos.x == 7)
      Some(CastlingSide.Kingside)
    else
      None

  val toFigurine = owner match {
    case BlackChessPlayer ⇒ '♜'
    case WhiteChessPlayer ⇒ '♖'
  }

  val pieceName = "Rook"
  val toAn = "R"
  val toDn = Set("R", "KR", "QR")
  lazy val toIccf = 2
  val toFen = if (owner == WhiteChessPlayer) toAn.head else toAn.head.toLower
  override val isRook = true
  def withOwner(newOwner: ChessPlayer) = ♜(pos, newOwner)
  def movedTo(newXY: XY) = ♜(newXY, owner)
  override def cantMove(to: XY, board: ChessBoard)(implicit rules: ChessRules = ChessRules.default) =
    pos.x != to.x && pos.y != to.y
}

case class ♝(override val pos: XY, override val owner: ChessPlayer) extends ChessPiece(pos, owner) {
  protected val hasRecursiveDeltas = true
  protected val deltaPatterns = Piece.toXYs(Set((-1, -1), (1, 1), (-1, 1), (1, -1)))

  val toFigurine = owner match {
    case BlackChessPlayer ⇒ '♝'
    case WhiteChessPlayer ⇒ '♗'
  }

  val pieceName = "Bishop"
  val toAn = "B"
  val toDn = Set("B", "KB", "QB")
  lazy val toIccf = 3
  val toFen = if (owner == WhiteChessPlayer) toAn.head else toAn.head.toLower
  override val isBishop = true
  def withOwner(newOwner: ChessPlayer) = ♝(pos, newOwner)
  def movedTo(newXY: XY) = ♝(newXY, owner)
  override def cantMove(to: XY, board: ChessBoard)(implicit rules: ChessRules = ChessRules.default) =
    (pos - to).abs.subtractXY != 0
}

case class ♞(override val pos: XY, override val owner: ChessPlayer) extends ChessPiece(pos, owner) {
  protected val hasRecursiveDeltas = false
  protected val deltaPatterns = Piece.toXYs(Set((-1, -2), (1, -2), (-1, 2), (1, 2), (-2, -1), (-2, 1), (2, -1), (2, 1)))

  val toFigurine = owner match {
    case BlackChessPlayer ⇒ '♞'
    case WhiteChessPlayer ⇒ '♘'
  }

  val pieceName = "Knight"
  val toAn = "N"
  val toDn = Set("N", "KN", "QN")
  lazy val toIccf = 4
  val toFen = if (owner == WhiteChessPlayer) toAn.head else toAn.head.toLower
  override val isKnight = true
  def withOwner(newOwner: ChessPlayer) = ♞(pos, newOwner)
  def movedTo(newXY: XY) = ♞(newXY, owner)
  override def cantMove(to: XY, board: ChessBoard)(implicit rules: ChessRules = ChessRules.default) =
    deltas(board).forall(pos + _ != to)
}

case class ♛(override val pos: XY, override val owner: ChessPlayer) extends ChessPiece(pos, owner) {
  protected val hasRecursiveDeltas = true
  protected val deltaPatterns = Piece.toXYs(Set((-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (1, 1), (-1, 1), (1, -1)))

  val toFigurine = owner match {
    case BlackChessPlayer ⇒ '♛'
    case WhiteChessPlayer ⇒ '♕'
  }

  val pieceName = "Queen"
  val toAn = "Q"
  val toDn = Set("Q")
  lazy val toIccf = 1
  val toFen = if (owner == WhiteChessPlayer) toAn.head else toAn.head.toLower
  override val isQueen = true
  def withOwner(newOwner: ChessPlayer) = ♛(pos, newOwner)
  def movedTo(newXY: XY) = ♛(newXY, owner)
  override def cantMove(to: XY, board: ChessBoard)(implicit rules: ChessRules = ChessRules.default) =
    (pos - to).abs.subtractXY != 0 && pos.x != to.x && pos.y != to.y
}

object ♚ {
  val initialX = 4
  def initialY(owner: ChessPlayer) = if (owner == BlackChessPlayer) 0 else 7
  def rookDelta(kingDelta: XY) = if (kingDelta.x == -2) XY(3, 0) else XY(-2, 0)
  def rookDelta(castlingSide: CastlingSide) = if (castlingSide == CastlingSide.Kingside) XY(-2, 0) else XY(3, 0)
  def rookX(castlingSide: CastlingSide) = if (castlingSide == CastlingSide.Kingside) 7 else 0
  def kingDelta(castlingSide: CastlingSide) =  if (castlingSide == CastlingSide.Kingside) XY(2, 0) else XY(-2, 0)
}

case class ♚(override val pos: XY, override val owner: ChessPlayer) extends ChessPiece(pos, owner) {
  protected val hasRecursiveDeltas = false
  protected lazy val deltaPatterns =
    Piece.toXYs(Set((-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (1, 1), (-1, 1), (1, -1))) ++
      (if (isInInitialPosition) Piece.toXYs(Set((-2, 0), (2, 0))) else Set())

  def initialY = ♚.initialY(owner)
  def initialX = ♚.initialX
  def targetRookPosition(dx: Int) = XY(if (dx < 0) 0 else 7, initialY)
  def rookDeltaFor(kingDelta: XY) = ♚.rookDelta(kingDelta)
  lazy val isInInitialPosition = pos.x == initialX && pos.y == initialY

  val toFigurine = owner match {
    case BlackChessPlayer ⇒ '♚'
    case WhiteChessPlayer ⇒ '♔'
  }

  val pieceName = "King"
  val toAn = "K"
  val toDn = Set("K")
  lazy val toIccf = throw new RuntimeException("King does not have an Iccf code") with NoStackTrace
  val toFen = if (owner == WhiteChessPlayer) toAn.head else toAn.head.toLower
  override val isKing = true
  def withOwner(newOwner: ChessPlayer) = ♚(pos, newOwner)
  def movedTo(newXY: XY) = ♚(newXY, owner)
  override def cantMove(to: XY, board: ChessBoard)(implicit rules: ChessRules = ChessRules.default) =
    pos.chebyshevDistance(to) > 1
}
case class ♟(override val pos: XY, override val owner: ChessPlayer, dy: Int) extends ChessPiece(pos, owner) {
  val isInInitialPosition = dy == 1 && pos.y == 1 || dy == -1 && pos.y == 6
  protected val hasRecursiveDeltas = false
  protected val deltaPatterns =
    Piece.toXYs(Set((-1, dy), (0, dy), (1, dy)) ++ (if (isInInitialPosition) Set((0, 2 * dy)) else Set()))

  def promotingPosition(dy: Int) = Map(-1 -> 0, 1 -> 7)(dy)
  val distanceToPromotion = math.abs(pos.y - promotingPosition(dy))
  val isPromoting = pos.y == promotingPosition(dy)

  val toFigurine = owner match {
    case BlackChessPlayer ⇒ '♟'
    case WhiteChessPlayer ⇒ '♙'
  }

  val pieceName = "Pawn"
  val toAn = ""
  val toDn = Set("P")
  lazy val toIccf = throw new RuntimeException("Pawn does not have an Iccf code") with NoStackTrace
  val toFen = if (owner == WhiteChessPlayer) 'P' else 'p'
  override val isPawn = true
  def withOwner(newOwner: ChessPlayer) = ♟(pos, newOwner, dy)
  def movedTo(newXY: XY) = ♟(newXY, owner, dy)
  override def cantMove(to: XY, board: ChessBoard)(implicit rules: ChessRules = ChessRules.default) =
    deltas(board).forall(pos + _ != to)
}

object EnPassantPawn {
  def fromXYD(pos: XY, delta: XY, grid: Vector[Option[ChessPiece]]): Option[EnPassantPawn] = {
    if (pos.exists && (pos + delta).exists) {
      grid((pos + delta).toI) map {
        case p: ♟ ⇒ EnPassantPawn(pos, p)
      }
    } else None
  }
}
case class EnPassantPawn(from: XY, pawn: ♟)
