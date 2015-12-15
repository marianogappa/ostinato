package ostinato.chess.core

import ostinato.core.{ BoardSize, XY, Piece }

abstract class ChessPiece(pos: XY, owner: ChessPlayer) extends Piece[ChessPlayer, ChessMovement, ChessBoard, ChessRules, ChessPiece](pos, owner) {
  val (isRook, isKnight, isBishop, isQueen, isKing, isPawn) = (false, false, false, false, false, false)
  val toAn: String
  val toFen: Char
  def isThreatened(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Boolean = threatenedBy(board).nonEmpty
  def isDefended(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Boolean = defendedBy(board).nonEmpty

  def threatenedBy(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Set[ChessPiece] =
    enemy.pieces(board).filter(_.canMoveTo(pos, board)(rules.copy(kingIsTakeable = true, checkForThreatens = false)))

  def defendedBy(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Set[ChessPiece] =
    owner.pieces(board).filter(_.canMoveTo(pos, board.move(MoveMovement(withOwner(enemy), XY(0, 0))))(
      rules.copy(checkForThreatens = false)))

  def canMoveTo(to: XY, board: ChessBoard)(implicit rules: ChessRules = ChessRules.default) = !cantMove(to) && movements(board).exists {
    m ⇒ (pos + m.delta) == to
  }

  def cantMove(to: XY)(implicit rules: ChessRules = ChessRules.default): Boolean

  def enemy: ChessPlayer = this.owner.enemy
  def withOwner(newOwner: ChessPlayer): ChessPiece
  def equals(that: ChessPiece) = pos == that.pos && owner == that.owner
  override def toString = s"${owner.name}'s $pieceName on (${pos.x}, ${pos.y})"
  def movements(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Set[ChessMovement]
  val toChar: Char
  val pieceName: String
}

object ♜ {
  val deltas = Piece.toXYs(Set((-1, 0), (1, 0), (0, -1), (0, 1)))
  def char(owner: ChessPlayer) = owner match {
    case BlackChessPlayer ⇒ '♜'
    case WhiteChessPlayer ⇒ '♖'
  }
}
object ♝ {
  val deltas = Piece.toXYs(Set((-1, -1), (1, 1), (-1, 1), (1, -1)))
  def char(owner: ChessPlayer) = owner match {
    case BlackChessPlayer ⇒ '♝'
    case WhiteChessPlayer ⇒ '♗'
  }
}
object ♞ {
  val deltas = Piece.toXYs(Set((-1, -2), (1, -2), (-1, 2), (1, 2), (-2, -1), (-2, 1), (2, -1), (2, 1)))
  def char(owner: ChessPlayer) = owner match {
    case BlackChessPlayer ⇒ '♞'
    case WhiteChessPlayer ⇒ '♘'
  }
}
object ♚ {
  def deltas(addCastlingDeltas: Boolean) = normalDeltas ++ (if (addCastlingDeltas) Piece.toXYs(Set((-2, 0), (2, 0))) else Set())
  def normalDeltas = ♜.deltas ++ ♝.deltas
  def rookDeltaFor(kingDelta: XY) = XY(if (kingDelta.x < 0) 3 else -2, 0)
  def char(owner: ChessPlayer) = owner match {
    case BlackChessPlayer ⇒ '♚'
    case WhiteChessPlayer ⇒ '♔'
  }
}
object ♛ {
  val deltas = ♚.normalDeltas
  def char(owner: ChessPlayer) = owner match {
    case BlackChessPlayer ⇒ '♛'
    case WhiteChessPlayer ⇒ '♕'
  }
}
object ♟ {
  def deltas(dy: Int, isInInitialPosition: Boolean) =
    Piece.toXYs(Set((-1, dy), (0, dy), (1, dy)) ++ (if (isInInitialPosition) Set((0, 2 * dy)) else Set()))

  def char(owner: ChessPlayer) = owner match {
    case BlackChessPlayer ⇒ '♟'
    case WhiteChessPlayer ⇒ '♙'
  }
  def promotingPosition(dy: Int)(implicit boardSize: BoardSize) = Map(-1 -> 0, 1 -> (boardSize.x - 1))(dy)
}

case class ♜(override val pos: XY, override val owner: ChessPlayer) extends ChessPiece(pos, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Set[ChessMovement] = {
    ♜.deltas.flatMap { case delta ⇒ allMovementsOfDelta(pos, delta, board) }
  }
  val toChar = ♜.char(owner)
  val pieceName = "Rook"
  val toAn = "R"
  val toFen = if (owner == WhiteChessPlayer) toAn.head else toAn.head.toLower
  override val isRook = true
  def withOwner(newOwner: ChessPlayer) = ♜(pos, newOwner)
  def movedTo(newXY: XY) = ♜(newXY, owner)
  override def cantMove(to: XY)(implicit rules: ChessRules = ChessRules.default) = pos.x != to.x && pos.y != to.y
}

case class ♝(override val pos: XY, override val owner: ChessPlayer) extends ChessPiece(pos, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Set[ChessMovement] = {
    ♝.deltas.flatMap { case delta ⇒ allMovementsOfDelta(pos, delta, board) }
  }
  val toChar = ♝.char(owner)
  val pieceName = "Bishop"
  val toAn = "B"
  val toFen = if (owner == WhiteChessPlayer) toAn.head else toAn.head.toLower
  override val isBishop = true
  def withOwner(newOwner: ChessPlayer) = ♝(pos, newOwner)
  def movedTo(newXY: XY) = ♝(newXY, owner)
  override def cantMove(to: XY)(implicit rules: ChessRules = ChessRules.default) = (pos - to).abs.subtractXY != 0
}

case class ♞(override val pos: XY, override val owner: ChessPlayer) extends ChessPiece(pos, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Set[ChessMovement] = {
    ♞.deltas.flatMap { case delta ⇒ movementOfDelta(pos, delta, board) }
  }
  val toChar = ♞.char(owner)
  val pieceName = "Knight"
  val toAn = "N"
  val toFen = if (owner == WhiteChessPlayer) toAn.head else toAn.head.toLower
  override val isKnight = true
  def withOwner(newOwner: ChessPlayer) = ♞(pos, newOwner)
  def movedTo(newXY: XY) = ♞(newXY, owner)
  override def cantMove(to: XY)(implicit rules: ChessRules = ChessRules.default) = ♞.deltas.forall(pos + _ != to)
}

case class ♛(override val pos: XY, override val owner: ChessPlayer) extends ChessPiece(pos, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Set[ChessMovement] = {
    ♛.deltas.flatMap { case delta ⇒ allMovementsOfDelta(pos, delta, board) }
  }
  val toChar = ♛.char(owner)
  val pieceName = "Queen"
  val toAn = "Q"
  val toFen = if (owner == WhiteChessPlayer) toAn.head else toAn.head.toLower
  override val isQueen = true
  def withOwner(newOwner: ChessPlayer) = ♛(pos, newOwner)
  def movedTo(newXY: XY) = ♛(newXY, owner)
  override def cantMove(to: XY)(implicit rules: ChessRules = ChessRules.default) = (pos - to).abs.subtractXY != 0 && pos.x != to.x && pos.y != to.y
}

case class ♚(override val pos: XY, override val owner: ChessPlayer) extends ChessPiece(pos, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Set[ChessMovement] = {
    ♚.deltas(isInInitialPosition).flatMap { case delta ⇒ movementOfDelta(pos, delta, board) }
  }

  def initialY(implicit rules: ChessRules = ChessRules.default, chessBoardSize: BoardSize) =
    if (owner == WhiteChessPlayer && rules.whitePawnDirection == 1 ||
      owner == BlackChessPlayer && rules.whitePawnDirection == -1)
      0
    else
      chessBoardSize.y - 1

  def targetRookPosition(dx: Int)(implicit rules: ChessRules = ChessRules.default) = XY(if (dx < 0) 0 else chessBoardSize.x - 1, initialY)

  def isInInitialPosition(implicit rules: ChessRules = ChessRules.default) = pos.x == 4 && pos.y == initialY
  val toChar = ♚.char(owner)
  val pieceName = "King"
  val toAn = "K"
  val toFen = if (owner == WhiteChessPlayer) toAn.head else toAn.head.toLower
  override val isKing = true
  def withOwner(newOwner: ChessPlayer) = ♚(pos, newOwner)
  def movedTo(newXY: XY) = ♚(newXY, owner)
  override def cantMove(to: XY)(implicit rules: ChessRules = ChessRules.default) = pos.chebyshevDistance(to) > 1
}
case class ♟(override val pos: XY, override val owner: ChessPlayer, dy: Int) extends ChessPiece(pos, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Set[ChessMovement] = {
    ♟.deltas(dy, isInInitialPosition).flatMap { case delta ⇒ movementOfDelta(pos, delta, board) }
  }
  val isInInitialPosition = dy == 1 && pos.y == 1 || dy == -1 && pos.y == chessBoardSize.y - 2
  val isPromoting = pos.y == ♟.promotingPosition(dy)
  val toChar = ♟.char(owner)
  val pieceName = "Pawn"
  val toAn = ""
  val toFen = if (owner == WhiteChessPlayer) 'P' else 'p'
  override val isPawn = true
  def withOwner(newOwner: ChessPlayer) = ♟(pos, newOwner, dy)
  def movedTo(newXY: XY) = ♟(newXY, owner, dy)
  override def cantMove(to: XY)(implicit rules: ChessRules = ChessRules.default) = ♟.deltas(dy, isInInitialPosition).forall(pos + _ != to)
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
