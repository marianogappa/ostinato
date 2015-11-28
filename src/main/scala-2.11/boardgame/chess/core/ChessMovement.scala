package boardgame.chess.core

import boardgame.core.{ Movement, XY }

// TODO it's easy to implement threatens: Set[ChessPiece]
// TODO override toString
class ChessMovement(
    val fromPiece: ChessPiece,
    val delta: XY,
    val isCheck: Boolean = false,
    val isCheckmate: Boolean = false) extends Movement[ChessPiece](fromPiece, delta) {
}

abstract class ChessMovementFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false): ChessMovement
}

case class TakeMovementFactory(fromPiece: ChessPiece, delta: XY, toPiece: ChessPiece) extends ChessMovementFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    TakeMovement(fromPiece, delta, toPiece, isCheck, isCheckmate)
}

case class TakeMovement(
    override val fromPiece: ChessPiece,
    override val delta: XY,
    toPiece: ChessPiece,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessMovement(fromPiece, delta) {

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} takes ${toPiece.owner.name}'s ${toPiece.pieceName}"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
}

case class MoveMovementFactory(fromPiece: ChessPiece, delta: XY) extends ChessMovementFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    MoveMovement(fromPiece, delta, isCheck, isCheckmate)
}

case class MoveMovement(
    override val fromPiece: ChessPiece,
    override val delta: XY,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessMovement(fromPiece, delta) {

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} moves to ${fromPiece.pos + delta}"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
}

case class EnPassantTakeMovementFactory(fromPawn: ♟, delta: XY, toPawn: ♟) extends ChessMovementFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    EnPassantTakeMovement(fromPawn, delta, toPawn, isCheck, isCheckmate)
}

case class EnPassantTakeMovement(
    fromPawn: ♟,
    override val delta: XY, toPawn: ♟,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessMovement(fromPawn, delta) {

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} takes ${toPawn.owner.name}'s en passant"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
}

case class EnPassantMovementFactory(fromPawn: ♟, delta: XY) extends ChessMovementFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    EnPassantMovement(fromPawn, delta, isCheck, isCheckmate)
}

case class EnPassantMovement(
    fromPawn: ♟,
    override val delta: XY,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessMovement(fromPawn, delta) {

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} moves forward twice (en passant)"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
}

case class PromoteMovementFactory(fromPiece: ♟, delta: XY, toPiece: ChessPiece) extends ChessMovementFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    PromoteMovement(fromPiece, delta, toPiece, isCheck, isCheckmate)
}

case class PromoteMovement(
    override val fromPiece: ♟,
    override val delta: XY,
    toPiece: ChessPiece,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessMovement(fromPiece, delta) {

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} promotes"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
}

case class CastlingMovementFactory(fromPiece: ♚, kingDelta: XY, targetRook: ♜, rookDelta: XY) extends ChessMovementFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    CastlingMovement(fromPiece, kingDelta, targetRook, rookDelta, isCheck, isCheckmate)
}

case class CastlingMovement(
    override val fromPiece: ♚, kingDelta: XY, targetRook: ♜, rookDelta: XY,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessMovement(fromPiece, kingDelta) {

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} castles"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
}
