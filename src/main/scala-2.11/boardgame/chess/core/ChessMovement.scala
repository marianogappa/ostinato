package boardgame.chess.core

import boardgame.core.{ Movement, XY }

// TODO it's easy to implement threatens: Set[ChessPiece]
// TODO override toString
class ChessMovement(val fromPiece: ChessPiece, val delta: XY) extends Movement[ChessPiece](fromPiece, delta)
case class TakeMovement(override val fromPiece: ChessPiece, override val delta: XY, toPiece: ChessPiece) extends ChessMovement(fromPiece, delta)
case class MoveMovement(override val fromPiece: ChessPiece, override val delta: XY) extends ChessMovement(fromPiece, delta)
case class EnPassantTakeMovement(fromPawn: ♟, override val delta: XY, toPawn: ♟) extends ChessMovement(fromPawn, delta)
case class EnPassantMovement(fromPawn: ♟, override val delta: XY) extends ChessMovement(fromPawn, delta)
case class PromoteMovement(override val fromPiece: ♟, override val delta: XY) extends ChessMovement(fromPiece, delta)
case class CastlingMovement(override val fromPiece: ♚, kingDelta: XY, targetRook: ♜, rookDelta: XY) extends ChessMovement(fromPiece, kingDelta)

object CheckMateMovement {
  def from(m: ChessMovement) = CheckMateMovement(m.fromPiece, m.delta)
}
case class CheckMateMovement(override val fromPiece: ChessPiece, override val delta: XY) extends ChessMovement(fromPiece, delta)
