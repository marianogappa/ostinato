package ostinato.chess.core

import ostinato.core.{ Action, XY }

abstract class ChessAction(
    val fromPiece: ChessPiece,
    val delta: XY,
    val isCheck: Boolean = false,
    val isCheckmate: Boolean = false) extends Action[ChessBoard, ChessAction, ChessPiece, ChessPlayer, ChessRules](fromPiece, delta) {

  val isFinal = false
  val turn = fromPiece.owner
  val enemy = turn.enemy
  val toAn: String

  def gridUpdates = {
    List(
      fromPiece.pos.toI -> None,
      (fromPiece.pos + delta).toI -> Some(fromPiece.movedTo(fromPiece.pos + delta))
    )
  }
}

abstract class ChessActionFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false): ChessAction
}

case class CaptureActionFactory(fromPiece: ChessPiece, delta: XY, toPiece: ChessPiece) extends ChessActionFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    CaptureAction(fromPiece, delta, toPiece, isCheck, isCheckmate)
}

case class CaptureAction(
    override val fromPiece: ChessPiece,
    override val delta: XY,
    toPiece: ChessPiece,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessAction(fromPiece, delta) {

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} captures ${toPiece.owner.name}'s ${toPiece.pieceName}"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
  val toAn = fromPiece.toAn + 'x' + (fromPiece.pos + delta).toAn + (if (isCheck) Fan.check else "")
}

case class CapturePromoteActionFactory(fromPiece: ChessPiece, delta: XY, capturedPiece: ChessPiece, promotePiece: ChessPiece) extends ChessActionFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    CapturePromoteAction(fromPiece, delta, capturedPiece, promotePiece, isCheck, isCheckmate)
}

case class CapturePromoteAction(
    override val fromPiece: ChessPiece,
    override val delta: XY,
    capturedPiece: ChessPiece,
    promotePiece: ChessPiece,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessAction(fromPiece, delta) {

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} captures ${capturedPiece.owner.name}'s ${capturedPiece.pieceName} and promotes to ${promotePiece.pieceName}"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
  val toAn = fromPiece.toAn + 'x' + (fromPiece.pos + delta).toAn + (if (isCheck) Fan.check else "")
  override def gridUpdates = super.gridUpdates ++ List(promotePiece.pos.toI -> Some(promotePiece))
}

case class MoveActionFactory(fromPiece: ChessPiece, delta: XY) extends ChessActionFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    MoveAction(fromPiece, delta, isCheck, isCheckmate)
}

case class MoveAction(
    override val fromPiece: ChessPiece,
    override val delta: XY,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessAction(fromPiece, delta) {

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} moves to ${fromPiece.pos + delta}"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
  val toAn= fromPiece.toAn + (fromPiece.pos + delta).toAn + (if (isCheck) Fan.check else "")
}

case class EnPassantTakeActionFactory(fromPawn: ♟, delta: XY, toPawn: ♟) extends ChessActionFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    EnPassantCaptureAction(fromPawn, delta, toPawn, isCheck, isCheckmate)
}

case class EnPassantCaptureAction(
    fromPawn: ♟,
    override val delta: XY, toPawn: ♟,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessAction(fromPawn, delta) {

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} captures ${toPawn.owner.name}'s en passant"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
  val toAn = fromPiece.pos.toAn.x.toString + 'x' + (fromPiece.pos + delta).toAn + "e.p." + (if (isCheck) Fan.check else "")
  override def gridUpdates = super.gridUpdates ++ List(toPawn.pos.toI -> None)
}

case class EnPassantActionFactory(fromPawn: ♟, delta: XY) extends ChessActionFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    EnPassantAction(fromPawn, delta, isCheck, isCheckmate)
}

case class EnPassantAction(
    fromPawn: ♟,
    override val delta: XY,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessAction(fromPawn, delta) {

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} moves forward twice (en passant)"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
  val toAn = fromPiece.toAn + (fromPiece.pos + delta).toAn + (if (isCheck) Fan.check else "")
}

case class PromoteActionFactory(fromPiece: ♟, delta: XY, toPiece: ChessPiece) extends ChessActionFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    PromoteAction(fromPiece, delta, toPiece, isCheck, isCheckmate)
}

case class PromoteAction(
    override val fromPiece: ♟,
    override val delta: XY,
    toPiece: ChessPiece,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessAction(fromPiece, delta) {

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} promotes to ${toPiece.pieceName}"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
  val toAn =
    (fromPiece.pos + delta).toAn + toPiece.toAn + (if (isCheck) Fan.check else "")
  override def gridUpdates = super.gridUpdates ++ List(toPiece.pos.toI -> Some(toPiece))
}

object CastlingAction {
  private def rookX(side: CastlingSide.Value) = ♚.rookX(side)
  private def kingDelta(side: CastlingSide.Value) = ♚.kingDelta(side)
  private def rookDelta(side: CastlingSide.Value) = ♚.rookDelta(side)

  private def constructWith(player: ChessPlayer, side: CastlingSide.Value, isCheck: Boolean, isCheckmate: Boolean) =
    CastlingAction(
      ♚(XY(♚.initialX, ♚.initialY(player)), player),
      kingDelta(side),
      ♜(XY(rookX(side), ♚.initialY(player)), player),
      rookDelta(side), isCheck, isCheckmate
    )

  def whiteKingside(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    constructWith(WhiteChessPlayer, CastlingSide.Kingside, isCheck, isCheckmate)

  def whiteQueenside(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    constructWith(WhiteChessPlayer, CastlingSide.Queenside, isCheck, isCheckmate)

  def blackKingside(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    constructWith(BlackChessPlayer, CastlingSide.Kingside, isCheck, isCheckmate)

  def blackQueenside(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    constructWith(BlackChessPlayer, CastlingSide.Queenside, isCheck, isCheckmate)
}

case class CastlingActionFactory(fromPiece: ♚, kingDelta: XY, targetRook: ♜, rookDelta: XY) extends ChessActionFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    CastlingAction(fromPiece, kingDelta, targetRook, rookDelta, isCheck, isCheckmate)
}

case class CastlingAction(
    override val fromPiece: ♚, kingDelta: XY, targetRook: ♜, rookDelta: XY,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessAction(fromPiece, kingDelta) {

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} castles"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
  val toAn = if (isKingside) Fan.kingSideCastle else Fan.queenSideCastle
  def isKingside = kingDelta.x == 2
  def isQueenside = !isKingside

  override def gridUpdates =
    super.gridUpdates ++
      List(
        targetRook.pos.toI -> None,
        (targetRook.pos + rookDelta).toI -> Some(targetRook.movedTo(targetRook.pos + rookDelta))
      )
}

abstract class FinalAction(player: ChessPlayer) extends ChessAction(♚(XY(0, 0), player), XY(0, 0)) {
  val isDraw = false
  val isLoss = false
  override val isFinal = true
}

case class DrawActionFactory(player: ChessPlayer) extends ChessActionFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    DrawAction(player, isCheck, isCheckmate)
}

case class DrawAction(
    player: ChessPlayer,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends FinalAction(player) {

  override val isDraw = true
  override def toString = s"${player.name} claims draw"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
  val toAn = Fan.draw
  override def gridUpdates = List()
}

case class LoseAction(player: ChessPlayer) extends FinalAction(player) {
  override val isLoss = true
  override def toString = s"${player.enemy.name} wins"
  val toAn = Fan.checkmate(player.enemy)
  override def gridUpdates = List()
}
