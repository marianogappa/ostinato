package ostinato.chess.core

import ostinato.core.{ Action, XY }

abstract class ChessAction(
    val fromPiece: ChessPiece,
    val delta: XY,
    val isCheck: Boolean = false,
    val isCheckmate: Boolean = false) extends Action[ChessPiece](fromPiece, delta) {

  def toAn(implicit rules: ChessRules = ChessRules.default): String
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
  def toAn(implicit rules: ChessRules = ChessRules.default) =
    fromPiece.toAn + 'x' + (fromPiece.pos + delta).toAn + (if (isCheck) Fan.check else "")
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
  def toAn(implicit rules: ChessRules = ChessRules.default) = fromPiece.toAn + (fromPiece.pos + delta).toAn + (if (isCheck) Fan.check else "")
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
  def toAn(implicit rules: ChessRules = ChessRules.default) =
    fromPiece.pos.toAn.x.toString + 'x' + (fromPiece.pos + delta).toAn + "e.p." + (if (isCheck) Fan.check else "")
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
  def toAn(implicit rules: ChessRules = ChessRules.default) = fromPiece.toAn + (fromPiece.pos + delta).toAn + (if (isCheck) Fan.check else "")
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

  override def toString = s"${fromPiece.owner.name}'s ${fromPiece.pieceName} promotes"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
  def toAn(implicit rules: ChessRules = ChessRules.default) =
    (fromPiece.pos + delta).toAn + toPiece.toAn + (if (isCheck) Fan.check else "")
  override def gridUpdates = super.gridUpdates ++ List(toPiece.pos.toI -> Some(toPiece))
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
  def toAn(implicit rules: ChessRules = ChessRules.default) =
    if (kingDelta.sign.x == 1) Fan.kingSideCastle else Fan.queenSideCastle
  override def gridUpdates =
    super.gridUpdates ++
      List(
        targetRook.pos.toI -> None,
        (targetRook.pos + rookDelta).toI -> Some(targetRook.movedTo(targetRook.pos + rookDelta))
      )
}

case class DrawActionFactory(fromPlayer: ChessPlayer) extends ChessActionFactory {
  def complete(isCheck: Boolean = false, isCheckmate: Boolean = false) =
    DrawAction(fromPlayer, isCheck, isCheckmate)
}

case class DrawAction(
    fromPlayer: ChessPlayer,
    override val isCheck: Boolean = false,
    override val isCheckmate: Boolean = false) extends ChessAction(♚(XY(0, 0), fromPlayer), XY(0, 0)) {
  override def toString = s"${fromPiece.owner.name}'s claims draw"
  def withCheck = this.copy(isCheck = true)
  def withCheckmate = this.copy(isCheckmate = true)
  def toAn(implicit rules: ChessRules = ChessRules.default) = Fan.draw
  override def gridUpdates = List()
}
