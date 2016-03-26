package ostinato.chess.core

import ostinato.core.Player

case object WhiteChessPlayer extends ChessPlayer("White") {
  def enemy = BlackChessPlayer
  def toFen = 'w'
}

case object BlackChessPlayer extends ChessPlayer("Black") {
  def enemy = WhiteChessPlayer
  def toFen = 'b'
}

abstract class ChessPlayer(name: String) extends Player[ChessBoard, ChessAction, ChessPiece, ChessPlayer, ChessOptimisations](name) {
  def kingPiece(board: ChessBoard): Option[ChessPiece] = pieces(board).find(_.isKing)
  def enemy: ChessPlayer
  def toFen: Char

  def initialY = this match {
    case WhiteChessPlayer => 7
    case BlackChessPlayer => 0
  }

  override def actions(board: ChessBoard)(implicit rules: ChessOptimisations = ChessOptimisations.default): Set[ChessAction] = {
    val noDeltaValidation = rules.copy(validateDeltasOnActionCalculation = false)
    (board.hasInsufficientMaterial, super.actions(board)(noDeltaValidation), kingPiece(board)) match {
      case (true, _, _)                 ⇒ Set(DrawAction(this))
      case (_, a, Some(k)) if a.isEmpty ⇒ Set(if (k.isThreatened(board)) LoseAction(this) else DrawAction(this))
      case (_, a, _)                    ⇒ a ++ Set(LoseAction(this), DrawAction(this))
    }
  }

  // TODO actionStream doesn't have: ++ Set(LoseAction(this), DrawAction(this))
  override def actionStream(board: ChessBoard)(implicit rules: ChessOptimisations = ChessOptimisations.default): Stream[ChessAction] = {
    val noDeltaValidation = rules.copy(validateDeltasOnActionCalculation = false)
    (board.hasInsufficientMaterial, super.actionStream(board)(noDeltaValidation), kingPiece(board)) match {
      case (true, _, _)                 ⇒ Stream(DrawAction(this))
      case (_, a, Some(k)) if a.isEmpty ⇒ Stream(if (k.isThreatened(board)) LoseAction(this) else DrawAction(this))
      case (_, a, _)                    ⇒ a
    }
  }

  def nonFinalActions(board: ChessBoard)(implicit rules: ChessOptimisations = ChessOptimisations.default) =
    actions(board).filter {
      case a: FinalAction ⇒ false
      case _              ⇒ true
    }

  def rooks(board: ChessBoard): Set[♜] = pieces(board) flatMap { case p: ♜ ⇒ Set(p); case _ ⇒ Set.empty[♜] }
  def knights(board: ChessBoard): Set[♞] = pieces(board) flatMap { case p: ♞ ⇒ Set(p); case _ ⇒ Set.empty[♞] }
  def bishops(board: ChessBoard): Set[♝] = pieces(board) flatMap { case p: ♝ ⇒ Set(p); case _ ⇒ Set.empty[♝] }
  def queens(board: ChessBoard): Set[♛] = pieces(board) flatMap { case p: ♛ ⇒ Set(p); case _ ⇒ Set.empty[♛] }
  def kings(board: ChessBoard): Set[♚] = pieces(board) flatMap { case p: ♚ ⇒ Set(p); case _ ⇒ Set.empty[♚] }
  def pawns(board: ChessBoard): Set[♟] = pieces(board) flatMap { case p: ♟ ⇒ Set(p); case _ ⇒ Set.empty[♟] }
}
