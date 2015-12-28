package ostinato.chess.core

import ostinato.core.Player

case object WhiteChessPlayer extends ChessPlayer("White") {
  def enemy = BlackChessPlayer
}

case object BlackChessPlayer extends ChessPlayer("Black") {
  def enemy = WhiteChessPlayer
}

abstract class ChessPlayer(name: String) extends Player[ChessBoard, ChessAction, ChessPiece, ChessPlayer, ChessRules](name) {
  def kingPiece(board: ChessBoard): Option[ChessPiece] = pieces(board).find(_.isKing)
  def enemy: ChessPlayer

  override def actions(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Set[ChessAction] =
    (board.insufficientMaterial, super.actions(board), kingPiece(board)) match {
      case (true, _, _)                 ⇒ Set(DrawAction(this))
      case (_, a, Some(k)) if a.isEmpty ⇒ Set(if (k.isThreatened(board)) LoseAction(this) else DrawAction(this))
      case (_, a, _)                    ⇒ super.actions(board) ++ Set(LoseAction(this), DrawAction(this))
    }

  def nonWinDrawActions(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default) =
    actions(board).filter {
      case LoseAction(_)       ⇒ false
      case DrawAction(_, _, _) ⇒ false
      case _                   ⇒ true
    }

  def rooks(board: ChessBoard) = pieces(board) filter (_.isRook)
  def knights(board: ChessBoard) = pieces(board) filter (_.isKnight)
  def bishops(board: ChessBoard) = pieces(board) filter (_.isBishop)
  def queens(board: ChessBoard) = pieces(board) filter (_.isQueen)
  def kings(board: ChessBoard) = pieces(board) filter (_.isKing)
  def pawns(board: ChessBoard) = pieces(board) filter (_.isPawn)
}
