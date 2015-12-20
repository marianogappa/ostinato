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
  def actions(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Set[ChessAction] =
    pieces(board) flatMap (_.actions(board))

  def cantMoveAction = DrawAction(this)

  def rooks(board: ChessBoard) = pieces(board) filter (_.isRook)
  def knights(board: ChessBoard) = pieces(board) filter (_.isKnight)
  def bishops(board: ChessBoard) = pieces(board) filter (_.isBishop)
  def queens(board: ChessBoard) = pieces(board) filter (_.isQueen)
  def kings(board: ChessBoard) = pieces(board) filter (_.isKing)
  def pawns(board: ChessBoard) = pieces(board) filter (_.isPawn)
}
