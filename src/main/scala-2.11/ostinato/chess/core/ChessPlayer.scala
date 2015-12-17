package ostinato.chess.core

import ostinato.core.Player

case object WhiteChessPlayer extends ChessPlayer("White") {
  def enemy = BlackChessPlayer
}

case object BlackChessPlayer extends ChessPlayer("Black") {
  def enemy = WhiteChessPlayer
}

abstract class ChessPlayer(name: String) extends Player[ChessBoard, ChessMovement, ChessPiece, ChessPlayer](name) {
  def kingPiece(board: ChessBoard): Option[ChessPiece] = pieces(board).find(_.isKing)
  def enemy: ChessPlayer
  def movements(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Set[ChessMovement] =
    pieces(board) flatMap (_.movements(board))

  def cantMoveMovement = DrawMovement(this)

  def rooks(board: ChessBoard) = pieces(board) filter (_.isRook)
  def knights(board: ChessBoard) = pieces(board) filter (_.isKnight)
  def bishops(board: ChessBoard) = pieces(board) filter (_.isBishop)
  def queens(board: ChessBoard) = pieces(board) filter (_.isQueen)
  def kings(board: ChessBoard) = pieces(board) filter (_.isKing)
  def pawns(board: ChessBoard) = pieces(board) filter (_.isPawn)
}
