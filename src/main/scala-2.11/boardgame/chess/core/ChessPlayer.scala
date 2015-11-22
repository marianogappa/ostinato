package boardgame.chess.core

import boardgame.core.Player

case object WhiteChessPlayer extends ChessPlayer("White") {
  def enemy = BlackChessPlayer
}

case object BlackChessPlayer extends ChessPlayer("Black") {
  def enemy = WhiteChessPlayer
}

abstract class ChessPlayer(name: String) extends Player[ChessBoard, ChessMovement, ChessPiece, ChessPlayer](name) {
  def kingPiece(board: ChessBoard): Option[ChessPiece] = pieces(board).find(_.isKing)
  def enemy: ChessPlayer
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] =
    pieces(board) flatMap (_.movements(board))
}
