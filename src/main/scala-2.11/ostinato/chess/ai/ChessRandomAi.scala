package ostinato.chess.ai

import ostinato.chess.core.{ ChessGame, ChessMovement, ChessBoard, ChessPlayer }
import ostinato.core.RandomAi

case class ChessRandomAi(player: ChessPlayer, seed: Option[Long] = None) extends RandomAi[ChessBoard, ChessPlayer, ChessGame, ChessMovement](player, seed)