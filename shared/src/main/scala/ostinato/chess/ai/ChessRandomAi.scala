package ostinato.chess.ai

import ostinato.chess.core._
import ostinato.core.RandomAi

case class ChessRandomAi(player: ChessPlayer, seed: Option[Long] = None)
  extends RandomAi[ChessBoard, ChessAction, ChessPiece, ChessPlayer, ChessOptimisations, ChessGame](player, seed) {

  // N.B. the only reason this is here is to have a default implicit value for rules
  override def nextAction(game: ChessGame)(implicit opts: ChessOptimisations = ChessOptimisations.default): Option[ChessAction] =
    super.nextAction(game)

  def nextNonFinalAction(game: ChessGame): Option[ChessAction] = {
    val optsForAi = ChessOptimisations.default.copy(
      extraValidationOnActionApply = false, dontCalculateHistory = true)

    shuffleHead(game.board.actionStream(optsForAi).toList)
  }
}
