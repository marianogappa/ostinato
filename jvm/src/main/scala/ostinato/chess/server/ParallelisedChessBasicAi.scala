package ostinato.chess.server

import ostinato.chess.ai.ChessBasicAi
import ostinato.chess.core.{ChessAction, ChessGame, ChessOptimisations, ChessPlayer}

class ParallelisedChessBasicAi(player: ChessPlayer, depth: Int = 1, debug: Boolean = false) extends
  ChessBasicAi(player, depth, debug) {

  override def evaluateAllActions(actions: Stream[ChessAction], game: ChessGame, optsForAi: ChessOptimisations) =
    actions.par.map { action ⇒
      if (debug) {
        println(s"Evaluating [$action] with depth [$depth] on thread [${Thread.currentThread()}]")
      }
      (action, alphabeta(game.board.doAction(action)(optsForAi).get, action)(optsForAi))
    }.seq
}
