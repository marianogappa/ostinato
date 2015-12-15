package ostinato.chess.core

import ostinato.core.Rules

object ChessRules {
  def default = ChessRules(
    whitePawnDirection = -1,
    kingIsTakeable = false,
    allowImpossibleBoards = false,
    noKingMeansLoss = false,
    checkForThreatens = true
  )
}

case class ChessRules(
                       whitePawnDirection: Int,
                       kingIsTakeable: Boolean,
                       allowImpossibleBoards: Boolean,
                       noKingMeansLoss: Boolean,
                       checkForThreatens: Boolean) extends Rules

