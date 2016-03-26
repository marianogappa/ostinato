package ostinato.chess.core

import ostinato.core.Rules

object ChessRules {
  def default = ChessRules(
    whitePawnDirection = -1,
    kingIsTakeable = false,
    checkForThreatens = true,
    validateDeltasOnActionCalculation = true,
    extraValidationOnActionApply = false
  )
}

case class ChessRules(
                       whitePawnDirection: Int,
                       kingIsTakeable: Boolean,
                       checkForThreatens: Boolean,
                       validateDeltasOnActionCalculation: Boolean,
                       extraValidationOnActionApply: Boolean
                     ) extends Rules

