package ostinato.chess.core

import ostinato.core.Rules

object ChessRules {
  def default = ChessRules(
    kingIsTakeable = false,
    checkForThreatens = true,
    validateDeltasOnActionCalculation = true,
    extraValidationOnActionApply = false
  )
}

case class ChessRules(
  kingIsTakeable: Boolean,
  checkForThreatens: Boolean,
  validateDeltasOnActionCalculation: Boolean,
  extraValidationOnActionApply: Boolean) extends Rules

