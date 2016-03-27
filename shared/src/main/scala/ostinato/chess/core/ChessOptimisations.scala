package ostinato.chess.core

import ostinato.core.Rules

object ChessOptimisations {
  def default = ChessOptimisations(
    kingIsTakeable = false,
    checkForThreatens = true,
    validateDeltasOnActionCalculation = true,
    extraValidationOnActionApply = false,
    dontCalculateHistory = false
  )

  def noCheckForThreatens = default.copy(checkForThreatens = false)
}

case class ChessOptimisations(
  kingIsTakeable: Boolean,
  checkForThreatens: Boolean,
  validateDeltasOnActionCalculation: Boolean,
  extraValidationOnActionApply: Boolean,
  dontCalculateHistory: Boolean) extends Rules

