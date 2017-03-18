package ostinato.chess.core

import ostinato.core.Rules

object ChessOptimisations {
  def default = ChessOptimisations(
    kingIsTakeable = false,
    checkForThreatens = true,
    validateDeltasOnActionCalculation = true,
    extraValidationOnActionApply = false,
    dontCalculateHistory = false,
    optimistic = false
  )

  def noCheckForThreatens = default.copy(checkForThreatens = false)

  def beOptimistic = default.copy(optimistic = true)
}

case class ChessOptimisations(kingIsTakeable: Boolean,
                              checkForThreatens: Boolean,
                              validateDeltasOnActionCalculation: Boolean,
                              extraValidationOnActionApply: Boolean,
                              dontCalculateHistory: Boolean,
                              optimistic: Boolean)
    extends Rules
