package ostinato.chess.ai

import ostinato.chess.core._
import ostinato.core.Ai

case class ChessBasicAi(player: ChessPlayer, debug: Boolean = false, seed: Option[Long] = None)
    extends Ai[ChessBoard, ChessAction, ChessPiece, ChessPlayer, ChessRules, ChessGame](player, seed) {

  override def nextAction(game: ChessGame)(implicit rules: ChessRules = ChessRules.default): Option[ChessAction] = {
    val actions = game.board.actionStream.toSeq
    val options = actions map (action => (action, {
      val value = alphabeta(game.board.doAction(action).get, action)
      if (debug) println(s"Evaluate $action => $value")
      value
    }))

    val chosen = options.maxBy(_._2)._1
    if (debug) println(s"Chose => $chosen")
    Some(chosen)
  }

  def alphabeta(board: ChessBoard, action: ChessAction, depth: Int = 4, alpha: Long = -Long.MaxValue, beta: Long = Long.MaxValue, currentPlayer: ChessPlayer = player): Long = {
    var a = alpha
    var b = beta

    if (depth == 0)
      evaluate(board, action)
    else if (player == currentPlayer) {
      var v = -Long.MaxValue
      board.actionStream.takeWhile { action =>
        v = math.max(v, alphabeta(board.doAction(action).get, action, depth - 1, a, b, currentPlayer.enemy))
        a = math.max(a, v)
        b > a
      }
      v
    } else {
      var v = Long.MaxValue
      board.actionStream.takeWhile { action =>
        v = math.min(v, alphabeta(board.doAction(action).get, action, depth - 1, a, b, currentPlayer.enemy))
        b = math.min(a, v)
        b > a
      }
      v
    }
  }

  private def sign(owner: ChessPlayer) = if (owner == player) 1 else -1

  private def materialValue(piece: ChessPiece) = piece match {
    case p: ♛ ⇒ 9 * sign(piece.owner)
    case p: ♚ ⇒ 100 * sign(piece.owner)
    case p: ♝ ⇒ 3 * sign(piece.owner)
    case p: ♞ ⇒ 3 * sign(piece.owner)
    case p: ♜ ⇒ 5 * sign(piece.owner)
    case p: ♟ ⇒ 1 * sign(piece.owner)
  }

  private def materialValue(board: ChessBoard): Long = board.pieces.map(materialValue).sum

  private def evaluate(board: ChessBoard, action: ChessAction): Long = {
    action match {
      case a: LoseAction => Long.MaxValue * sign(a.player.enemy)
      case _ =>
        val undefendedOwnPieces = player.pieces(board).filter(!_.isDefended(board))
        val undefendedEnemyPieces = player.enemy.pieces(board).filter(!_.isDefended(board))

        val undefendedOwnPieceValue = undefendedOwnPieces.map(materialValue).sum
        val undefendedThreatenedOwnPieceValue = undefendedOwnPieces.filter(_.isThreatened(board)).map(materialValue).sum
        val undefendedEnemyPieceValue = math.abs(undefendedEnemyPieces.map(materialValue).sum)
        val undefendedThreatenedEnemyPieceValue = math.abs(undefendedEnemyPieces.filter(_.isThreatened(board)).map(materialValue).sum)

        (if (action.isCheck) 1000 else 0) * sign(action.turn) +
          1000000 * materialValue(board) +
          10000 * undefendedThreatenedEnemyPieceValue +
          -10000 * undefendedThreatenedOwnPieceValue +
          1000 * undefendedEnemyPieceValue +
          -1000 * undefendedOwnPieceValue
    }
  }
}

