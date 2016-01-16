package ostinato.chess.ai

import ostinato.chess.core._
import ostinato.core.Ai

case class ChessBasicAi(player: ChessPlayer, debug: Boolean = false, seed: Option[Long] = None)
    extends Ai[ChessBoard, ChessAction, ChessPiece, ChessPlayer, ChessRules, ChessGame](player, seed) {

  override def nextAction(game: ChessGame)(implicit rules: ChessRules = ChessRules.default): Option[ChessAction] = {
    val actions = game.board.actionStream.toSeq
    val options = actions map (action ⇒ (action, alphabeta(game.board.doAction(action).get, action)))

    Some(options.maxBy(_._2)._1)
  }

  def alphabeta(board: ChessBoard, action: ChessAction, depth: Int = 2, alpha: Long = -Long.MaxValue, beta: Long = Long.MaxValue, currentPlayer: ChessPlayer = player): Long = {
    var a = alpha
    var b = beta
    if (depth == 0)
      evaluate(board, action)
    else if (player == currentPlayer) {
      var v = -Long.MaxValue
      board.actionStream.takeWhile { action ⇒
        v = math.max(v, alphabeta(board.doAction(action).get, action, depth - 1, a, b, currentPlayer.enemy))
        a = math.max(a, v)
        b > a
      }
      v
    } else {
      var v = Long.MaxValue
      board.actionStream.takeWhile { action ⇒
        v = math.min(v, alphabeta(board.doAction(action).get, action, depth - 1, a, b, currentPlayer.enemy))
        b = math.min(b, v)
        b > a
      }
      v
    }
  }

  private def sign(owner: ChessPlayer) = if (owner == player) 1 else -1

  private def materialValue(piece: ChessPiece) = piece match {
    case p: ♛ ⇒ 9
    case p: ♚ ⇒ 100
    case p: ♝ ⇒ 3
    case p: ♞ ⇒ 3
    case p: ♜ ⇒ 5
    case p: ♟ ⇒ 1
  }

  private def isInMiddle(piece: ChessPiece): Boolean =
    piece.pos.x >= 2 && piece.pos.x <= 5 && piece.pos.y >= 3 && piece.pos.y <= 4

  private def int(b: Boolean) = if (b) 1 else 0

  private def evaluate(board: ChessBoard, action: ChessAction): Long = {
    action match {

      // PRIORITY #1: GAME END
      case a: LoseAction ⇒ Long.MaxValue * sign(board.turn.enemy)
      case _ ⇒

        // PRIORITY #2: MATERIAL VALUE
        val undefendedOwnPieces = player.pieces(board).filter(!_.isDefended(board))
        val undefendedThreatenedOwnPieces = undefendedOwnPieces.filter(_.isThreatened(board))
        val likelyLostValue = undefendedThreatenedOwnPieces.toList.sortBy(materialValue).lastOption.map(materialValue).getOrElse(0)

        val playerMaterialValue = player.pieces(board).map(materialValue).sum - likelyLostValue
        val enemyMaterialValue = player.enemy.pieces(board).map(materialValue).sum
        val totalMaterialValue = playerMaterialValue - enemyMaterialValue

        // PRIORITY #3: MAKES CHECK
        val makesCheck = action.isCheck

        // PRIORITY #4: UNDEFENDED THREATENED OWN PIECE VALUE
        val undefendedThreatenedOwnPieceValue = undefendedThreatenedOwnPieces.map(materialValue).sum

        // PRIORITY #5: UNDEFENDED OWN PIECE VALUE
        val undefendedOwnPieceValue = undefendedOwnPieces.map(materialValue).sum

        // PRIORITY #6: UNDEFENDED THREATENED ENEMY PIECE VALUE
        val undefendedEnemyPieces = player.enemy.pieces(board).filter(!_.isDefended(board))
        val undefendedThreatenedEnemyPieceValue = undefendedEnemyPieces.filter(_.isThreatened(board)).map(materialValue).sum

        // PRIORITY #7: UNDEFENDED ENEMY PIECE VALUE
        val undefendedEnemyPieceValue = undefendedEnemyPieces.map(materialValue).sum

        // REST
        val gameStage =
          if (board.fullMoveNumber <= 5)
            "early"
          else if (board.pieces.size <= 10)
            "late"
          else
            "middle"

        val totalDistanceToPromotion = player.pawns(board).map(_.distanceToPromotion).sum
        val totalDistanceToPromotionMultiplier = if (gameStage == "early") 1 else if (gameStage == "middle") 10 else 100
        val hasMiddle = player.pieces(board).count(isInMiddle) > player.enemy.pieces(board).count(isInMiddle)
        val enemyHasMiddle = player.enemy.pieces(board).count(isInMiddle) > player.pieces(board).count(isInMiddle)

        val couldCastleButMovesKing = {
          val canCastle =
            board.castlingAvailable((player, CastlingSide.Kingside)) || board.castlingAvailable((player, CastlingSide.Queenside))

          val movesKing = action match {
            case MoveAction(k: ♚, _, _, _) ⇒ true
            case _                         ⇒ false
          }

          canCastle && movesKing
        }

        val castles = action match {
          case _: CastlingAction ⇒ true
          case _                 ⇒ false
        }

        int(castles) * 10 +
          int(couldCastleButMovesKing) * -1000 +
          int(hasMiddle) * 1000 +
          int(enemyHasMiddle) * -1000 +
          totalDistanceToPromotion * totalDistanceToPromotionMultiplier +
          undefendedEnemyPieceValue * 10000 +
          undefendedThreatenedEnemyPieceValue * 100000 +
          undefendedOwnPieceValue * 1000000 +
          undefendedThreatenedOwnPieceValue * 10000000 +
          int(makesCheck) * 1000000000 +
          totalMaterialValue * 10000000000L
    }
  }
}

