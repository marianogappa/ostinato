package ostinato.chess.ai

import ostinato.chess.core._
import ostinato.core.Ai

case class ChessBasicAi(player: ChessPlayer, depth: Int = 1, debug: Boolean = false)
    extends Ai[ChessBoard, ChessAction, ChessPiece, ChessPlayer, ChessOptimisations, ChessGame](player) {

  override def nextAction(game: ChessGame)(implicit opts: ChessOptimisations = ChessOptimisations.default): Option[ChessAction] = {
    val optsForAi = opts.copy(extraValidationOnActionApply = false, dontCalculateHistory = true)
    val actions = game.board.actionStream.force
    val options = actions map (action ⇒ (action, alphabeta(game.board.doAction(action)(optsForAi).get, action)(optsForAi)))

    if (debug) { options foreach println }

    val chosenAction = options.sortWith(sort).head._1

    if (debug) { println(s"Chosen action: $chosenAction") }

    Some(chosenAction)
  }

  def sort(a: (ChessAction, Long), b: (ChessAction, Long)) = (a, b) match {
    case _ if a._1.isCheckmate => true
    case _ if b._1.isCheckmate => false
    case _ if a._2 > b._2 => true
    case _ if a._2 < b._2 => false
    case ((ac: CapturePromoteAction, _), (_, _)) => true
    case ((_, _), (bc: CapturePromoteAction, _)) => false
    case ((ac: PromoteAction, _), (_, _)) => true
    case ((_, _), (bc: PromoteAction, _)) => false
    case ((ac: CaptureAction, _), (_, _)) => true
    case ((_, _), (bc: CaptureAction, _)) => false
    case ((ac: EnPassantCaptureAction, _), (_, _)) => true
    case ((_, _), (bc: EnPassantCaptureAction, _)) => false
    case ((ac: CastlingAction, _), (_, _)) => true
    case ((_, _), (bc: CastlingAction, _)) => false
    case _ => a._2 > b._2
  }

  def alphabeta(board: ChessBoard, action: ChessAction, depth: Int = depth, alpha: Long = -Long.MaxValue, beta: Long = Long.MaxValue)(implicit opts: ChessOptimisations): Long = {
    var a = alpha
    var b = beta

    if (depth == 0) {
      val value = evaluate(board, action)
      value
    } else {
      if (player != action.turn) {
        var v = -Long.MaxValue
          board.actionStream.foreach { newAction ⇒
            v = math.max(v, alphabeta(board.doAction(newAction).get, newAction, depth - 1, a, b))
            a = math.max(a, v)
            if (b <= a) return v
          }
          v
      } else {
        var v = Long.MaxValue
          board.actionStream.foreach { newAction ⇒
            v = math.min(v, alphabeta(board.doAction(newAction).get, newAction, depth - 1, a, b))
            b = math.min(b, v)
            if (b <= a) return v
          }
          v
      }
    }
  }

  private def sign(owner: ChessPlayer) = if (owner == player) 1 else -1

  private def materialValue(piece: ChessPiece) = piece match {
    case p: ♛ ⇒ 9
    case p: ♚ ⇒ 0
    case p: ♝ ⇒ 3
    case p: ♞ ⇒ 3
    case p: ♜ ⇒ 5
    case p: ♟ ⇒ 1
  }

  private def isInMiddle(piece: ChessPiece): Boolean =
    piece.pos.x >= 2 && piece.pos.x <= 5 && piece.pos.y >= 3 && piece.pos.y <= 4

  private def int(b: Boolean) = if (b) 1 else 0

  private def evaluate(board: ChessBoard, action: ChessAction)(implicit opts: ChessOptimisations = ChessOptimisations.default): Long = {
    action match {

      // PRIORITY #1: GAME END
      case a: LoseAction ⇒ Long.MaxValue * sign(action.turn.enemy)
      case _ ⇒
        if (action.isCheckmate) {
          Long.MaxValue * sign(action.turn)
        }
        else {

          // PRIORITY #2: MATERIAL VALUE
          val undefendedOwnPieces = action.turn.pieces(board).filter(!_.isDefended(board))
          val undefendedThreatenedOwnPieces = undefendedOwnPieces.filter(_.isThreatened(board))

          val enemyMaterialValue = action.turn.enemy.pieces(board).toList.map(materialValue).sum
          val playerMaterialValueWithoutLikelyLostValue = action.turn.pieces(board).toList.map(materialValue).sum
          val enemyHasMaterialValueAdvantage = enemyMaterialValue > playerMaterialValueWithoutLikelyLostValue

          val likelyLostValue = undefendedThreatenedOwnPieces.toList.sortBy(materialValue).lastOption.map(materialValue).getOrElse {
            if (enemyHasMaterialValueAdvantage) {
              val threatenedOwnPieces = action.turn.pieces(board).flatMap { ownPiece =>
                ownPiece.threatenedBy(board) match {
                  case Some(enemyPiece) if materialValue(ownPiece) >= materialValue(enemyPiece) => Set(materialValue(ownPiece))
                  case _ => Set.empty[Int]
                }
              } // TODO optimise!

              if (threatenedOwnPieces.isEmpty) {
                0
              } else {
                threatenedOwnPieces.max
              }
            } else {
              0
            }
          }

          val playerMaterialValue = playerMaterialValueWithoutLikelyLostValue - likelyLostValue
          val totalMaterialValue = playerMaterialValue * sign(action.turn) + enemyMaterialValue * sign(action.turn.enemy)

          // PRIORITY #3: MAKES CHECK
          val makesCheck = action.isCheck

          // PRIORITY #4: UNDEFENDED THREATENED OWN PIECE VALUE
          val undefendedThreatenedOwnPieceValue = undefendedThreatenedOwnPieces.toList.map(materialValue).sum

          // PRIORITY #5: UNDEFENDED OWN PIECE VALUE
          val undefendedOwnPieceValue = undefendedOwnPieces.toList.map(materialValue).sum

          // PRIORITY #6: UNDEFENDED THREATENED ENEMY PIECE VALUE
          val undefendedEnemyPieces = action.turn.enemy.pieces(board).filter(!_.isDefended(board))
          val undefendedThreatenedEnemyPieceValue = undefendedEnemyPieces.filter(_.isThreatened(board)).toList.map(materialValue).sum

          // PRIORITY #7: UNDEFENDED ENEMY PIECE VALUE
          val undefendedEnemyPieceValue = undefendedEnemyPieces.toList.map(materialValue).sum

          // REST
          val gameStage =
            if (board.fullMoveNumber <= 5)
              "early"
            else if (board.pieces.size <= 10)
              "late"
            else
              "middle"

          val totalDistanceToPromotion = action.turn.pawns(board).map(_.distanceToPromotion).sum
          val totalDistanceToPromotionMultiplier = if (gameStage == "early") 1 else if (gameStage == "middle") 10 else 100
          val hasMiddle = action.turn.pieces(board).count(isInMiddle) > action.turn.enemy.pieces(board).count(isInMiddle)
          val hasMiddleMultiplier = if (gameStage == "late") 0 else 1
          val enemyHasMiddle = action.turn.enemy.pieces(board).count(isInMiddle) > action.turn.pieces(board).count(isInMiddle)

          val couldCastleButMovesKing = {
            val canCastle =
              board.castlingAvailable((action.turn, CastlingSide.Kingside)) || board.castlingAvailable((action.turn, CastlingSide.Queenside))

            val movesKing = action match {
              case MoveAction(k: ♚, _, _, _) ⇒ true
              case _ ⇒ false
            }

            canCastle && movesKing
          }

          val castles = action match {
            case _: CastlingAction ⇒ true
            case _ ⇒ false
          }

          val idleBishopKnightPenalty =
            if (gameStage == "early")
              (action.turn.knights(board) ++ action.turn.bishops(board)).count(_.pos.y == action.turn.initialY)
            else
              0

          val nonIdleRookPenalty =
            if (gameStage == "early")
              action.turn.rooks(board).count(r => r.pos.y != action.turn.initialY || (r.pos.x != 0 && r.pos.x != 7))
            else
              0

          val value: Long =
            int(castles) * 100 * sign(action.turn) +
            int(couldCastleButMovesKing) * -100 * sign(action.turn) +
            idleBishopKnightPenalty * -100 * sign(action.turn) +
            nonIdleRookPenalty * -100 * sign(action.turn) +
            int(hasMiddle) * 100 * hasMiddleMultiplier * sign(action.turn) +
            int(enemyHasMiddle) * -100 * hasMiddleMultiplier * sign(action.turn) +
            totalDistanceToPromotion * totalDistanceToPromotionMultiplier * sign(action.turn) +
            undefendedEnemyPieceValue * 1000 * sign(action.turn) +
            undefendedThreatenedEnemyPieceValue * 10000 * sign(action.turn) +
            undefendedOwnPieceValue * -100000 * sign(action.turn) +
            undefendedThreatenedOwnPieceValue * -1000000 * sign(action.turn) +
            int(makesCheck) * 100000000 * sign(action.turn) +
            totalMaterialValue * 1000000000L

          value
        }
    }
  }
}

