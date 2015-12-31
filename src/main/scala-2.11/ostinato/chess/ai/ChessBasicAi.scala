package ostinato.chess.ai

import ostinato.chess.core._
import ostinato.core.Ai

case class ChessBasicAi(player: ChessPlayer, seed: Option[Long] = None)
    extends Ai[ChessBoard, ChessAction, ChessPiece, ChessPlayer, ChessRules, ChessGame](player, seed) {

  override def nextAction(game: ChessGame)(implicit rules: ChessRules = ChessRules.default): Option[ChessAction] = {
    val actionTree: List[(List[ChessAction], Int)] = tree(game.board, List(), 2)
    val actionTreeWithoutLeaves: List[(List[ChessAction], Int)] = actionTree.map(t => (t._1.init, t._2))
    val valuesOfEachPath: Map[List[ChessAction], List[(List[ChessAction], Int)]] = actionTreeWithoutLeaves.groupBy(_._1)
    val minimumFloorOfEachPath: Map[List[ChessAction], Int] = valuesOfEachPath.map(kv => (kv._1, kv._2.map(_._2).min))
    val actionWithMaximumMinimumFloor: (List[ChessAction], Int) = minimumFloorOfEachPath.maxBy(_._2)

    actionWithMaximumMinimumFloor._1.headOption
  }

  def tree(board: ChessBoard, actions: List[ChessAction], depth: Int): List[(List[ChessAction], Int)] =
    if (depth == 0) {
      if (actions.isEmpty)
        List((List(), evaluate(board)))
      else
        actions.last match {
          case a: DrawAction => List((actions, (Int.MaxValue / 2) * sign(a.player.enemy)))
          case a: LoseAction => List((actions, Int.MaxValue * sign(a.player.enemy)))
          case _ => List((actions, evaluate(board)))
        }
    } else {
      board.actions.map(a => (a, board.doAction(a))).flatMap {
        case (a, Some(newBoard)) => tree(newBoard, actions :+ a, depth - 1)
        case _ => tree(board, actions, depth - 1)
      }.toList
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

  private def materialValue(board: ChessBoard): Int = board.pieces.map(materialValue).sum

  def evaluate(board: ChessBoard): Int = materialValue(board)
}

