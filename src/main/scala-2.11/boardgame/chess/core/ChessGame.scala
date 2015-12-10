package boardgame.chess.core

import boardgame.core._

object ChessGame {
  def fromString(
    string: String,
    turn: ChessPlayer = WhiteChessPlayer,
    hasCastled: Map[ChessPlayer, Boolean] = Map(WhiteChessPlayer -> false, BlackChessPlayer -> false),
    fullMoveNumber: Int = 1,
    halfMoveClock: Int = 0)(
      implicit rules: ChessRules = ChessRules.default): ChessGame = {

    val (white, black) = (WhiteChessPlayer, BlackChessPlayer)
    val charVector = string.split('\n').mkString.zipWithIndex.toVector
    val grid = charVector map {
      case ('♜', i) ⇒ Some(♜(XY.fromI(i), black))
      case ('♞', i) ⇒ Some(♞(XY.fromI(i), black))
      case ('♝', i) ⇒ Some(♝(XY.fromI(i), black))
      case ('♛', i) ⇒ Some(♛(XY.fromI(i), black))
      case ('♚', i) ⇒ Some(♚(XY.fromI(i), black))
      case ('♟', i) ⇒ Some(♟(XY.fromI(i), black, rules.whitePawnDirection * -1))
      case ('♖', i) ⇒ Some(♜(XY.fromI(i), white))
      case ('♘', i) ⇒ Some(♞(XY.fromI(i), white))
      case ('♗', i) ⇒ Some(♝(XY.fromI(i), white))
      case ('♕', i) ⇒ Some(♛(XY.fromI(i), white))
      case ('♔', i) ⇒ Some(♚(XY.fromI(i), white))
      case ('♙', i) ⇒ Some(♟(XY.fromI(i), white, rules.whitePawnDirection))
      case _        ⇒ None
    }

    val enPassantPawns = charVector flatMap {
      case ('↑', i) ⇒ EnPassantPawn.fromXYD(XY.fromI(i), XY(0, -1), grid)
      case ('↓', i) ⇒ EnPassantPawn.fromXYD(XY.fromI(i), XY(0, 1), grid)
      case _        ⇒ None
    }

    // TODO: headOption means keep only the first; this is incorrect: if there's 2 there's a problem!
    new ChessGame(new ChessBoard(grid, turn, enPassantPawns.headOption, hasCastled, fullMoveNumber, halfMoveClock), rules)
  }

  val defaultGame: ChessGame = fromString(
    """♜♞♝♛♚♝♞♜
      |♟♟♟♟♟♟♟♟
      |........
      |........
      |........
      |........
      |♙♙♙♙♙♙♙♙
      |♖♘♗♕♔♗♘♖
      |""".stripMargin)
}

class ChessGame(override val board: ChessBoard, override val rules: ChessRules) extends Game[ChessBoard, ChessPlayer](
  board, List(WhiteChessPlayer, BlackChessPlayer), rules) {

  val whitePlayer = WhiteChessPlayer
  val blackPlayer = BlackChessPlayer

  def isGameOver(implicit rules: ChessRules = ChessRules.default): Boolean = isDraw || lossFor.nonEmpty
  def lossFor(implicit rules: ChessRules = ChessRules.default): Option[ChessPlayer] = players find (board.isLossFor(_) == true)
  def isDraw(implicit rules: ChessRules = ChessRules.default): Boolean = players exists board.isDrawFor
}
