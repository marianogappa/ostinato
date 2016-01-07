package ostinato.chess.core

import ostinato.core._

object ChessGame {
  def fromGridString(
    string: String,
    turn: ChessPlayer = WhiteChessPlayer,
    castlingAvailable: Map[(ChessPlayer, CastlingSide.Value), Boolean] = castlingFullyAvailable,
    fullMoveNumber: Int = 1,
    halfMoveClock: Int = 0)(
      implicit rules: ChessRules = ChessRules.default): ChessGame = {

    val grid = ChessGrid.fromGridString(string)

    val enPassantPawns = ChessGrid.charVector(string) flatMap {
      case ('↑', i) ⇒ EnPassantPawn.fromXYD(XY.fromI(i), XY(0, -1), grid)
      case ('↓', i) ⇒ EnPassantPawn.fromXYD(XY.fromI(i), XY(0, 1), grid)
      case _        ⇒ None
    }

    // TODO: headOption means keep only the first; this is incorrect: if there's 2 there's a problem!
    ChessGame(ChessBoard(grid, turn, enPassantPawns.headOption, castlingAvailable, fullMoveNumber, halfMoveClock), rules)
  }

  def fromFen(fenString: String)(implicit rules: ChessRules = ChessRules.default): Option[ChessGame] =
    if (Fen.isValidFen(fenString)) {
      val s = fenString.split(" +")

      val (gridS, turnS, castlingAvailableS, enPassantS, halfMoveCountS, fullMoveNumberS) = (s(0), s(1), s(2), s(3), s(4), s(5))
      val castlingAvailable = Fen.calculateCastlingAvailable(castlingAvailableS)
      val turn = Fen.calculateTurn(turnS)

      turn flatMap { turn ⇒
        (
          ChessGrid.fromGridString(gridS.map(Fen.shortFenTransformation(_)).mkString),
          Fen.calculateEnPassantPawn(enPassantS, turn),
          Fen.calculateNumber(halfMoveCountS),
          Fen.calculateNumber(fullMoveNumberS)
        ) match {
            case (
              grid: Vector[Option[ChessPiece]],
              epp: Option[EnPassantPawn],
              Some(halfMoveCount: Int),
              Some(fullMoveNumber: Int)
              ) ⇒

              Some(ChessGame(ChessBoard(grid, turn, epp, castlingAvailable, fullMoveNumber, halfMoveCount), rules))
            case _ ⇒
              None
          }
      }
    } else {
      None
    }

  def fromShortFen(shortFenString: String)(implicit rules: ChessRules = ChessRules.default): Option[ChessGame] =
    if (Fen.isValidShortFen(shortFenString)) {
      Some(
        ChessGame(ChessBoard(ChessGrid.fromGridString(shortFenString.map(Fen.shortFenTransformation(_)).mkString)), rules)
      )
    } else {
      None
    }

  val defaultGame: ChessGame = fromGridString(
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

case class ChessGame(override val board: ChessBoard, override val rules: ChessRules) extends Game[ChessBoard, ChessAction, ChessPiece, ChessPlayer, ChessRules](
  board, chessPlayers, rules) {

  val whitePlayer = WhiteChessPlayer
  val blackPlayer = BlackChessPlayer

  def isGameOver(implicit rules: ChessRules = ChessRules.default): Boolean = isDraw || lossFor.nonEmpty
  def lossFor(implicit rules: ChessRules = ChessRules.default): Option[ChessPlayer] = players find (board.isLossFor(_) == true)
  def isDraw(implicit rules: ChessRules = ChessRules.default): Boolean = board.isDraw

  def toShortFen = board.toShortFen
  def toFen = board.toFen
}
