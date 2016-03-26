package ostinato.chess.core

import ostinato.core._

import scala.util.{ Success, Failure, Try }
import scala.util.control.NoStackTrace

object ChessGame {
  def fromGridString(
    string: String,
    turn: ChessPlayer = WhiteChessPlayer,
    castlingAvailable: Map[(ChessPlayer, CastlingSide.Value), Boolean] = castlingFullyAvailable,
    fullMoveNumber: Int = 1,
    halfMoveClock: Int = 0): Try[ChessGame] = {

    val grid = ChessGrid.fromGridString(string)

    val enPassantPawns = ChessGrid.charVector(string) flatMap {
      case ('↑', i) ⇒ EnPassantPawn.fromXYD(XY.fromI(i), XY(0, -1), grid)
      case ('↓', i) ⇒ EnPassantPawn.fromXYD(XY.fromI(i), XY(0, 1), grid)
      case _        ⇒ None
    }

    if (grid.size != 64) {
      Failure(InvalidChessGridSizeException)
    } else if (enPassantPawns.size > 1) {
      Failure(MoreThanOneEnPassantPawnException)
    } else {
      Success(
        ChessGame(
          ChessBoard(grid, turn, enPassantPawns.headOption, castlingAvailable, fullMoveNumber, halfMoveClock),
          ChessOptimisations.default
        )
      )
    }
  }

  def fromFen(fenString: String): Try[ChessGame] =
    if (Fen.isValidFen(fenString)) {
      val s = fenString.split(" +")

      val (gridS, turnS, castlingAvailableS, enPassantS, halfMoveCountS, fullMoveNumberS) = (s(0), s(1), s(2), s(3), s(4), s(5))
      val castlingAvailable = Fen.calculateCastlingAvailable(castlingAvailableS)
      val turn = Fen.calculateTurn(turnS)

      val grid = ChessGrid.fromGridString(gridS.map(Fen.shortFenTransformation(_)).mkString)
      val halfMoveCount = Fen.calculateNumber(halfMoveCountS)
      val fullMoveNumber = Fen.calculateNumber(fullMoveNumberS)
      val enPassantPawn = turn flatMap (t ⇒ Fen.calculateEnPassantPawn(enPassantS, t))

      (grid, turn, enPassantPawn, halfMoveCount, fullMoveNumber) match {
        case (_, None, _, _, _) ⇒
          Failure(InvalidTurnException)
        case (_grid, _, _, _, _) if _grid.size != 64 ⇒
          Failure(InvalidChessGridSizeException)
        case (_, _, _, Failure(_), _) ⇒
          Failure(InvalidHalfMoveCountException)
        case (_, _, _, _, Failure(_)) ⇒
          Failure(InvalidFullMoveNumberException)
        case (_grid, Some(_turn), _epp, Success(_halfMoveCount), Success(_fullMoveNumber)) ⇒
          Success(ChessGame(
            ChessBoard(_grid, _turn, _epp, castlingAvailable, _fullMoveNumber, _halfMoveCount),
            ChessOptimisations.default
          ))
      }
    } else {
      Failure(FenStringRegexMismatchException)
    }

  def fromShortFen(shortFenString: String): Try[ChessGame] =
    if (Fen.isValidShortFen(shortFenString)) {
      val grid = ChessGrid.fromGridString(shortFenString.map(Fen.shortFenTransformation(_)).mkString)

      if (grid.size != 64)
        Failure(InvalidChessGridSizeException)
      else
        Success(ChessGame(ChessBoard(grid), ChessOptimisations.default))

    } else {
      Failure(FenStringRegexMismatchException)
    }

  def fromOstinatoString(string: String): Try[ChessGame] = {
    OstinatoString.splitFenIccf(string) map {
      case (fenString: String, iccfString: String) ⇒
        fromFen(fenString) flatMap { game ⇒
          OstinatoString.calculateHistory(iccfString) map { _history =>
            game.copy(board = game.board.copy(history = _history))
          }
        }
    } getOrElse Failure(OstinatoStringRegexMismatchException)
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
      |""".stripMargin).get
}

case class ChessGame(override val board: ChessBoard, override val rules: ChessOptimisations) extends Game[ChessBoard, ChessAction, ChessPiece, ChessPlayer, ChessOptimisations](
  board, chessPlayers, rules) {

  val whitePlayer = WhiteChessPlayer
  val blackPlayer = BlackChessPlayer

  def isGameOver(implicit opts: ChessOptimisations = ChessOptimisations.default): Boolean = isDraw || lossFor.nonEmpty
  def lossFor(implicit opts: ChessOptimisations = ChessOptimisations.default): Option[ChessPlayer] = players find (board.isLossFor(_) == true)
  def isDraw(implicit opts: ChessOptimisations = ChessOptimisations.default): Boolean = board.isDraw

  def toShortFen = board.toShortFen
  def toFen = board.toFen
  def toOstinatoString = board.toOstinatoString

  def rotate: ChessGame = copy(board.rotate)
}

case object InvalidTurnException extends RuntimeException with NoStackTrace
case object InvalidChessGridSizeException extends RuntimeException with NoStackTrace
case object InvalidFullMoveNumberException extends RuntimeException with NoStackTrace
case object InvalidHalfMoveCountException extends RuntimeException with NoStackTrace
case object MoreThanOneEnPassantPawnException extends RuntimeException with NoStackTrace
case object FenStringRegexMismatchException extends RuntimeException with NoStackTrace
case object OstinatoStringRegexMismatchException extends RuntimeException with NoStackTrace
