package ostinato.chess.core

import ostinato.core.XY

import scala.util.Try
import scala.util.control.NoStackTrace

object Fen {
  def +(f: Fen, char: Char) = {
    val isNewLine = (f.cellCount + 1) % 8 == 0
    val newLine = if (isNewLine && f.cellCount < 63) "/" else ""

    char match {
      case ' ' ⇒
        Fen(
          f.partialString + (if (isNewLine) (f.emptyCells + 1).toString else "") + newLine,
          f.cellCount + 1,
          if (isNewLine) 0 else f.emptyCells + 1)
      case c ⇒
        Fen(
          f.partialString + (if (f.emptyCells != 0) f.emptyCells.toString else "") + c + newLine,
          f.cellCount + 1,
          0
        )
    }
  }

  def isValidFen(s: String) =
    s.trim.matches("""\s*[rnbqkpRNBQKP\/\d]+\s+[wb]\s+([KQkq]{1,4}|\-)\s*[\-abcdefgh12345678]{1,2}\s*[\d]{1,2}\s*[\d]{1,2}\s*""")

  def isValidShortFen(s: String) =
    s.trim.matches("""\s*[rnbqpkRNBQKP\/\d]+\s*""")

  lazy val shortFenTransformation: Map[Char, String] = Map(
    'r' -> "♜",
    'n' -> "♞",
    'b' -> "♝",
    'q' -> "♛",
    'k' -> "♚",
    'p' -> "♟",
    'R' -> "♖",
    'N' -> "♘",
    'B' -> "♗",
    'Q' -> "♕",
    'K' -> "♔",
    'P' -> "♙",
    '1' -> ".",
    '2' -> "..",
    '3' -> "...",
    '4' -> "....",
    '5' -> ".....",
    '6' -> "......",
    '7' -> ".......",
    '8' -> "........",
    '/' -> "\n"
  )

  def calculateTurn(s: String): Option[ChessPlayer] = s.trim match {
    case "w" ⇒ Some(WhiteChessPlayer)
    case "b" ⇒ Some(BlackChessPlayer)
    case _   ⇒ None
  }

  def calculateCastlingAvailable(s: String): Map[(ChessPlayer, CastlingSide.Value), Boolean] = Map(
    (WhiteChessPlayer, CastlingSide.Kingside) -> s.contains('K'),
    (WhiteChessPlayer, CastlingSide.Queenside) -> s.contains('Q'),
    (BlackChessPlayer, CastlingSide.Kingside) -> s.contains('k'),
    (BlackChessPlayer, CastlingSide.Queenside) -> s.contains('q')
  )

  def calculateEnPassantPawn(s: String, turn: ChessPlayer)(
    implicit rules: ChessOptimisations = ChessOptimisations.default): Option[EnPassantPawn] = {
    val (upperRow, lowerRow) = (2, 5)

    (s.trim, ChessXY.fromAn(s)) match {
      case ("-", _)  ⇒ None
      case (_, None) ⇒ None
      case (_, Some(xy: XY)) ⇒ xy.y match {
        case `upperRow` ⇒ Some(EnPassantPawn(xy, ♟(XY(xy.x, upperRow + 1), turn.enemy, 1)))
        case `lowerRow` ⇒ Some(EnPassantPawn(xy, ♟(XY(xy.x, lowerRow - 1), turn.enemy, -1)))
      }
    }
  }

  def calculateNumber(s: String) = Try(s.trim.toInt)
}

case class Fen(partialString: String = "", cellCount: Int = 0, emptyCells: Int = 0) {
  override def toString = partialString
}
