package boardgame.chess

import boardgame.core.{XY, BoardSize}

package object core {
  implicit val chessBoardSize = BoardSize(8, 8)

  implicit class ChessXY(pos: XY) {
    lazy val chars = "abcdefgh"

    def toAn(implicit rules: ChessRules = ChessRules.default, chessBoardSize: BoardSize) =
      if (rules.whitePawnDirection == 1)
        An(chars(chessBoardSize.x - 1 - pos.x), pos.y + 1)
      else
        An(chars(pos.x), chessBoardSize.y - pos.y)
  }

  case class An(x: Char, y: Int) {
    override def toString = s"$x$y"
  }

  object Fan {
    def checkmate(winner: ChessPlayer) = if (winner == WhiteChessPlayer) "1-0" else "0-1"
    def check(implicit rules: ChessRules = ChessRules.default) = "+"
    def kingSideCastle(implicit rules: ChessRules = ChessRules.default) = "0-0"
    def queenSideCastle(implicit rules: ChessRules = ChessRules.default) = "0-0-0"
  }

  // TODO do complete Fen with active position, etc
  object Fen {
    def +(f: Fen, char: Char) = {
      val isNewLine = (f.cellCount + 1) % 8 == 0
      val newLine = if (isNewLine && f.cellCount < 63) "/" else ""

      char match {
        case ' ' =>
          Fen(
            f.partialString + (if (isNewLine) (f.emptyCells + 1).toString else "") + newLine,
            f.cellCount + 1,
            if (isNewLine) 0 else f.emptyCells + 1)
        case c =>
          Fen(
            f.partialString + (if (f.emptyCells != 0) f.emptyCells.toString else "") + c + newLine,
            f.cellCount + 1,
            0
          )
      }
    }
  }
  case class Fen(partialString: String = "", cellCount: Int = 0, emptyCells: Int = 0) {
    override def toString = partialString
  }
}
