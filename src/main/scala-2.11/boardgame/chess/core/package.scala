package boardgame.chess

import boardgame.core.{XY, BoardSize}

package object core {
  implicit val chessBoardSize = BoardSize(8, 8)

  implicit class ChessXY(pos: XY) {
    lazy val chars = "abcdefgh"

    def toAn(implicit rules: ChessRules, chessBoardSize: BoardSize) =
      if (rules.whitePawnDirection == 1)
        An(chars(chessBoardSize.x - 1 - pos.x), pos.y + 1)
      else
        An(chars(pos.x), chessBoardSize.y - pos.y)
  }

  case class An(x: Char, y: Int)
}
