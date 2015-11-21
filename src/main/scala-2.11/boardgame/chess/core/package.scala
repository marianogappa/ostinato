package boardgame.chess

import boardgame.core.BoardSize

package object core {
  implicit val chessBoardSize = BoardSize(8, 8)
}
