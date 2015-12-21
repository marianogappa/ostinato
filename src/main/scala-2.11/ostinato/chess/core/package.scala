package ostinato.chess

import ostinato.core.{ XY, BoardSize }

package object core {
  implicit val chessBoardSize = BoardSize(8, 8)

  object CastlingSide extends Enumeration {
    type CastlingSide = Value
    val Queenside, Kingside = Value
  }

  lazy val chessPlayers: List[ChessPlayer] = List(WhiteChessPlayer, BlackChessPlayer)
  lazy val castlingSides = List(CastlingSide.Queenside, CastlingSide.Kingside)

  lazy val castlingFullyAvailable: Map[(ChessPlayer, CastlingSide.Value), Boolean] = (for {
    chessPlayer ← chessPlayers
    castlingSide ← castlingSides
  } yield (chessPlayer, castlingSide) -> true).toMap

  lazy val castlingFullyUnavailable = castlingFullyAvailable map (kv => (kv._1, false))

  object ChessXY {
    lazy val chars = "abcdefgh"
    def fromAn(string: String)(implicit rules: ChessRules = ChessRules.default) = {
        val s = string.filter(_ > ' ').toLowerCase
      if (s.length == 2 && s.matches("""[a-h][1-8]"""))
        if (rules.whitePawnDirection == 1)
          Some(XY(chars.indexOf(s(0)), s(1).asDigit - 1))
        else
          Some(XY(chars.indexOf(s(0)), chessBoardSize.y - 1 - (s(1).asDigit - 1)))
      else
        None
    }
  }

  object ChessGrid {
    def fromGridString(s: String)(implicit rules: ChessRules = ChessRules.default): Vector[Option[ChessPiece]] = {
      charVector(s) map {
        case ('♜', i) ⇒ Some(♜(XY.fromI(i), BlackChessPlayer))
        case ('♞', i) ⇒ Some(♞(XY.fromI(i), BlackChessPlayer))
        case ('♝', i) ⇒ Some(♝(XY.fromI(i), BlackChessPlayer))
        case ('♛', i) ⇒ Some(♛(XY.fromI(i), BlackChessPlayer))
        case ('♚', i) ⇒ Some(♚(XY.fromI(i), BlackChessPlayer))
        case ('♟', i) ⇒ Some(♟(XY.fromI(i), BlackChessPlayer, rules.whitePawnDirection * -1))
        case ('♖', i) ⇒ Some(♜(XY.fromI(i), WhiteChessPlayer))
        case ('♘', i) ⇒ Some(♞(XY.fromI(i), WhiteChessPlayer))
        case ('♗', i) ⇒ Some(♝(XY.fromI(i), WhiteChessPlayer))
        case ('♕', i) ⇒ Some(♛(XY.fromI(i), WhiteChessPlayer))
        case ('♔', i) ⇒ Some(♚(XY.fromI(i), WhiteChessPlayer))
        case ('♙', i) ⇒ Some(♟(XY.fromI(i), WhiteChessPlayer, rules.whitePawnDirection))
        case _        ⇒ None
      }
    }
    def charVector(s: String) = s.split('\n').mkString.zipWithIndex.toVector
  }

  implicit class ChessXY(pos: XY) {

    def toAn(implicit rules: ChessRules = ChessRules.default, chessBoardSize: BoardSize) =
      if (rules.whitePawnDirection == 1)
        An(ChessXY.chars(chessBoardSize.x - 1 - pos.x), pos.y + 1)
      else
        An(ChessXY.chars(pos.x), chessBoardSize.y - pos.y)
  }

  case class An(x: Char, y: Int) {
    override def toString = s"$x$y"
  }

  object Fan {
    def checkmate(winner: ChessPlayer) = if (winner == WhiteChessPlayer) "1-0" else "0-1"
    def check(implicit rules: ChessRules = ChessRules.default) = "+"
    def kingSideCastle(implicit rules: ChessRules = ChessRules.default) = "0-0"
    def queenSideCastle(implicit rules: ChessRules = ChessRules.default) = "0-0-0"
    def draw(implicit rules: ChessRules = ChessRules.default) = "½–½"
  }
}
